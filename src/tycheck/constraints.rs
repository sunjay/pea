use std::collections::HashSet;

use ena::unify::{UnifyKey, InPlaceUnificationTable};

use crate::{diagnostics::Diagnostics, source_files::Span};

use super::{subst::Subst, ty::{self, Ty, TySpan, UnifyError}, solver};

#[derive(Debug, Clone)]
pub enum UnifyErrorSpan {
    MismatchedTypes {
        ty1: TySpan,
        ty2: TySpan,
    },

    ArityMismatch {
        ty1: TySpan,
        ty1_arity: usize,
        ty2: TySpan,
        ty2_arity: usize,
    },
}

impl UnifyErrorSpan {
    fn new(err: UnifyError, ty1_span: Option<Span>, ty2_span: Option<Span>) -> Self {
        match err {
            UnifyError::MismatchedTypes { ty1, ty2 } => {
                UnifyErrorSpan::MismatchedTypes {
                    ty1: TySpan {
                        ty: ty1,
                        span: ty1_span,
                    },
                    ty2: TySpan {
                        ty: ty2,
                        span: ty2_span,
                    },
                }
            },
            UnifyError::ArityMismatch { ty1, ty1_arity, ty2, ty2_arity } => {
                UnifyErrorSpan::ArityMismatch {
                    ty1: TySpan {
                        ty: ty1,
                        span: ty1_span,
                    },
                    ty1_arity,
                    ty2: TySpan {
                        ty: ty2,
                        span: ty2_span,
                    },
                    ty2_arity,
                }
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar(u32);

impl UnifyKey for TyVar {
    /// None - type has not been determined yet
    /// Some(Ty) - the type that has been resolved so far for this variable
    type Value = Option<TySpan>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(id: u32) -> Self {
        TyVar(id)
    }

    fn tag() -> &'static str {
        "TyVar"
    }
}

#[derive(Debug, Default)]
pub struct ConstraintSet {
    /// The concrete types for each variable, collected along the way.
    ///
    /// A union-find implementation keeps track of which type variables are equal to each other
    /// and maintains that equivalence as values are updated.
    ty_var_table: InPlaceUnificationTable<TyVar>,
    /// The set of type variables which are allowed to default to `()` if ambiguous
    default_unit: HashSet<TyVar>,
}

impl ConstraintSet {
    /// Returns the type that has been inferred so far for the given variable
    pub fn current_type(&mut self, ty_var: TyVar) -> Option<TySpan> {
        self.ty_var_table.probe_value(ty_var)
    }

    /// Generates a fresh type variable and returns it
    pub fn fresh_type_var(&mut self) -> TyVar {
        self.ty_var_table.new_key(None)
    }

    /// Adds a constraint that asserts that the given type variable is the given type
    pub fn ty_var_is_ty(&mut self, ty_var: TyVar, ty: Ty, span: impl Into<Option<Span>>) -> Result<(), UnifyErrorSpan> {
        let span = span.into();

        let mut extra_constraints = Vec::new();

        // HACK: We are actually unifying twice here, once for real, and once to please the API of ena
        let other_ty = self.ty_var_table.probe_value(ty_var);
        let ty = match other_ty {
            Some(other_ty) => {
                let other_ty_span = other_ty.span;

                ty.unify(other_ty.ty, &mut extra_constraints).map_err(|err| {
                    UnifyErrorSpan::new(err, span, other_ty_span)
                })?
            },
            None => ty,
        };

        self.apply_extra_constraints(extra_constraints, span)?;

        let ty_span = TySpan {ty, span};
        self.ty_var_table.union_value(ty_var, Some(ty_span));

        Ok(())
    }

    /// Adds a constraint that asserts that the given type variables unify
    pub fn ty_vars_unify(&mut self, ty_var1: TyVar, ty_var2: TyVar) -> Result<(), UnifyErrorSpan> {
        // HACK: We are actually unifying twice here, once for real, and once to please the API of ena
        let ty1 = self.ty_var_table.probe_value(ty_var1);
        let ty2 = self.ty_var_table.probe_value(ty_var2);
        let ty = match (ty1, ty2) {
            (Some(ty1), Some(ty2)) => {
                let ty1_span = ty1.span;
                let ty2_span = ty2.span;

                let mut extra_constraints = Vec::new();

                let ty = ty1.ty.unify(ty2.ty, &mut extra_constraints).map_err(|err| {
                    UnifyErrorSpan::new(err, ty1_span, ty2_span)
                })?;
                // Could choose either span, just need to make sure we don't discard the span when
                // either is `None`
                let span = ty2_span.or(ty1_span);

                self.apply_extra_constraints(extra_constraints, span)?;

                Some(TySpan {ty, span})
            },

            (Some(ty1), None) => Some(ty1),

            (None, Some(ty2)) => Some(ty2),

            (None, None) => None,
        };

        self.ty_var_table.union_value(ty_var1, ty);
        self.ty_var_table.union(ty_var1, ty_var2);

        Ok(())
    }

    fn apply_extra_constraints(&mut self, constraints: Vec<ty::Constraint>, span: Option<Span>) -> Result<(), UnifyErrorSpan> {
        for constraint in constraints {
            match constraint {
                ty::Constraint::TyVarIsTy { ty_var, ty } => {
                    self.ty_var_is_ty(ty_var, ty, span)?;
                },

                ty::Constraint::TyVarsUnify { ty_var1, ty_var2 } => {
                    self.ty_vars_unify(ty_var1, ty_var2)?;
                },
            }
        }

        Ok(())
    }

    /// Allows the given type variable to default to `()` if ambiguous
    pub fn ty_var_default_unit(&mut self, ty_var: TyVar) {
        self.default_unit.insert(ty_var);
    }

    /// Solves this constraint set, and returns a substitution that can be used to map all type
    /// variables to concrete types
    pub fn solve(self, diag: &Diagnostics) -> Subst {
        let Self {mut ty_var_table, default_unit} = self;
        let ty_vars = (0..ty_var_table.len() as u32).map(TyVar);

        solver::apply_defaults(&mut ty_var_table, ty_vars.clone(), &default_unit);

        let errors_occurred = diag.emitted_errors() > 0;

        ty_vars.map(|ty_var| {
            let ty_span = match ty_var_table.probe_value(ty_var) {
                Some(ty_span) => ty_span,
                None => if errors_occurred {
                    // Error recovery: Just assume `()` since we're going to quit anyway
                    TySpan {ty: Ty::Unit, span: None}
                } else {
                    //TODO: Report an error when type variables are ambiguous (no resolved type)
                    unreachable!("bug: not all types were inferred into concrete types");
                },
            };

            (ty_var, ty_span.ty)
        }).collect()
    }
}
