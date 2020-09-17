use crate::nir;

/// A token provided by every call to `push` and passed to every call to `pop`
///
/// Helps verify that stack is being used coherently. (Not foolproof.)
// Intentionally not Copy since this should only be used once by move
#[derive(Debug, PartialEq, Eq, Hash)]
#[must_use]
pub struct ScopeToken(usize);

#[derive(Debug, Default)]
pub struct ScopeStack {
    stack: Vec<nir::Scope>,
}

impl ScopeStack {
    /// Pushes a new scope onto the scope stack
    pub fn push(&mut self) -> ScopeToken {
        self.stack.push(nir::Scope::default());
        ScopeToken(self.stack.len())
    }

    /// Pops the top scope from the scope stack and returns it
    ///
    /// Panics if there is currently no top scope
    pub fn pop(&mut self, token: ScopeToken) -> nir::Scope {
        let ScopeToken(expected_len) = token;
        assert_eq!(self.stack.len(), expected_len, "bug: mismatched scope push and pop calls");

        self.stack.pop().expect("bug: popped too many scopes")
    }

    /// Returns a reference to the top scope
    ///
    /// Panics if there is currently no top scope
    pub fn top(&self) -> &nir::Scope {
        self.stack.last().expect("bug: no current scope")
    }

    /// Returns a mutable reference to the top scope
    ///
    /// Panics if there is currently no top scope
    pub fn top_mut(&mut self) -> &mut nir::Scope {
        self.stack.last_mut().expect("bug: no current scope")
    }
}
