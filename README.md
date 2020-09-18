# pea

[![Build Status](https://dev.azure.com/sunjayv/Pea/_apis/build/status/sunjay.pea?branchName=master)](https://dev.azure.com/sunjayv/Pea/_build/latest?definitionId=6&branchName=master)

Programming languages / compilers / interpreters experiments

## Building and Running Tests

To build the interpreter and run it on a sample file:

```bash
cargo run tests/run-pass/hello.pea
```

To run tests:

```bash
cargo test
```

To run tests and overwrite the `.stdout` and `.stderr` files:

```bash
TESTRUNNER=overwrite cargo test
```

This should only be used when the output is modified and those files need to be updated.
