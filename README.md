# LQCCS Interpreter

Interpreter for linear quantum CCS.

## Contributors

Ferraro Domenico | ferraro.domenico125@gmail.com

## Grammar
![grammar](./grammar.png)

## Testing
To test the parser you can see the tests in [test_parser.ml](./test/test_parser.ml) and run them with `make test_parser`.

To test the typechecker you can see the tests in [test_typecheck.ml](./test/test_typecheck.ml) and run them with `make test_typecheck`.

To test the quantum operations you can see the tests in [test_qop.ml](./test/test_qop.ml) and run them with `make test_qop`.

To test the eval you can see the tests in [test_eval.ml](./test/test_eval.ml) and run them with `make test_eval`.

## Usage

### Install dependencies

```
make deps
```

### Build

```
make build
```

### Run

```
make start
```

With no arguments, the program executes the REPL loop. Otherwise, it is possible to run the interpreter on a single file using `make start path/to/file`.
