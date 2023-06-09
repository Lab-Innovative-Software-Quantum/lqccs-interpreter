# LQCCS Interpreter

Interpreter for linear quantum CCS.

# Contributors

Ferraro Domenico | ferraro.domenico125@gmail.com

### Table of contents
* [Grammar](#grammar)
* [Usage](#usage)

## Grammar
![grammar](./grammar.png)

## Testing
To test the parser you can see the tests in [test_parser.ml](./test/test_parser.ml) and run them with `make test_parser`.

To test the typecheck you can see the tests in [test_typecheck.ml](./test/test_typecheck.ml) and run them with `make test_typecheck`.

## Usage
Run the REPL loop with `make start`. If you want to pass a file to the interpreter you can simply use `make start path/to/file`.

### Install dependencies

``` 
make deps
```

### Build

``` 
make build
```
