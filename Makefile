.DEFAULT_GOAL := build
EXE=lqccsint
BUILD_DIR=_build/default/
TESTDIR=test/samples
TEST_SOURCES := $(wildcard $(TESTDIR)/*.mc)
UNAME := $(shell uname -m)

ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
$(eval $(ARGS):;@:)

.PHONY: deps
deps: ## Install development dependencies
	opam install -y dune ocamlformat utop ocaml-lsp-server
	opam install --deps-only --with-test --with-doc -y .
ifeq ($(UNAME), arm64)
	brew install openblas
	export LDFLAGS="-L/opt/homebrew/opt/openblas/lib"
	export CPPFLAGS="-I/opt/homebrew/opt/openblas/include"
	export PKG_CONFIG_PATH="/opt/homebrew/opt/openblas/lib/pkgconfig"
	opam pin -n "git+https://github.com/mseri/owl.git#arm64" --with-version=1.1.0
	opam install owl.1.1.0
else
	opam install owl
endif

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	opam exec -- dune build --root .

.PHONY: reset-parser-messages
reset-parser-messages:
	cp _build/default/lib/parserMessages.auto.messages ./lib/parserMessages.messages

.PHONY: start
start: build ## Run the interpreter
	opam exec -- dune exec --root . bin/$(EXE).exe $(ARGS)

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean --root .

.PHONY: doc
doc: ## Generate odoc documentation
	opam exec -- dune build --root . @doc

.PHONY: servedoc
servedoc: doc ## Open odoc documentation with default web browser
	open _build/default/_doc/_html/index.html

.PHONY: fmt
fmt: ## Format the codebase with ocamlformat
	opam exec -- dune build --root . --auto-promote @fmt

.PHONY: watch
watch: ## Watch for the filesystem and rebuild on every change
	opam exec -- dune build --root . --watch

.PHONY: utop
utop: ## Run a REPL and link with the project's libraries
	opam exec -- dune utop --root . lib -- -implicit-bindings

.PHONY: test_typecheck
test_typecheck:  ## Run all the tests for the typechecker                                    
	opam exec -- dune exec test/test_typecheck.exe

.PHONY: test_parser
test_parser:  ## Run all the tests for the typechecker                                    
	opam exec -- dune exec test/test_parser.exe

.PHONY: test_eval
test_eval:  ## Run all the tests for the evaluation                                    
	opam exec -- dune exec test/test_eval.exe

.PHONY: test_qop
test_qop:  ## Run all the tests for the quantum operations                                    
	opam exec -- dune exec test/test_qop.exe

.PHONY: test
test: test_parser test_typecheck test_eval test_qop ## Run all the tests
