##
## EPITECH PROJECT, 2025
## GLaDOS
## File description:
## Makefile
##

STACK = stack
EXECUTABLE = $(shell $(STACK) path --local-install-root)/bin/glados
OUTPUT = ./glados
TEST_DIR = test
COVERAGE_DIR = $(TEST_DIR)/coverage

all: build

build:
	@echo "Building Haskell project with Stack..."
	@$(STACK) install --local-bin-path "./"

run: build
	@echo "Running executable..."
	@$(OUTPUT) -- $(ARGS)

clean:
	@echo "Cleaning project..."
	@$(STACK) clean

fclean: clean
	@echo "Removing generated files..."
	@rm -rf .stack-work $(OUTPUT)
	@rm -rf $(COVERAGE_DIR)/*
	@rm -rf .hpc/
	@rm -f report.xml

tests_run:
	@echo "Running tests..."
	@$(STACK) test --coverage
	@mkdir -p $(COVERAGE_DIR)
	@stack hpc report --all --destdir $(COVERAGE_DIR)

xml_gen:
	@stack test --test-arguments="--xml=report.xml"

re: fclean all
