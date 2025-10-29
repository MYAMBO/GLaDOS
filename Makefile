##
## EPITECH PROJECT, 2025
## GLaDOS
## File description:
## Makefile
##

STACK = stack
TEST_DIR = test
COVERAGE_DIR = $(TEST_DIR)/coverage

all:    build_vm build_compiler

clean:    clean_vm clean_compiler

fclean:    clean fclean_vm fclean_compiler

tests_run:
	@$(STACK) clean
	@echo "\n--- Running Haskell Compiler Tests (with coverage) ---"
	@$(STACK) test glados-compiler --coverage
	@echo "\n--- Running Virtual Machine Tests (no coverage) ---"
##@$(STACK) test glados-vm --no-coverage

xml_gen:
	@mkdir -p build/test-results/test
	@rm -f *.tix .hpc/* || true
	@stack test --test-arguments="--xml build/test-results/test/results.xml"

re: fclean all

VM_DIR = vm
VM_OUTPUT = glados-vm

vm:
	@$(MAKE) -C $(VM_DIR) all

build_vm:
	@$(MAKE) -C $(VM_DIR) $(VM_OUTPUT)

clean_vm:
	@$(MAKE) -C $(VM_DIR) clean

fclean_vm:
	@$(MAKE) -C $(VM_DIR) fclean

re_vm:
	@$(MAKE) -C $(VM_DIR) re

COMPILER_OUTPUT = glados-compiler
COMPILER_DIR = compiler

build_compiler:
	@$(MAKE) -C $(COMPILER_DIR) $(COMPILER_OUTPUT)

clean_compiler:
	@$(MAKE) -C $(COMPILER_DIR) clean

fclean_compiler:
	@$(MAKE) -C $(COMPILER_DIR) fclean

re_compiler:
	@$(MAKE) -C $(COMPILER_DIR) re