##
## EPITECH PROJECT, 2025
## GLaDOS
## File description:
## Makefile
##

STACK = stack
TEST_DIR = test
COVERAGE_DIR = $(TEST_DIR)/coverage

all:	build_vm build_compiler

clean:	clean_vm clean_compiler

fclean:	clean fclean_vm fclean_compiler

tests_run:
	@echo "Running tests (root)..."
	@mkdir -p $(COVERAGE_DIR)
	@stack hpc report --all --destdir $(COVERAGE_DIR)
	@echo "Running tests (vm)..."
	@cd $(VM_DIR) && $(STACK) test --coverage
	@cd $(VM_DIR) && stack hpc report --all --destdir coverage
	@echo "Running tests (compiler)..."
	@cd $(COMPILER_DIR) && $(STACK) test --coverage
	@cd $(COMPILER_DIR) && stack hpc report --all --destdir coverage
	@echo "Collecting coverage reports..."

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


COMPILER_DIR = compiler
COMPILER_OUTPUT = glados-compiler

compiler:
	@$(MAKE) -C $(COMPILER_DIR) all

build_compiler:
	@$(MAKE) -C $(COMPILER_DIR) $(COMPILER_OUTPUT)

clean_compiler:
	@$(MAKE) -C $(COMPILER_DIR) clean

fclean_compiler:
	@$(MAKE) -C $(COMPILER_DIR) fclean

re_compiler:
	@$(MAKE) -C $(COMPILER_DIR) re
