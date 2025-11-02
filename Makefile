##
## EPITECH PROJECT, 2025
## GLaDOS
## File description:
## Makefile
##

STACK = stack
TEST_DIR = test
COVERAGE_DIR = $(TEST_DIR)/coverage

all:    build_vm build_compiler build_decompiler

clean:    clean_vm clean_compiler clean_decompiler

fclean:    clean fclean_vm fclean_compiler fclean_decompiler

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

DECOMPILER_DIR = decompiler
DECOMPILER_OUTPUT = glados-decompiler

decompiler:
	@$(MAKE) -C $(DECOMPILER_DIR) all

build_decompiler:
	@$(MAKE) -C $(DECOMPILER_DIR) $(DECOMPILER_OUTPUT)

clean_decompiler:
	@$(MAKE) -C $(DECOMPILER_DIR) clean

fclean_decompiler:
	@$(MAKE) -C $(DECOMPILER_DIR) fclean

re_decompiler:
	@$(MAKE) -C $(DECOMPILER_DIR) re
