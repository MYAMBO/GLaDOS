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
	@$(STACK) test --coverage || true
	@mkdir -p $(COVERAGE_DIR)
	@stack hpc report --all --destdir $(COVERAGE_DIR) || true
	@echo "Running tests (vm)..."
	@cd $(VM_DIR) && $(STACK) test --coverage || true
	@cd $(VM_DIR) && stack hpc report --all --destdir coverage || true
	@echo "Running tests (compiler)..."
	@cd $(COMPILER_DIR) && $(STACK) test --coverage || true
	@cd $(COMPILER_DIR) && stack hpc report --all --destdir coverage || true
	@echo "Collecting coverage reports..."
	@$(MAKE) collect_coverage || true

xml_gen:
	@mkdir -p build/test-results/test
	@rm -f *.tix .hpc/* || true
	@echo "Generating XML (root)..."
	@stack test --test-arguments="--xml build/test-results/test/results.xml"
	@echo "Generating XML (vm)..."
	@cd $(VM_DIR) && stack test --test-arguments="--xml build/test-results/vm-results.xml" || true
	@echo "Generating XML (compiler)..."
	@cd $(COMPILER_DIR) && stack test --test-arguments="--xml build/test-results/compiler-results.xml" || true

collect_coverage:
	@mkdir -p $(COVERAGE_DIR)
	@ROOT_INDEX=$$(ls -d .stack-work/install/*/*/*/hpc/index.html 2>/dev/null | head -n1) ; \
	if [ -n "$$ROOT_INDEX" ]; then \
		cp "$$ROOT_INDEX" $(COVERAGE_DIR)/hpc_index_root.html ; \
		echo "Copied root coverage index to $(COVERAGE_DIR)/hpc_index_root.html" ; \
	else \
		echo "No root coverage index found to collect." ; \
	fi

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
