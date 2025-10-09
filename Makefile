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
VM_DIR = VM

all: $(OUTPUT) build_vm

$(OUTPUT):
	@echo "Building Haskell project with Stack..."
	@$(STACK) install --local-bin-path "./"

run: $(OUTPUT)
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
	@mkdir -p build/test-results/test
	@rm -f *.tix .hpc/* || true
	@stack test --test-arguments="--xml build/test-results/test/results.xml"

re: fclean all

.PHONY: clean fclean tests_run xml_gen re run



VM_DIR = VM
VM_OUTPUT = glados-vm

vm:
	@$(MAKE) -C $(VM_DIR) all

build_vm:
	@$(MAKE) -C $(VM_DIR) $(glados-vm)

run_vm:
	@$(MAKE) -C $(VM_DIR) run ARGS="$(ARGS)"

clean_vm:
	@$(MAKE) -C $(VM_DIR) clean

fclean_vm:
	@$(MAKE) -C $(VM_DIR) fclean

re_vm:
	@$(MAKE) -C $(VM_DIR) re
