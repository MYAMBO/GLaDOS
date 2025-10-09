##
## EPITECH PROJECT, 2025
## GLaDOS VM
## File description:
## Makefile for the standalone VM
##

STACK = stack
OUTPUT = glados-vm

all: build

build:
	@echo "Building GLaDOS VM..."
	@$(STACK) install --local-bin-path "./"

run: build
	@echo "Running GLaDOS VM..."
	@./$(OUTPUT) $(ARGS)

clean:
	@echo "Cleaning GLaDOS VM project..."
	@$(STACK) clean

fclean: clean
	@echo "Removing generated VM files..."
	@rm -f $(OUTPUT)
	@rm -rf .stack-work

re: fclean all
