##
## EPITECH PROJECT, 2025
## GLaDOS VM
## File description:
## Makefile for the standalone VM
##

STACK = stack
OUTPUT = glados-compiler

all: $(OUTPUT)

$(OUTPUT):
	@echo "Building GLaDOS Compiler..."
	@$(STACK) install --local-bin-path "./"
	@mv $(OUTPUT) ../$(OUTPUT)

run: $(OUTPUT)
	@echo "Running GLaDOS Compiler..."
	@../$(OUTPUT) $(ARGS)

clean:
	@echo "Cleaning GLaDOS Compiler project..."
	@$(STACK) clean

fclean:	clean
	@echo "Removing generated Compiler files..."
	@rm -f ../$(OUTPUT)
	@rm -rf .stack-work

re:	fclean all
