##
## EPITECH PROJECT, 2025
## GLaDOS VM
## File description:
## Makefile for the decompiler
##

STACK = stack
OUTPUT = glados-decompiler

all: $(OUTPUT)

$(OUTPUT):
	@echo "Building GLaDOS DeCompiler..."
	@$(STACK) install --local-bin-path "./"
	@mv $(OUTPUT) ../$(OUTPUT)

run: $(OUTPUT)
	@echo "Running GLaDOS DeCompiler..."
	@../$(OUTPUT) $(ARGS)

clean:
	@echo "Cleaning GLaDOS DeCompiler project..."
	@$(STACK) clean

fclean:	clean
	@echo "Removing generated DeCompiler files..."
	@rm -f ../$(OUTPUT)
	@rm -rf .stack-work

re:	fclean all
