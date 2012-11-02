#Include the common makefile. Keep this here
-include erl-project/make/Makefile-common.mk
.PHONY : update
SHELL := /bin/bash

############################
#    YOUR CHANGES BELOW    #
############################

#COOKIE=spapi
#APP=appname
#CONFIG=rel/files/sys

############################
#   COMMON ENTRIES BELOW   #
############################

# First target so we can run without explicit targets
# inherits from common Makefile, but runs first
all::       update
# getdeps for deployar and setup for ci
setup::		update
getdeps::   update

############################
#    DO NOT EDIT BELOW     #
############################

# Update erl-project. Allow failures of git (eg. executing git inside Mock env)
update: erl-project
	@echo "Updating erl-project"
	-@cd erl-project && git pull && cd ..

# Clone erl-project (only once executed) and add it to .gitignore
erl-project:
	@echo "Cloning erl-project"
	-@git clone -q git@github.com:spilgames/erl-project.git 
	-@$(shell [[ `grep -c "erl-project" .gitignore` -eq 0 ]] && echo "erl-project" >> .gitignore)
	@make $(MAKECMDGOALS)
