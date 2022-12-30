# Makefile to generate installer format-g-docs
myhome:=$(shell cygpath -u ${HOME})
VENV:=$(myhome)/venvs/format-g-docs

all: format-g-docs.exe      ## Target executable

# test:
# 	echo venv=$(VENV)
# 	echo comspec=$(COMSPEC)
# 	echo shell=$(SHELL)
# 	echo unixpath=/$(subst \,/,$(subst :\,/,$(VENV)))
# 	echo $(myhome)
# 	echo $(shell cygpath -u ${HOME})

format-g-docs.exe: requirements.txt format-g-docs.py      ## Target executable
	python -m venv $(VENV)
	source $(VENV)/Scripts/activate; pip install -r requirements.txt; pyinstaller --onefile --paths $(VENV)/Lib/site-packages format-g-docs.py
	mv dist/format-g-docs.exe .


clean:             ## clean all of the generated files except the final executable
	rm -rf ~/venvs/format-g-docs
	rm -rf dist
	rm -rf build
	rm format-g-docs.spec


.PHONY: help clean
.DEFAULT_GOAL := help
help: ## Targets for this Makefile
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
