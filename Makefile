.PHONY: help copy-pkgs create-dir
.DEFAULT_GOAL := help

define BROWSER_PYSCRIPT
import os, webbrowser, sys, mkdocs

from urllib.request import pathname2url

webbrowser.open("file://" + pathname2url(os.path.abspath(sys.argv[1])))
endef
export BROWSER_PYSCRIPT

define PRINT_HELP_PYSCRIPT
import re, sys

for line in sys.stdin:
	match = re.match(r'^([a-zA-Z_-]+):.*?## (.*)$$', line)
	if match:
		target, help = match.groups()
		print("%-20s %s" % (target, help))
endef
export PRINT_HELP_PYSCRIPT

BROWSER := python3 -c "$$BROWSER_PYSCRIPT"
PACKAGES_DIR = /Users/t/Documents/R_Packages
TARGET_DIR = r-packages

help:
	@python3 -c "$$PRINT_HELP_PYSCRIPT" < $(MAKEFILE_LIST)

.PHONY: copy-pkgs create-dir

copy-pkgs: create-dir ## Copy R packages to r-packages/
	cp -rf $(PACKAGES_DIR)/ahead $(TARGET_DIR)/
	cp -rf $(PACKAGES_DIR)/bayesianrvfl $(TARGET_DIR)/
	cp -rf $(PACKAGES_DIR)/bcn $(TARGET_DIR)/
	cp -rf $(PACKAGES_DIR)/crossvalidation $(TARGET_DIR)/
	cp -rf $(PACKAGES_DIR)/esgtoolkit $(TARGET_DIR)/
	cp -rf $(PACKAGES_DIR)/forecastingapi $(TARGET_DIR)/
	cp -rf $(PACKAGES_DIR)/learningmachine $(TARGET_DIR)/
	cp -rf $(PACKAGES_DIR)/misc $(TARGET_DIR)/
	cp -rf $(PACKAGES_DIR)/simulatetimeseries $(TARGET_DIR)/
	find $(TARGET_DIR) -maxdepth 3 -name '.git' -type d -exec rm -rf {} +

create-dir: ## Create r-packages dir if doesn't exist
	echo "mkdir -p $(TARGET_DIR)"
