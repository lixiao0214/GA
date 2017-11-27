.DEFAULT_GOAL := help

.PHONY: help clean-logs clean tests

help:
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

clean-logs: ## Remove logs.
	find . -name '*.log' -exec rm -fv {} \;

clean: ## Remove python artifacts and logs.
	make clean-pyc
	make clean-logs

tests: ## Run tests
	echo "No test for now"
