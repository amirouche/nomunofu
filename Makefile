SOURCES = 					\
	README.md				\
	src/srfi/srfi-1.md			\
	src/srfi/srfi-2.md			\
	src/srfi/srfi-4.md			\
	src/srfi/srfi-5.md			\
	src/srfi/srfi-6.md			\
	src/srfi/srfi-8.md			\
	src/srfi/srfi-9.md			\
	src/srfi/srfi-13.md			\
	src/srfi/srfi-14.md			\
	src/srfi/srfi-16.md			\
	src/srfi/srfi-17.md			\
	src/srfi/srfi-19.md			\
	src/srfi/srfi-23.md			\
	src/srfi/srfi-25.md			\
	src/srfi/srfi-26.md			\
	src/srfi/srfi-28.md			\
	src/srfi/srfi-29.md			\
	src/srfi/srfi-31.md			\
	src/srfi/srfi-34.md			\
	src/srfi/srfi-35.md			\
	src/srfi/srfi-37.md			\
	src/srfi/srfi-38.md			\
	src/srfi/srfi-39.md			\
	src/srfi/srfi-41.md			\
	src/srfi/srfi-42.md			\
	src/srfi/srfi-43.md			\
	src/srfi/srfi-45.md			\

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

doc:
	pandoc $(SOURCES) -o arew-scheme.pdf

repl: ## repl for the win
	@./run

check: ## run tests using the library test runner (arew specific)
	mkdir -p profile
	./venv scheme --program make-check.scm

todo: ## Things that should be done
	@grep -nR --color=always TODO src/

xxx: ## Things that require attention
	@grep -nR --color=always --before-context=2  --after-context=2 XXX src/
