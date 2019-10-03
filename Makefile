SOURCES = 					\
	README.md				\
	src/arew/srfi/srfi-1.md			\
	src/arew/srfi/srfi-2.md			\
	src/arew/srfi/srfi-4.md			\
	src/arew/srfi/srfi-5.md			\
	src/arew/srfi/srfi-6.md			\
	src/arew/srfi/srfi-8.md			\
	src/arew/srfi/srfi-9.md			\
	src/arew/srfi/srfi-13.md		\
	src/arew/srfi/srfi-14.md		\
	src/arew/srfi/srfi-16.md		\
	src/arew/srfi/srfi-17.md		\
	src/arew/srfi/srfi-19.md		\
	src/arew/srfi/srfi-23.md		\
	src/arew/srfi/srfi-25.md		\
	src/arew/srfi/srfi-26.md		\
	src/arew/srfi/srfi-28.md		\
	src/arew/srfi/srfi-29.md		\
	src/arew/srfi/srfi-31.md		\
	src/arew/srfi/srfi-34.md		\
	src/arew/srfi/srfi-35.md		\
	src/arew/srfi/srfi-37.md		\
	src/arew/srfi/srfi-38.md		\
	src/arew/srfi/srfi-39.md		\
	src/arew/srfi/srfi-41.md		\
	src/arew/srfi/srfi-42.md		\
	src/arew/srfi/srfi-43.md		\
	src/arew/srfi/srfi-45.md		\
	src/arew/srfi/srfi-48.md		\
	src/arew/srfi/srfi-51.md		\
	src/arew/srfi/srfi-54.md		\
	src/arew/srfi/srfi-60.md		\
	src/arew/srfi/srfi-61.md		\
	src/arew/srfi/srfi-67.md		\
	src/arew/srfi/srfi-69.md		\
	src/arew/srfi/srfi-98.md		\
	src/arew/srfi/srfi-101.md		\
	src/arew/srfi/srfi-111.md		\
	src/arew/srfi/srfi-113.md		\
	src/arew/srfi/srfi-115.md		\
	src/arew/srfi/srfi-116.md		\
	src/arew/srfi/srfi-117.md		\
	src/arew/srfi/srfi-124.md		\
	src/arew/srfi/srfi-125.md		\
	src/arew/srfi/srfi-127.md		\
	src/arew/srfi/srfi-128.md		\
	src/arew/srfi/srfi-132.md		\
	src/arew/srfi/srfi-133.md		\

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

doc:
	cat $(SOURCES) > arew-scheme.md
	pandoc arew-scheme.md -o arew-scheme.html
	pandoc arew-scheme.html -o arew-scheme.pdf

repl: ## repl for the win
	@./run

check: ## run tests using the library test runner (arew specific)
	mkdir -p profile
	./venv scheme --program make-check.scm

todo: ## Things that should be done
	@grep -nR --color=always TODO src/

xxx: ## Things that require attention
	@grep -nR --color=always --before-context=2  --after-context=2 XXX src/
