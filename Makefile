SOURCES = 					\
	README.md				\
	src/scheme/base.md			\
	src/scheme/bitwise.md			\
	src/scheme/box.md			\
	src/scheme/bytevector.md		\
	src/scheme/case-lambda.md		\
	src/scheme/char.md			\
	src/scheme/charset.md			\
	src/scheme/comparator.md		\
	src/scheme/complex.md			\
	src/scheme/cxr.md			\
	src/scheme/division.md			\
	src/scheme/ephemeron.md			\
	src/scheme/eval.md			\
	src/scheme/file.md			\
	src/scheme/fixnum.md			\
	src/scheme/flonum.md			\
	src/scheme/generator.md			\
	src/scheme/hash-table.md		\
	src/scheme/ideque.md			\
	src/scheme/inexact.md			\
	src/scheme/lazy.md			\
	src/scheme/list-queue.md		\
	src/scheme/list.md			\
	src/scheme/load.md			\
	src/scheme/lseq.md			\
	src/scheme/mapping.md			\
	src/scheme/mapping/hash.md		\
	src/scheme/process-context.md		\
	src/scheme/r5rs.md			\
	src/scheme/read.md			\
	src/scheme/regex.md			\
	src/scheme/repl.md			\
	src/scheme/rlist.md			\
	src/scheme/set.md			\
	src/scheme/sort.md			\
	src/scheme/stream.md			\
	src/scheme/text.md			\
	src/scheme/time.md			\
	src/scheme/vector.md			\
	src/scheme/write.md			\

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
