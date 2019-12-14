help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

init: ## Install dependencies with guix
	guix package -i guile3.0-bytestructures guile3.0-gcrypt guile-arew wiredtiger@3.2.0-0

check: ## Run tests
	guile -L . check.scm
	@echo "\033[95m\n\nWin!\n\033[0m"

todo: ## Things that should be done
	@grep -nR --color=always --after-context=4 TODO .

xxx: ## Things that require attention
	@grep -nR --color=always --after-context=4 XXX .

repl: ## Start a guile REPL with rlwrap
	rlwrap guile -L .

web: ## Start the default web server
	guile -L . nomunofu.scm serve 3 8080

latest-lexemes.nt:
	wget https://dumps.wikimedia.org/wikidatawiki/entities/latest-lexemes.nt.bz2
	bzip2 -d latest-lexemes.nt.bz2

index: ## Index test.nt
	time guile -L . nomunofu.scm index 3 test.nt

query: ## Send a query to the server
	./query.py

clean:
	rm -f okvs.wt* WiredTiger*
