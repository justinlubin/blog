all:
	stack build && stack exec site rebuild

watch:
	stack exec site watch

clean:
	stack exec site clean

repl:
	stack ghci

publish:
	make clean && make all && git checkout gh-pages && ./copy.sh && \
		git add -A && git commit -m "Pull updates from 'main'" && git push && \
		git checkout main
