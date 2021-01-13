all:
	stack build && stack exec site rebuild

clean:
	stack exec site clean

repl:
	stack ghci

publish:
	make clean && make all && git checkout gh-pages && ./copy.sh && \
		git add -A && git commit -m "Pull updates from 'main'" && git push && \
		git checkout master
