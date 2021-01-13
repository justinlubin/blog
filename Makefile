all:
	stack build && stack exec site rebuild

clean:
	stack exec site clean

repl:
	stack ghci
