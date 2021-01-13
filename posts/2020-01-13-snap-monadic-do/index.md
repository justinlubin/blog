---
title: 'Do as I Say: &ldquo;Monadic Do&rdquo; Notation in Snap!'
fancyTitle: 'Do as I Say: <i>Monadic Do</i> Notation in Snap<i>!</i>'
draft: true
---

[Snap*!*](https://snap.berkeley.edu/) is a programming language with a
block-based editor. For a while, I was skeptical of its expressive capabilities,
but, after finally playing around with it myself, I realized that it has support
for some extremely expressive constructs that allow for embedded domain-specific
languages.

As part of
[Sarah Chasins](https://schasins.com/)'s
[CS 294: Building User-Centered Programming Tools](http://schasins.com/cs294-usable-programming-2020/)
class, I implemented a *monadic do* construct in Snap*!*, similar to
[monadic do notation in Haskell](https://en.wikibooks.org/wiki/Haskell/do_notation)
and
[binding operators in OCaml](https://caml.inria.fr/pub/docs/manual-ocaml/bindingops.html). 


# Functor and Monad Typeclasses

First, I introduced a blocks that allows for the definition of "anonymous"
functor insances:

![The anonymous functor typeclass definition block.](functor.png)

As well as one for anonymous monad instances:

![This anonymous monad typeclass definition block.](monad.png)

## hello

mroe content
