# Simply Typed Lambda Calculus

To get started in purescript, I decided to follow along with the
"Type Theory in Purescript" videos on Youtube:
https://www.youtube.com/watch?v=T51P_uICy9E&list=PLg0VnzA8LyrHofInH4gVUoOWnWDXo0LOg

In particular I want to learn the parser combinator library in order to
be able to use purescript for a toy compiler.

This directory will probably contain more or less exactly what they develop
in the videos.

## Some notes on what I learned

The biggest single thing is that I made a little progress in understanding
recursive definitions in PureScript.

Before I get into that, something important to realize is that most languages -
here OCaml / ML is an exception rather than the norm - *automatically* allow
functions to be mutually recursive. This can work out of the box trivially since
function evaluation is by definition a kind of lazy operation, and a function
itself is just a pointer to instructions that can reference any other pointers.

Mutually recursive *data* on the other hand is always tricky. Most eager
languages like Python don't really allow it at all, and instead folks rely on
mutation to do lazy evaluation.

Haskell and Ocaml do allow it - Haskell can handle it out of the box because it
evaluates *everything* lazily, and Ocaml allows a `let rec` form over data so
that I think you can define mutually recursive data without mutable fields (actually
I'm only 80% sure of this, and you may have to use some kind of lazy field marker to make it work in which case Ocaml is actually more like Purescript in some ways).

Purescript on the other hand does not have a `let rec` form, *and* doesn't let
you just mutate things willy-nilly. The solution is to *directly* express the
underlying fixpoint operation used by (both self- and mutual-) recursion: for
mutually recursive definitions you need to make callbacks explicit and use the
`fix` operator to define the high-level data built out of mutual recursion.

This comes up very quickly in parser combinators. Unlike hand-written parsers
where the components are plain functions (and hence can be mutually recursive without
any special work), parser combinators are *data* - admittedly they are just functions
under the hood but semantically they are values and cannot reference one another
recursively.

You can see the solution - which requires that the underlying type implement
`Lazy` which I still need to learn more about - in the `parseTerm` definition.

This, by the way, is where I got stuck trying to write a simple parser
combinator in purescript by hand. It might be worth eventually understanding
how Lazy actually works and trying to go ahead and do this so that I really
grok the fundamentals!