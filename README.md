# elm-anima

Anima is an attempt at an animation framework entirely in Elm and intended to
be used with the [Elm architecture][elmarch].  This library is the product of
an experiment to arrive at an architecture using types and the algebra around
them as [functional thinking][ft] tools. Anima encompasses normal "fixed"
animations, retargetable/additive animations as well as physics-based
animations.  The core library is about 500 lines of Elm code and includes an
opinionated notion of application architecture that's an elaboration of the
[Elm architecture][elmarch]. The purpose of the library is not so much as
a production grade animation library, but as an exploration of concepts.

For starters, look at the examples in the `examples/` folder. Pre-compiled
versions of these examples are also available in the `examples/build/' folder.

**Status**: Work in progress

# Examples

1. [Red box](https://github.com/srikumarks/elm-anima/blob/master/examples/redbox.elm)
2. [Photo viewer](https://github.com/srikumarks/elm-anima/blob/master/examples/photos.elm)
3. [Red box, but using physics](https://github.com/srikumarks/elm-anima/blob/master/examples/redbox_physics.elm)


[Elm]: http://elm-lang.org
[virtual-dom]: https://github.com/evancz/virtual-dom
[elmarch]: http://elm-lang.org/guide/architecture
[afrp]: http://haskell.cs.yale.edu/wp-content/uploads/2011/02/workshop-02.pdf
[Automaton]: http://package.elm-lang.org/packages/evancz/automaton/1.0.1/Automaton
[fast]: http://elm-lang.org/blog/blazing-fast-html
[Automaton.run]: http://package.elm-lang.org/packages/evancz/automaton/1.0.1/Automaton#run
[Signal.Mailbox]: http://package.elm-lang.org/packages/elm-lang/core/2.1.0/Signal#mailboxes
[elm-discuss-anim]: https://groups.google.com/forum/#!topic/elm-discuss/4sAbCc6HmVM
[ft]: http://sriku.org/blog/2015/08/11/talk-functional-thinking-for-fun-and-profit/


