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

For starters, look at the examples in the
[examples/](https://github.com/srikumarks/elm-anima/tree/master/examples)
folder. Pre-compiled versions of these examples are also available in the
[examples/build/](https://github.com/srikumarks/elm-anima/tree/master/examples/build)
folder.

**Status**: Work in progress

## Examples

1. [Red box][redbox_view] ([source][redbox_source])
2. [Photo viewer][photo_view] ([source][photo_source])
3. [Red box, but using physics][redbox_physics_view] ([source][redbox_physics_source])

[redbox_view]: https://cdn.rawgit.com/srikumarks/elm-anima/master/examples/build/redbox.html
[redbox_source]: https://github.com/srikumarks/elm-anima/blob/master/examples/redbox.elm
[photo_view]: https://cdn.rawgit.com/srikumarks/elm-anima/master/examples/build/photos.html
[photo_source]: https://github.com/srikumarks/elm-anima/blob/master/examples/photos.elm
[redbox_physics_view]: https://cdn.rawgit.com/srikumarks/elm-anima/master/examples/build/redbox_physics.html
[redbox_physics_source]: https://github.com/srikumarks/elm-anima/blob/master/examples/redbox_physics.elm

## Key idea

The framework treats an "app" as a wiring up of four processes - a "modeller", "director",
"animator" and "viewer".

- The **Modeller** is a process that maintains a "model" in response to user input. 
  The data in this model is typically what the user cares about. It does not include
  any presentation related information.

  ```elm
  modeller : Automaton input model
  ```

- The **Director** is a process that provides an indication of the stable state of
  the view in response to input and the updated model. The director does not worry
  about the details of how this stable state of the UI is going to be achieved.

  ```elm
  director : Automaton (input, model) direction
  ```

- The **Animator** is a process that takes the indications of the director and worries
  about how to reach the indicated stable state. Its output is detailed instructions for
  "what should be shown right now".

  ```elm
  animator : Animation direction viewstate
  ```

- The **Viewer** takes the instantaneous instructions of the animator and shows the UI.
  This can usually be a pure function that computes a `Html`.

  ```elm
  viewer : Automaton (model, viewstate) output
  ```

## Auxiliary concepts

### Animation

An animation is thought of as a process that takes an input value valid for a
small time interval and produces a corresponding output value for the same time
interval.

```elm
type alias Animation input output = Automaton (TimeStep, input) (TimeStep, output)
```

### Particle

Physics based animations are built around the notion of a "particle", which is seen
as a process that responds to a number of forces by updating its position in a 
"phase space" consisting of the pair `(position, momentum)`.

```elm
type alias Particle space = Animation (List (Force space)) (PhasePos space)
type alias PhasePos space = (space, space)
type Force space
        = Drag space                -- Drag positionVector
        | SomeForce space           -- SomeForce forceVector
        | Kick space                -- Kick impulseVector
        | Wall space Float space    -- Wall normalVector coeffOfRestitution pointOnWall
        | Buff space space          -- a wall where coeffOfRestitution is zero
        | Friction Float Float      -- Friction restSpeed fricCoeff
        | Spring space Float Float  -- Spring anchorPoint hookeConstant dampingFactor
        | Gravity space             -- Gravity gvector
```

### Space

Animations apply to continuous values, although there are generalizations to
discrete values too. We need to be able to do some common calculations on these
values such as linear interpolation, find distances and apply dynamics. The
values can be one, two or three dimensional, or can be colour. The mathematical
notion of a vector space is useful in this regard. 

To capture this variety of what can be animated and the nature of the
calculations that animations entail, the module `Space` introduces the abstract
notion of a continuous space of points that we move smoothly though.  The other
animation concepts rely on this abstraction.


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


