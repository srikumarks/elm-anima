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

Some notes on the approach and motivation are also available in
[this blog post](http://sriku.org/blog/2015/12/13/towards-reactive-animation-in-elm/).

**Status**: Work in progress. Unoptimized. Use at your own risk.

## Examples

1. [Red box][redbox_view] ([.elm][redbox_source])
2. [Photo viewer][photo_view] ([.elm][photo_source])
3. [Red box, but using physics][redbox_physics_view] ([.elm][redbox_physics_source])
4. The famous [Example 8][e8_view] ([.elm][e8_source]) from [The Elm Architecture tutorial][eatut].

A more sophisticated series of examples demonstrating progressive
addition of animation and behaviour to an application while keeping
the core application model unmodified --

4. [veggies1][veggies1_view] ([.elm][veggies1_source])
   Presents the core interface which has two lists of labels - "fruits" and
   "veggies". Click on a label to move it to the other list. The idea is to
   sort the two lists to have the fruits correctly under the fruits list
   and the veggies under the veggies list. The program doesn't verify that
   you've actually done so, but just lets you play around.

5. [veggies2][veggies2_view] ([.elm][veggies2_source])
   Functionally exactly the same as the previous one, but prepares for more
   sophisticated animation by moving the layout functionality into the 
   "director" mechanism.

6. [veggies3][veggies3_view] ([.elm][veggies3_source])
   Functionally the same as the previous - i.e. you move labels from one list
   to the other by clicking on them. However, the movement is now smoothly
   animated. You can click as fast as you want and even on elements in flight,
   if you're that fast. In other words, these are retargetable animations.

7. [veggies4][veggies4_view] ([.elm][veggies4_source])
   A bit richer functionality. You can reorder within a list or
   across lists by dragging and dropping labels. Once dropped, the items all
   animate into their new positions smoothly. Also, you get indicators showing
   what's been picked up and where it will be dropped if you let go of the
   mouse picker. This is a precursor to full multi-touch interactions.

[redbox_view]: https://cdn.rawgit.com/srikumarks/elm-anima/master/examples/build/redbox.html
[redbox_source]: https://github.com/srikumarks/elm-anima/blob/master/examples/redbox.elm
[photo_view]: https://cdn.rawgit.com/srikumarks/elm-anima/master/examples/build/photos.html
[photo_source]: https://github.com/srikumarks/elm-anima/blob/master/examples/photos.elm
[redbox_physics_view]: https://cdn.rawgit.com/srikumarks/elm-anima/master/examples/build/redbox_physics.html
[redbox_physics_source]: https://github.com/srikumarks/elm-anima/blob/master/examples/redbox_physics.elm
[veggies1_view]: https://cdn.rawgit.com/srikumarks/elm-anima/master/examples/build/veggies1.html
[veggies2_view]: https://cdn.rawgit.com/srikumarks/elm-anima/master/examples/build/veggies2.html
[veggies3_view]: https://cdn.rawgit.com/srikumarks/elm-anima/master/examples/build/veggies3.html
[veggies4_view]: https://cdn.rawgit.com/srikumarks/elm-anima/master/examples/build/veggies4.html
[veggies1_source]: https://github.com/srikumarks/elm-anima/blob/master/examples/veggies1.elm
[veggies2_source]: https://github.com/srikumarks/elm-anima/blob/master/examples/veggies2.elm
[veggies3_source]: https://github.com/srikumarks/elm-anima/blob/master/examples/veggies3.elm
[veggies4_source]: https://github.com/srikumarks/elm-anima/blob/master/examples/veggies4.elm
[e8_view]: https://cdn.rawgit.com/srikumarks/elm-anima/master/examples/build/example8.html
[e8_source]: https://github.com/srikumarks/elm-anima/blob/master/examples/example8.elm
[eatut]: https://github.com/evancz/elm-architecture-tutorial/

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

- The **Animator** is a process that takes the indications of the director and
  worries about how to reach the indicated stable state. Its output is detailed
  instructions for "what should be shown right now". While it may look like the
  distinction between the director and the animator is merely that of role, the
  runtime treats them on different schedules. The director gets to process all
  input events, including frame step clock ticks. So it may execute an
  arbitrary number of times every second. However, the animator runs at 60fps
  - i.e. only once on every frame step.

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
        = Drag space                -- Drag positionVectorChange
        | SomeForce space           -- SomeForce forceVector
        | Kick space                -- Kick impulseVector
        | Wall space Float space    -- Wall normalVector coeffOfRestitution pointOnWall
        | Buff space space          -- a wall where coeffOfRestitution is zero
        | Friction Float Float      -- Friction restSpeed fricCoeff
        | Spring space Float Float  -- Spring anchorPoint hookeConstant dampingFactor
        | Gravity space             -- Gravity gvector
```

### Picker

The `picker` is an automaton that tracks drag and drop operations. One item
at a time can be in the "being picked and dragged" state and the picker
continuously outputs information about what is currently picked, if any,
and if so how far has it been moved.

```elm
type PickerAction = PickupItem String | MoveItem | DropItem
type alias Picker space = Automaton (space, Maybe PickerAction) (Maybe (String, space, Bool))
picker : Picker Point2D
```

The input is the pair `(mousePos, action)` and the output is
an optional triple of `(key, moveExtent, active)`. The `active` boolean
will turn false the moment the picker is dropped and will be true as long
as the item remains "picked".

**PPS**: Drag-n-Drop using these concepts does not use the HTML drag events.
Instead, it works directly with the mouse events. For the moment, I think this
is better than the HTML drag events since we're not limited to dragging only
one thing at a time ... though I'm yet to build a picker that supports
multitouch.

**PS**: The "particle collection" concept uses a `Dict`, which I think is not
the most efficiently implemented, but will probably suffice for these experiments.

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


