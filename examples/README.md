# Examples

1. [Red box][redbox_view] ([.elm][redbox_source])
2. [Photo viewer][photo_view] ([.elm][photo_source])
3. [Red box, but using physics][redbox_physics_view] ([.elm][redbox_physics_source])
4. The famous [Example 8][e8_view] ([.elm][e8_source]) from [The Elm Architecture tutorial][eatut].

A more sophisticated series of examples demonstrating progressive
addition of animation and behaviour to an application while keeping
the core application model unmodified --

5. [veggies1][veggies1_view] ([.elm][veggies1_source])
   Presents the core interface which has two lists of labels - "fruits" and
   "veggies". Click on a label to move it to the other list. The idea is to
   sort the two lists to have the fruits correctly under the fruits list
   and the veggies under the veggies list. The program doesn't verify that
   you've actually done so, but just lets you play around.

6. [veggies2][veggies2_view] ([.elm][veggies2_source])
   Functionally exactly the same as the previous one, but prepares for more
   sophisticated animation by moving the layout functionality into the 
   "director" mechanism.

7. [veggies3][veggies3_view] ([.elm][veggies3_source])
   Functionally the same as the previous - i.e. you move labels from one list
   to the other by clicking on them. However, the movement is now smoothly
   animated. You can click as fast as you want and even on elements in flight,
   if you're that fast. In other words, these are retargetable animations.

8. [veggies4][veggies4_view] ([.elm][veggies4_source])
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

