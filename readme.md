[box](https://tonyday567.github.io/box/index.html) [![Build Status](https://travis-ci.org/tonyday567/box.svg)](https://travis-ci.org/tonyday567/box)
===

> The box is dark and full of terrors.

Take two ubiquitous concepts. There are things that emit stuff. They instantiate a particular thing on demand, somehow and somewhere beyond our concern and viewbox.  We ask an Emitter for an `a` and an `a` is there, or not (best to assume a `Maybe a`). The library calls this an Emitter:

```
newtype Emitter m a = Emitter { emit :: m (Maybe a)}
```
It emits over a type constructor, m, and this is often monadic in nature, and often IO.

Then there is the opposite.  There are things that commit stuff. We offer a Committer a particular thing and they take it away beyond our concerns. It is committed to the void. We offer an `a` and it is reported back whether the committment was succesful.The library calls this a Committer:

```
newtype Committer m a = Committer { commit :: a -> m Bool}
```

These two types are duals, across a wide spectrum of what that means. If you do something with a committer, like write to a socket, you can often immediately write an emitter the dual of this, like read from a socket.

When you have both things, something that emits and something that commits, over the same carrier, you have a Box:

```
data Box m c e = Box
  { committer :: Committer m c
  , emitter :: Emitter m e
  }
```

A Box tends to flip polarity, like a Möbius strip. and sometimes like a Dali.

You can see a committer as a wire into the box, and an emitter as a wire out of the box. You can then think of the box as opaque, and make the tasks on the outside of the box that much easier, removing concern about what is going on inside the box. But then coding tends to get you inside the box, and point-of-view shifts. From inside the box, stuff,, a's,, are coming from what we had thought of as the committer. It is emitting from our new viewbox, and vice versa. You are the black box, and your box matters. 

Note how you can get confused with these metaphors.  The wire going into the box is a committer from a point of view outside the metaphorical box but looks like an emitter from the inside. The wire going out is a committer from the inside and an emitter from the outside.

The key to understanding this library is to resist having to choose a single frame of reference and, instead, focus on the types.

A `Box m c e` unifies the functorial wrapper `m` of the emitter and committer, which is what really makes it a functional box. Boxes can often be hooked together at this level for useful efficiencies.

A Box is also a [profunctor](https://bartoszmilewski.com/2019/03/27/promonads-arrows-and-einstein-notation-for-profunctors/) and, to quote Bartos, a profunctor can be used to glue together two categories. A Box.Transducer, a stateful stream converter with a Category instance, can be used to connect up a box to create a sound compositional building block for arbitrarily complex problems.

Like other paradigms, boxes can be not fun places to get stuck in.  Much of the library are not, in fact, emitters and committers but emitter, committer and box continuations. If you gave me something that takes a box and does something, then I'll give you a something is a natural piece of logic within the library context, and so large parts of the functionality are [continuation-parsing style](https://ro-che.info/articles/2019-06-07-why-use-contt).

Finally, the interface between boxes is a natural place to create concurrency, and Box.Queue provides queues that are guaranteed to not race or block.

> “Do not define me by my gender or my socio-economic status, Noah Willis. Do not tell me who I am and do not tell me who society thinks I am and then put me in that box and expect me to stay there. Because, I swear to God, I will climb the hell out of that box and I will take that box you've just put me in and I will use that box to smash your face in until you're nothing more than a freckly, bloodied pulp. You got that, sweet cheeks?” ~ Megan Jacobson, Yellow


recipe
---

```
stack exec --test concurrency-tests --file-watch
```
