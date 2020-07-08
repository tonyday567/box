[box](https://tonyday567.github.io/box/index.html) [![Build Status](https://travis-ci.org/tonyday567/box.svg)](https://travis-ci.org/tonyday567/box)
===

> The box is dark and full of terrors.

Take two ubiquitous concepts. There are things that emit stuff. They instantiate a particular thing on demand, somehow and somewhere beyond our concern and frame of reference.  We ask for an `a` and an `a` is there, or not (the thing is a `Maybe a`). The library calls this an Emitter:

```
newtype Emitter m a = Emitter { emit :: m (Maybe a)}
```

And then there is the opposite.  There are things that commit stuff. We offer them a particular thing and they take it away beyond our concerns. It is committed to the void. We offer an `a` and it is committed, or not. The library calls this a Committer:

```
newtype Committer m a = Committer { commit :: a -> m Bool}
```

When you have both things, something that emits and something that commits, and you product them, the library calls this a Box:

```
data Box m c e = Box
  { committer :: Committer m c
  , emitter :: Emitter m e
  }
```

If you choose to see the committer as a conduit into the box, the emitter as a conduit out of the box, and remove concern about what is going on inside the box, then this frame of reference is often called a black box. Or you can choose to think from inside the box. From inside the box, there is again a conduit coming in that emits and a conduit going out that commits. You can take those things being emitted, turn them into something else and emit them.  You are the black box. 

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
stack build --test --exec "$(stack path --local-install-root)/bin/concurrency-tests" --file-watch
```
