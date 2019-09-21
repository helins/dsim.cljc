# dvlopt.dsim

[![Clojars
Project](https://img.shields.io/clojars/v/dvlopt/dsim.svg)](https://clojars.org/dvlopt/dsim)

![](http://www.classicgaming.cc/classics/asteroids/images/asteroids-arcade-screenshot.jpg)

For animations, games, and scientific purposes.

Discrete event simulation is an important field for working on a multitude of
common tasks such as animation. However, it is not needed to know any
theoretical concept, this library provides a simple way of understanding what it
is all about.

## Usage

We will need this:
```clj
(require '[dvlopt.dsim :as dsim])
```

Here is the [full API](https://dvlopt.github.io/doc/clojure/dvlopt/dsim/).

The following instructions might be more lengthy than absolutely needed but once
understood, the user will be able to create any complex animation or simulation
in a simple and composable fashion.

### Transitions

Everything revolves around transitions. A transition is a pure stepwise function
gradually modifying some arbitrary state presented in the form of a map.
Actually, transitions are part of the state itself and are located under a
specific key (ie. `dvlopt.dsim/transition-key`). It is both common and desired
for transitions to mirror the structure of the data they act upon. Hence, they
are often organized in nested maps. Let us pretend we wish to animate asteroids
moving in 2D:

```clj
(def state
     {dsim/transition-key {:asteroids {42 {:x ...
                                           :y ...}}}
      :asteroids {42 {:x 450
                      :y 1420}}})
```

This mirroring pattern is so common that, for instance, the path leading to an
:x value would be called the "data-path" (ie. [:asteroids 42 :x]). A transition,
once it is created, takes 3 arguments: the state it needs to probably modify,
its data-path, and the current step. A step represents some discrete point in
time. In a live animation, that would most likely be the current millisecond.

```clj
;; Let us add a transition starting from step 0, lasting 2000 steps, and moving
;; asteroid 42 on its X axis from 200 pixels to 800.

(def state-2
     (assoc-in state
               (dsim/transition-path [:asteroids 42 :x])
               (dsim/once 0
                          2000
                          (fn on-step [state data-path percent]
                            (assoc-in state
                                      data-path
                                      (dsim/scale 200
                                                  800
                                                  percent))))))
```

As described below, transitions are created by providing at least their first
step (ie. when do they begin), how many steps they last, and an `on-step`
function which takes 3 arguments just like the resulting transition. However,
the 3rd argument is not the current step but rather a percentage of completion
computed as (current-step - first-step) / n-steps. It makes things a lot easier
because the step itself does not really matter, we simply need to know how far
in the transition are we.

There exists 3 kinds of basic transitions:

- `once`, which lasts `n-steps` and terminates
- `infinite`, which endlessly repeats `n-steps`
- `repeating`, which repeats `n-steps` `n-times`

When creating non-infinite transitions, an `on-complete` function can be
provided. When a transition terminates, it automatically removes itself from the
state and calls its `on-complete` if there is any. This function takes 4
arguments: the current state map, the data-path, the completion step and the
current step. When some steps are skipped or missed, the completion step and the
current step might not match, hence it is useful to provide both. This happens
in live animations when a frame is not drawn at the exact millisecond a
transition is supposed to complete. Those `on-complete` functions are useful
when some action needs to be taken after completion, such as creating a new
transition.

### Poly-transitions

Chaining transitions is so common that we can now introduce poly-transitions.
Those higher-order transitions are created by providing a sequence of functions
returning transitions. After each such "sub-transition", the poly-transition
take care of creating and adding the next "sub-transition" by calling the next
function in the sequence. 3 arguments are provided to those functions: the state
at that moment, what should be the first step of the created "sub-transition",
and an `on-complete` function whose purpose is to - sort of recursively - repeat
this process for the next "sub-transition".

There are several reasons why a poly-transition follows a sequence of functions
returning transitions rather than prepared ones . One reason is that the user
does not need to mention what is the exact first-step of each sub-transition, it
is provided as an argument to the functions depending on when the previous
sub-transition ends. Another one is that such functions can be reused anywhere
else. Yet another one is that the state is very often needed to create the
transition (eg. fetching the current position of an asteroid). The state passed
to those functions is always up-to-date.

```clj
;; While it simply moves on its X axis, we want asteroid 42 to move on its Y
;; axis as well. Starting at step 0, it will first go up from 500 pixels to 700
;; in 1000 steps. Then, right after that, it will go down from 700 pixels back
;; to 500 in 2000 steps.

;; `dvlopt.dsim/fn-once` helps us for creating a function returning a transition
;; as mentioned in the last paragraph.

(def state-3
     (assoc-in state-2
               (dsim/transition-path [:asteroids 42 :y])
               (dsim/poly state-2
                          0
                          [(dsim/fn-once 1000
                                         (fn on-step [state data-path percent]
                                           (assoc-in state
                                                     data-path
                                                     (dsim/scale 500
                                                                 700
                                                                 percent))))
                           (dsim/fn-once 2000
                                         (fn on-step [state data-path percent]
                                           (assoc-in state
                                                     data-path
                                                     (dsim/scale 700
                                                                 500
                                                                 percent))))])))
```

Unlike for basic transitions, we pass the current state to `dsim/poly`. Remember
that those functions producing transitions receive the current state as an
argument. Therefore, we must provide it for the first function in the sequence
in order to bootstrap the poly-transition.

Similarly to basic transitions, there exists 3 kinds of poly-transitions:

- `poly`, like `once`, goes through its sequence only once
- `poly-infinite`, like `infinite` endlessly goes through its sequence
- `poly-repeating`, like `repeating`, goes through its sequence
exactly `n-times`

Those 6 functions for creating transitions have helpers prefixed by `fn-` (eg.
`once` -> `fn-once`) which can be used for creating poly-transitions just like
in the example above. It is trivial to create arbitrarily complex nested
poly-transitions.

### Non-linear modeling

Our asteroid is moving linearly. What if we want it to move non-linearly? For
instance, going faster and faster on its X axis? This is simply done by mapping
the percentage of completion, which is linear, to a non-linear progression. For
instance, we could use the ol' quadratic function as such:

```clj
(fn on-step [state data-path percent]
  (assoc-in state
            data-path
            (dsim/scale 200
                        800
                        (Math/pow percent
                                  2))))
```

### Less boilerplate by using the library

The library provides a series of functions for removing boilerplate. For
example, you have probably noticed we always use `assoc-in` with the data-path
in our `on-step` functions. We do so because we use the mirroring pattern
exposed at the beginning of those instructions. However, some reason could lead
us to organize things differently if needed. Hence, the mirroring pattern is not
enforced and the user has the liberty to do as it pleases. We can use some
utility functions from the library to simplify the mirroring pattern as well as
a few other things:

```clj
;; First, we merge our transitions with the current state which is simply an
;; empty map at this point. Our `on-step` functions are very declarative, they
;; take care of mapping the percentage of completion to a pixel value, linearly
;; or not, and assoc'ing this value at the data-path. Our transitions are also
;; provided with a pre-existing `on-complete` function which will entirely
;; remove our asteroid from the state only when all its transitions are done.

(def state
     (dsim/merge-transitions
       {}
       {:asteroids {42 {:x (dsim/once 0
                                      1000
                                      (dsim/fn-mirror-percent (comp (dsim/fn-scale 200
                                                                                   800)
                                                                    #(Math/pow %
                                                                               2)))
                                      dsim/remove-pre-data)
                        :y (dsim/poly {}
                                      0
                                      [(dsim/fn-once 1000
                                                     (dsim/fn-mirror-percent (dsim/fn-scale 500
                                                                                            700)))
                                       (dsim/fn-once 2000
                                                     (dsim/fn-mirror-percent (dsim/fn-scale 700
                                                                                            500)))]
                                      dsim/remove-pre-data)}}}))
```

Refer to the API for other utilities.

### Moving the state through "time"

Up to now, we have only created transitions. However, remember transitions are
just functions, they will not do anything if there are not called. Since we are
all setup, we can move our state through steps. Remember a step is any point in
some arbitrary time. It could be a specific frame in an animation, or a specific
millisecond in a live animation, a meaningful step in a scientific simulation,
or anything.

This is the most basic way of taking a state and moving it to the step you want:

```clj
(def state-at-0
     (dsim/move state
                0))
```

All transitions starting at step 0 now have gone through their first step. In
other words, our asteroid has begun moving. Often, there is no need for moving
step by step by ourselves. We can use the following function for mapping a
sequence of steps to a lazy sequence of [state' step]:

```clj
(def simple-simulation
     (dsim/move-seq state
                    (range)))
```

Indeed, the sequence of steps we provide starts from 0 and goes to infinity.
However, the sequence of [state' step] will not be infinite. It stops as soon as
there are no more transitions because it make no sense to continue, at this
point the state cannot evolve anymore. Hence, our sequence of states stops when
our asteroid stops moving.

This is a bit too easy. In real use, such as in the context of a game or any
non-trivial animation, events happen and they modify the state. For instance, if
a gamer presses the "jump" button, the on-screen character must indeed jump in
the game. Such events must be associated with a step and be handled properly.
Our state could be in an atom. We would move our state whenever a frame is
drawn, providing as a step the current time in milliseconds. Everytime the
"jump" button is pressed, we `swap!` this atom and add a transition to our
character so that it jumps.

Nonetheless, when drawing an animation a posteriori or doing some scientific
simulation, we often have the events in advance. It becomes tricky to take care
at the same time of both our state moving through steps and events happening at
some particular steps. Hence, here is what we can do using the library. Let us
say we want to create a short film of moving asteroids:

```clj
;; Our sequence of events. 2 asteroids will be created and they will eventually
;; collide. Each step is a millisecond.

(def events
     [{:type       :create-asteroid
       :number     0
       :x          200
       :y          250
       ::dsim/step 0}
      {:type       :create-asteroid
       :number     1
       :x          750
       :y          600
       ::dsim/step 2000}
      {:type        :collision
       :x           500
       :y           500
       ::dsim/step  5000}])


;; In a non-trivial simulation, we often need to see where things are going.
;; Here, our asteroids will move towards a point of collision, we need to know
;; that in advance. We find the :collision event and extract that point.

(defn initial-state

  [events]

  (let [collision (first (filter (comp #(identical? %
                                                    :collision)
                                       :type)
                                 events))]
    {:collision (select-keys collision
                             [::dsim/step
                              :x
                              :y])
     :status    :running}))



;; There are only 2 kinds of events: the creation of an asteroid and the final
;; collision. When an asteroid is created, a transition is setup so it moves
;; from its original coordinates as described in the event to the point of
;; collision we have extracted in our initial state. For clarity as well as
;; emphasizing the fact they last the same number of steps, the :x and :y values
;; are modeled within the same transition. It is also slightly more efficient.

(defn handle-event

  [state event]

  (condp identical?
         (:type event)
    :create-asteroid (let [collision     (:collision state)
                           asteroid-step (::dsim/step event)
                           n-steps       (- (::dsim/step collision)
                                            asteroid-step)]
                       (dsim/merge-transitions
                         state
                         {:asteroids {(:number event) (dsim/once asteroid-step
                                                                 n-steps
                                                                 (let [scale-x (dsim/fn-scale (:x event)
                                                                                              (:x collision))
                                                                       scale-y (dsim/fn-scale (:y event)
                                                                                              (:y collision))]
                                                                   (dsim/fn-mirror-percent (fn percent->coords [percent]
                                                                                             {:x (scale-x percent)
                                                                                              :y (scale-y percent)}))))}}))
    :collision       {:status :bang}))


;; All right, now we can lazily compute our sequence of [state' step]. Our
;; events will handled when needed. Although we provide an infinite range of
;; steps, the sequence stops as soon as there are no more events and all
;; transitions finish, meaning the state is stable and cannot evolve anymore.

(def simulation
     (dsim/move-events (initial-state events)
                       (range)
                       events
                       handle-event))


;; That's all. But what if we want to draw this animation as a series of files
;; using the Quil library? It easy provided we already have some `draw-state`
;; function somewhere. The only thing we have to do with our simulation is to
;; run it so that steps represent frames and not milliseconds. With a frame-rate
;; of 60 frames per second:

(doseq [[state'
         step]  (let [events' (map #(update %
                                            ::dsim/step
                                            dsim/millis->n-steps
                                            60)
                                   events)]
                  (dsim/move-events (initial-state events')
                                    (range)
                                    events'
                                    handle-event))
  (draw-state state')
  (q/save (format "frame-%016d.jpeg"
                  step)))
```

### In conclusion

By being built as it is, this library offers an easy and efficient way for
writing transitions while remaining particularily flexible if the user needs to
handle some special case. In the end, it is just functions calling functions,
nothing magic.


## License

Copyright © 2019 Adam Helinski

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
