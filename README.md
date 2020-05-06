# DSim

[![Clojars
Project](https://img.shields.io/clojars/v/dvlopt/dsim.svg)](https://clojars.org/dvlopt/dsim)

[![cljdoc badge](https://cljdoc.org/badge/dvlopt/dsim)](https://cljdoc.org/d/dvlopt/dsim)

Compatible with Clojurescript.

Event-driven engine for Clojure(script) heavily borrowing ideas from
[discrete-event
simulation](https://en.wikipedia.org/wiki/Discrete-event_simulation) and
[hybrid dynamical systems](https://en.wikipedia.org/wiki/Hybrid_system).

Uniquely useful for programs involving time or prioritization, in some way or
another. Data science, simulations, games, animations, etc. Any program where
any state needs to evolve methodically over time, over some order.

(inspired by [the contrapuntal works of J.S.
Bach](https://www.youtube.com/watch?v=Y9OUfBDIGhw&t=3212s))


- [Socratic rationale](#socratic-rationale)
	- [Knowing our priorities](#knowing-our-priorities)
	- [Mixing time and space](#mixing-time-and-space)
	- [At the same time, but not exactly](#same-time)
	- [Creating time in our universe](#creating-time)
	- [Events scheduling events](#events-scheduling-events)
	- [Setting the universe into motion](#setting-into-motion)
	- [A brief history of the universe](#history-of-universe)
- [Discrete-event simulation](#discrete-event-simulation)
	- [Programmatic time](#programmatic-time)
	- [Activities](#activities)
	- [Working queues and flat events](#working-queues-flat-events)
	- [Knowing when to stop](#stopping)
	- [Nesting queues for safety and timing](#nested-queues)
	- [Being meta about managing event state across time](#queue-state)
	- [Periodicity and repetition](#replaying)
	- [Error handling](#error-handling)
	- [Determined to be stochastic](#random)
- [Continuous and hybrid simulation](#continuous-hybrid)
	- [Glide through time with finite and infinite flows](#flows)
	- [Samplers and flow deduplication](#flows)
	- [Offline and online animations](#animation)
- [Serialization](#serialization)
	- [Leveraging the `dvlopt/fdat` library](#fdat)
	- [When using Transit](#transit)
- [Async, parallelization, and optimizations problems](#parallel)
- [Writing your own specific engine](#writing-engine)
- [Last few words](#last-words)
- [Run tests](#run-tests)

## Socratic rationale <a name="socratic-rationale">

Framing most of our programs as simulations opens up a new perspective that many
software developers do not consider. However, Clojure already holds a special
place when it comes to having notions about how values evolve over time.
Starting slowly, step by step, we shall try to build a deeper understanding of
time in the majority of programs we write. After a few entertaining examples, we
hope the reader will reach an "Aha!" moment. We won't lie to you, it might need
more than one read, but we believe that it is useful and that anyone can
benefit from reconsidering the role of time in computation.

One might not have to strictly read linearly and might attempt to jump to
sections of interest. After reading the first few sections, at least.

Ensuring you grabbed a nice cup of coffee, let us require what we need for our
special hammock time to come:

```clojure
(require '[dvlopt.dsim :as dsim])
```

Or clone this repo:
```bash
$ git clone https://github.com/dvlopt/dsim.clj
$ cd dsim.clj
$ clj -A:dev:test

# and your favorite REPL
```

### Knowing our priorities <a name="knowing-our-priorities">

In one way or another, what is time but an information about ordering? In its
simplest definition, time unravels as a sequence of events. A simple way to
order function in Clojure would be to use sorted maps.

```clojure
(sorted-map 41 event-1
            42 (sorted-map 1 event-2
                           2 event-3))
```

By using a sorted map, it is trivial to "schedule" an event for any moment,
supposing that keys represent some point in time. By nesting them, it is trivial
to have higher-order prioritization. We can say that each event has `ranks`, a
lower rank meaning a higher priority. For instance, `event-3` is ranked at `[42
2]`.

### Mixing time and space <a name="mixing-time-and-space">

For given ranks, more than one event might occur. We need to differentiate
them.

```clojure
(sorted-map 41 {:asteroids {:b-612 sunset}}
            42 (sorted-map 1  {:characters {:little-prince watch-sunset}
                           37 {:characters {:little-prince feeling-happy}
                               :baobab     grow}))
```

Instead of mere event functions, we now have nested unsorted maps of event
functions. Following ranks and then a path, the event we discover is now
located both in time and space. We can say that the `feeling-happy` event
happens to `[:characters :little-prince]` (which is the path, space) exactly at
`[42 2]` (which are ranks, time). A the same time, a `:baobab` is growing.

Such a data structure is what we decided to call a `ranked tree`, as our
unsorted maps are indeed "ranked" using sorted ones. Those ranked trees are the
core of DSim engines but are also available as an external library:

[dvlopt/rktree](https://github.com/dvlopt/rktree.cljc)

### At the same time, but not exactly <a name="same-time">

Following the previous excerpt, we know that `:little-prince` first watches the
sunset and then feels happy. What really matters is that relative order.
Instead of using further ranking within `[42]`, it would actually be best using
a queue.

```clojure
(sorted-map 42 {:characters {:little-prince (dsim/queue watch-sunset
                                                        feeling-happy)}
                :baobab     grow})
```

A queue is simply a Clojure persistent queue, a lesser known data structure,
more convenient than a vector as we shall demonstrate all along. Besides clarity,
they bring safety. Indeed, if something fails and `:little-prince` is unable to
watch the sunset as intended, he will not feel happy because the whole queue
will fail.

While sorted maps provide precise timing towards the roots of our tree, queues as
leaves allow several events to be grouped at the same `path`, same `ranks`,
while usefully providing sequential ordering.

When modelling events happening conceptually at the same time, sometimes it is
still required to execute them in some sequence. We understand that
`:little-prince` feels happy right from the moment he stars watching the sunset,
virtually at the same time, but without watching it he would not be happy.
Feeling happy is `dependent` on watching the sunset, whereas `:baobab` growing
has nothing to do with all that and is `independent` from what is happening to
the `:little-prince`.

Modelling dependencies using a queue as a leaf is like saying: "Event B happens
only after Event A".

Modelling dependencies using further ranking, as in our previous version, is
like saying: "Event B happens no matter what, but if Event A happens, it must
execute before Event B".

### Creating time in our universe <a name="creating-time">

We have not discussed what those events actually do. What does it mean to feel
happy? How does it impact the world, anything?

```clojure
(defn feeling-happy
  [ctx]
  (assoc-in ctx
            (conj (dsim/path ctx)
                  :mood)
            :happy))


(def ctx
     (dsim/e-conj {}
                  [42]
                  [:characters :little-prince]
                  feeling-happy))
```

Our universe is called a `ctx` (context). It contains some state as well as the
events modifying that state through time. Starting from an empty map, we have
scheduled happiness for path `[:characters :little-prince]` at ranks `[42]`,
and void transformed into world. The API provides several `e-XXX` functions for
scheduling, removing, or retrieving events.

An `event` takes a `ctx` and returns an updated `ctx`. When it is executed, an
`event` has access to its path (a call to `dvlopt.dsim/path` suffice), meaning
it knows where it happens, what for. Feeling happy is not specific to the
`:little-prince`, we can easily reuse it for anyone else as evidenced by the
implementation.

### Events scheduling events <a name="events-scheduling-events">

Because an `event` modifies a `ctx` and a `ctx` contains all scheduled events,
it stands to reason an `event` can schedule other events.

```clojure
(defn feeling-happy
  [ctx]
  (let [path         (dsim/path ctx)
        future-ranks (update (dsim/ranks ctx)
                             0
                             +
                             100)]
    (-> ctx
        (assoc-in (conj path
                        :mood)
                  :happy)
        (dsim/e-conj future-ranks
                     path
                     feeling-happy))))
```

Happiness is addictive. When someone feels happy, it is automatically
rescheduled in the future, at the same path. An event can easily retrieve its
ranks (calling `dvlopt.dsim/ranks`) and update them, just as happiness is here
rescheduled for 100 time units later.

### Setting the universe into motion <a name="setting-into-motion">

Like Isaac Newton's god, after designing our universe, scheduling first events,
we need to set it into motion. We shall use the "ptime engine". It considers
that the first rank represent a specific point in time and applies additional
contraints. For instance, that point in time can only increase and never
decrease as one cannot go back in time. Following that first rank, it jumps
from point in time to point in time, executing everything there is for a given
point in time while respecting further ranking (if any).

```clojure
(def run
     (dsim/ptime-engine))


(def ctx
     (run (dsim/e-conj {}
                       [42]
                       [:me]
                       feeling-happy)))

(= ctx
   {:me           {:mood :happy}
    ::dsim/events (sorted-map 142 {:me (dsim/queue feeling-happy)})
    ::dsim/ptime  42})
```

After our first run happening at point in time 42 (as indicated by the `::dsim/ptime`
key in the `ctx`), I now feel happy for the first time. We can see that it is already
rescheduled for 100 time units later as expected.

### A history of the universe <a name="history-of-universe">

Instead of calling our engine repeatedly, we can modify it and pump it up.

```clojure
(def lazy-run
     (dsim/historic (dsim/ptime-engine)))


(def history
     (lazy-run ctx))
```

Now, `history` contains all points in time that happened. It is easily
navigable, it is just a sequence. We could even stop at some point, grab the
`ctx`, change a few things, and do a lazy run from now. Have different
timelines. Extremely useful for data science purposes and many more creative
applications.

That sequence we created is endless as we are rescheduling happiness without
question. Let us add an unpredictable event which could disrupt that.

```clojure
(defn covid-19
  [ctx]
  (if (< (rand)
         0.0001)
    (-> ctx
        dsim/stop
        (assoc :pandemic?
               true)
        (assoc-in [:me :mood]
                  :still-happy-nonetheless))
    (dsim/e-conj ctx
                 (update (dsim/ranks ctx)
                         0
                         +
                         (rand-int 404))
                 [:covid-19]
                 covid-19)))


(def ctx-2
     (dsim/e-conj ctx
                  [1000]
                  [:covid-19]
                  covid-19))


(= (last (lazy-run ctx-2))
   {:me          :still-happy-nonethelesss
    :pandemic?   true
    ::dsim/ptime some-future-point-in-time})
```

There is a tiny chance the pandemic will happen, but not 0. If it does not
happen, it is rescheduled, risk remains. If it does happen, the world comes to a
halt, all scheduled events are removed (`dvlopt.dsim/stop`). There is nothing
left to run and thus stops the history of our universe until you, its creator,
add other events and start running them again.

That `covid-19` function does something awfully common: scheduling something
later at the same path as now. We could refactor it like so, making it more
general and usable for any virus:

```clojure
(defn virus [probability]

  (fn possible-outbreak [ctx]
    (if (< (rand)
           probability)
      (-> ctx
          dsim/stop
          (assoc :pandemic?
                 true))
      (dsim/rel-conj ctx
                     (dsim/rank+ (rand-int 404))
                     possible-outbreak))))
```

Our `virus` function takes a probability and returns an event function. If it is
not the time for an outbreak, that event function reschedules itself using
`dvlopt.dsim/rel-conj` at the same path. When? It accepts a function `ctx ->
ranks` to decide so. We conveniently use `dvlopt.dsim/rank+` which provides such
a function, returning current ranks, summing the first one (point in time) with
what is given.

Those `ctx -> ranks` functions are used in a few places in the API. They provide great flexibility.


## Discrete-event simulation <a name="discrete-event-simulation">

Congratulations, we have reinvented [discrete-event
simulation (DES)](https://en.wikipedia.org/wiki/Discrete-event_simulation).

Indeed, we check the distinctive features of DES:

- An event is an instantaneous change in state having no duration.
- An event happens at one specific point in time.
- Time moves by jumping from event to event and from point in time to point in
time. Between two point in times, in the absence of events, nothing happens, no
matter how long the time interval.

In addition:

- Events happen at a specific `path`, making it easier to organize our data spatially as well.
- We acknowledge the fact that events at a point in time can be dependent
    (further ranking and queues solve that problem).
- By making events part of the context they act upon, we mimick a common Clojure
    idiom ([Pedestal's interceptors](http://pedestal.io/reference/interceptors),
    [Coffee
    Grinders](https://www.reddit.com/r/Clojure/comments/fr3jxd/coffee_grinders_part_2/),
    ...)

### Programmatic time <a name="programmatic-time">

Time in our programs can be of any unit. In a game, it would probably be actual
milliseconds. In an animation, probably frames to draw. In a simulation, maybe
minutes, days, or years.

We have used integers to denote points in time. In reality, we can use real
numbers. In DES, time behaves discreetly but is itself not discreet. In other
words, it jumps to the next point in time, as opposed to flowing continuously,
but that point in time can be any future number, whole or real. For instance, in
an animation, it is perfectly normal to schedule something for a fractional
frame (eg. frame 42.404).

### Activities <a name="activities">

In DES terminology, an activity is a sequence of events typically dispatched in
time. Assuming a world where 1 time unit represents 1 hour:

```clojure
(def sun-activity
     (dsim/queue sunrise
                 (dsim/wq-delay (dsim/rank+ 12))
                 sunset))
```

First, the sun rises. Right away, `dvlopt.dsim/wq-delay` grabs the rest of the
queue and schedules it for later. When? It accepts a function `ctx -> ranks` and
once again we use `dvlopt.dsim/rank+` which returns such a function that will
find the current ranks and update the first one (denoting the point in time,
here in hours) by adding 12 (hours).

It is best to use a queue here. We know that `sunset` happens after `sunrise`,
but without a `sunrise` there should be no `sunset`.

### Working queues and flat events <a name="working-queues-flat-events">

Queues can be nested. In more complex scenarios, it can be useful having queues
of queues of event functions. No matter how nested or not, the engine momentarily
creates a special map in the `ctx` at `:dvlopt.dsim/e-flat`.

```clojure
{::e-flat {::path  "Current path, as returned by dvlopt.dsim/path"
           ::queue "Current working queue"
           ::ranks "Current ranks, as returned by dvlopt.dsim/ranks"}}
```

The term "flat event" reminds the fact that whatever is at a given `path` and
given `ranks` is virtually executed as one event, no matter how many functions
and nested queues it actually implies. They are all executed in the context of
the same `::e-flat`. If we need to share some state between functions of a same
event, this is the place to do so. After execution, `::e-flat` is effectively
removed and everything inside is garbage-collected.

This leads us to a recursive definition of an event: an event is a function or a
queue of events.

Even when an event is a single function, the same setup applies, and the
provided working queue is an empty one where further events for immediate
execution can be enqueued.

An inner queue has only access to itself in `[::e-flat ::queue]`. This is a
useful property that will become more obvious with experience, as it provides
isolation: an inner queue cannot impact an outer queue.

All `e-XXX` functions have arities both for the current working queue in
`::e-flat` and future events kept at `::events`. For instance, we have used
`e-conj` up to now:

```clojure
;; Enqueues an event in the current working queue
;;
(dsim/e-conj ctx
             event)

;; Enqueues an event in the future event tree
;;
(dsim/e-conj ctx
             [42]
             [:some :path]
             event)


;; Fetches the current working queue, maybe for modifying it and using 
;; `e-assoc` to put it back.
;;
(dsim/e-get ctx)

;; Fetches an event scheduled at [42] for path [:some :path].
;;
(dsim/e-dissoc ctx
               [42]
               [:some :path])
```

Also, useful `wq-XXX` functions are meant to work directly on the working queue
or used as events in order to manipulate that working queue. When discussing
activities, we have already discovered how to create delays via
`dvlopt.dsim/wq-delay`.

### Knowing when to stop <a name="stopping">

We have already seen `dvlopt.dsim/stop` which removes all events and anything
currently executing (if any), meaning the engine has nothing left to run.

In order to stop the currently executing flat event only, one can call
`dvlopt.dsim/e-dissoc`. Further, in order to stop the current working queue
only, one can call `dvlopt.dsim/wq-dissoc`.

### Nesting queues for safety and timing <a name="nested-queues">

Inner queues working in complete isolation from outer ones, besides safety,
provide a useful property when it comes to timing. Let us consider:

```clojure
(dsim/queue (dsim/queue event-a
                        (dsim/wq-delay (dsim/rank+ 500))
                        event-b)
            event-c)
```

What event gets executed when? What you see is what you get.
 
The engine finds the first queue which becomes the working queue. Immediately, it
recognizes the first event as being a queue as well. That inner queue
momentarily becomes the working queue, while the outer one is paused. The engine
can now execute the first event function `event-a`. After completion, it hits a
delay of 500 time units. That delay, remember, grabs the rest of the (inner)
working queue (which now has only `event-b` left) and reschedules it for later
at the same path in the event tree. Without anything left to execute for that
inner queue, the engine now restores the outer queue and thus can execute
`event-c` straight away.

The fact that the activity (ie. that inner queue) we schedule has a delay does
not impact anything outside of it (ie. the outer queue containing `event-c`). No
matter the order we add the activity or `event-c` in, we know the activity will
perform as intented, undisturbed, and we know `event-c` will execute when
intented without any delay.

The only thing that could have happened is that the activity, as a whole, could
have been cancelled by a prior function event by popping it out of its outer
queue.

Such properties emerge from the simple fact that we treat queues as strict FIFO
(First-In First-Out) datastructure. Once we add an element to the end, we can
only pop the first element at the beginning. In other words, we cannot modify
any element without rebuilding a whole new queue, which we do not in DSim.

### Being meta about managing event state across time <a name="queue-state">

Suppose an activity such as:

```clojure
(dsim/queue event-a
            (dsim/wq-delay (dsim/rank+ 500))
            event-b)
```

What if `event-a` produces some state that is needed later by `event-b`? Where
should we keep it? Not in `::e-flat` as it is removed after current execution,
way before `event-b` happens (being rescheduled in the future).

We could keep that state directly in the `ctx`, in the global state, so to say.
But what if, for some reason, `event-b` which needed it and was supposed to
clean it is not executed? That would result in memory leaks.

Instead, we can rely on an understated but brilliant feature of Clojure:
metadata. An executing event function always has access to its queue, the
current working queue. By storing state needed by the queue in its own metadata,
that state will be garbage-collected whenever the queue is disposed of.
Wherever the queue goes, its metadata follows. It does not pollute the global
state in the `ctx`.

Besides the usual `meta` and `vary-meta` provided by Clojure, siblings are
provided to work directly on the current working queue:

```clojure
(def ctx-2 (dsim/wq-vary-meta ctx
                              assoc
                              :dope?
                              true))


(= {:dope? true}
   (dsim/wq-meta ctx-2))
```

### Periodicity and repetition <a name="replaying">

Often, events, activities or part of them, repeat in time. We can use the
convenient `dvlopt.dsim/wq-capture` function which saves in the metadata of the
queue, the queue itself at the moment of capturing. So meta. We can then replay
that queue from that moment when we want.

For instance, an activity automatically repeating itself 500 time units after
it
ends:

```clojure
(dsim/queue dsim/wq-capture
            event-a
            event-b
            event-c
            (dsim/wq-delay (dsim/rank+ 500))
            (dsim/wq-replay (fn pred? [ctx]
                              true)))
```

`dvlopt.dsim/wq-replay` takes a stateless `(fn [ctx] boolean)` which decides
whether to replay the last captured state of the queue or not. If not, then
that captured queue is cleaned.

`dvlopt.dsim/wq-sreplay` is a stateful version `(fn [ctx seed] state)`.
Returning nil means not repeating. Returning anything but nil is considered as
a new state for that predicate. `dvlopt.dsim/pred-repeat` is an example of
such a function for repeating a captured queue `n` times:

```clojure
(dsim/queue dsim/wq-capture
            event-a
            event-b
            event-c
            (dsim/wq-delay (dsim/rank+ 500))
            (dsim/wq-sreplay dsim/-pred-repeat
                             2))
```
That last queue will be executed once and will repeat twice (thus executing 3
times in total).

A queue can be captured at any time and capturing can be nested, which can lead
to complex sequences unfolding in time:

```clojure
(dsim/queue dsim/wq-capture
            event-a
            dsim/wq-capture
            event-b
            (dsim/wq-sreplay dsim/wq-pred-repeat
                             2)
            event-c
            (dsim/wq-sreplay dsim/wq-pred-repeat
                             1))

;; Will execute: event-a, event-b
;;                        event-b
;;                        event-b, event-c
;;                                 event-a, event-b,
;;                                          event-b,
;;                                          event-b, event-c
```

That way of capturing and replaying does not seem that useful in the last
example. It looks like a glorified inner loop. However, it becomes extremely
useful when delays are added. We can repeat things that began way back in the
past and it would be extremely tricky to implement without storing a queue in
its own metadata.

Generally speaking, this is a practical example of why it can be useful to store
information in the metadatas of queues.

### Error handling <a name="error-handling">

Error handlers are located at the level of queues. When an event function throws
an exception, the engine looks for an error handling function located at
`::on-error` in the metadata of the queue. If it does not find one, the
exception bubbles up to the outer queue (if there is one) and the process
repeats. If it does find one, then it calls it with :

```clojure
{::ctx       "The `ctx`, safe to use, 
 ::ctx-inner "Only present if the exception has been thrown in an inner queue
              that did not handle it. Can be used to know the state of the `ctx`
              right before the exception. Should be discard beyond that use
              as there is no point running that broken context."
 ::error     "The catched exception"}

```

If an error handler returns nil instead of a `ctx`, then the engine keeps
looking for another handler in outer queues. If none is found, it ultimately
rethrows the exception.

### Determined to be stochastic <a name="random">

Some simulation are purely deterministic, meaning that given the same initial
parameters, they will always produce the same results. Others are stochastic,
relying on some random parameters.

For instance, the textbook example of discrete-event simulation is modelling a
waiting queue at some bank. Clients come at some random interval and a bank
teller handles them during a random interval of time, we are interested in how
that waiting queue evolves.

Modelling randomness is a huge topic. One thing is certain, generating random
values usually follows a particular statistical distribution in order to be
more realistic and more representative of the real world.

If you know what this is all about, then we point you to the excellent
[kixi.stats](https://github.com/MastodonC/kixi.stats) library and more
specifically to the [kixi.stats.distribution](https://cljdoc.org/d/kixi/stats/0.5.2/api/kixi.stats.distribution<Paste>) namespace
which provides ways for randomly sampling common statistical distributions.

If you do not know what this is all about, you probably do not need it at the
moment. Which does not mean you should not be curious and research the topic.

## Continuous and hybrid simulation <a name="continuous-hybrid">

In a continuous simulation, time does not jump. It gradually and constantly
flows, much like in our universe. Remember that in discrete-event simulation,
between events at two consecutive point in times, nothing happens, regardless of
how long the interval in between. In a continuous simulation, because time
flows, between two points in times resides an infinity of points in time and
thus, an infinity of states. The fall of a ball is an example of a continuous
phenomenon.

One problem arises: our computers are digital machines. On a digital machine,
there is no such thing as continuous, instructions are executed in a discrete
fashion. It follows that continuous phenomena, although described by continuous
functions, must be discretized in some way. A straightforward way would be to
sample those functions at a needed rate. This is what we do in any sort of
animation where there are plenty of continuous phenomena being sampled,
computed only for each frame, as one cannot compute an infinity of states on a
digital machine (and does not need to, there is nothing to draw between two
frames).

Luckily, using our discete-event methodology, we can simply decide that sampling
such a continuous function is itself a scheduled event. Given a few additionals
twists, the next section about "flows" provides details.

Finally, some phenomena are hybrid. Going further, that ball falling in a
continuous motion will bounce when it hits the ground. Hitting the ground can be
modelled as an instantaneous change in behavior happening at one specific
moment. Yes, modelled as a discrete event. That ball, having two continuous
"modes", falling and bouncing, and one discrete transition in between, is indeed
hybrid. Because we start from the idea that everything is an event, we are
already good to go.

### Glide through time with finite and infinite flows <a name="flows">

"Flow" is the shorter name we give to a continuous phenomenon. It is created
using an event, sampled using events, but lives outside the event tree for the
course of its duration. More precisely, it located in the flow tree (under
`::flows` in the `ctx`)

Some flows are `infinite`. Either there are indeed endless, or we simply do not
know yet when they end. Maybe when the `ctx` satisfies some condition. Maybe
some sort of stochastic condition. It typically looks like this:

```clojure
(e-conj {}
        [42]
        [:some :path]
        (queue (infinite (fn my-flow [ctx]
                           (let [ctx-2 (something-something ctx)]
                             (if (some-condition ctx-2)
                               (end-flow ctx-2)
                               (rel-conj ctx-2
                                         (rank+ ...later...)
                                         sample)))))
                an-event))
```

Creating our infinite flow is an event itself. It will be automatically sampled
(executed) a first time at creation. In addition, when created, the rest of the
queue is temporarily paused and will resume when the flow ends. In other words,
`an-event` will be executed only after the flow. That way, it is trivial to
chain flows and events by simply putting them on the same queue.

That flow and all the information it needs, such as that its paused queue, now
lives in the flow tree. The structure of the flow tree mimicks the event tree.
When sampled, a flow can access its path within `ctx` like so:

```clojure
(= [::flows :some :path]
   (flow-path ctx))
```

Everything located at that path lives as long as the flow. We see that if
`some-condition` is satisfied, we use `dvlopt.dsim/end-flow` on our latest
`ctx`. This function cleans the state associated with the flow and resumes the
rest of the queue (if any). Which means that if we have some state to store in
order to run our flow, it is the best place to do so as we know it will be
garbage-collected by the engine. We need not to store it elsewhere in the `ctx`
just so we can pollute it and have to worry about managing it ourselves.

Other times, flows are finite. We know in advance when they end. A finite flow
created using `dvlopt.dsim/finite`, besides initialization, will also
automatically schedule a sample for when it ends.

Flows have a notion of a relative point in time located at `[::e-flat ::ptime]`
and returned by the `dvlopt.dsim/ptime` function. For infinite ones, that
relative ptime starts at 0 when the flow is created and represents its "age".
For finite ones, because we know when they end, we are often interested in
where we are relative to their forseen lifetime. Thus, the relative ptime of
finite flows is a percentage of completion, a value between 0 and 1
(inclusive). The global point in time of the `ctx` remains always accessible
under `::ptime`.

For instance, suppose this simple animation: an object moving on its X axis
from pixel 200 to pixel 800 during 2000 milliseconds, in a universe where time
units are indeed milliseconds. Using our infrastructure, it is a matter of
barely a few lines of code to write a flow that can move anything from A to B.

To make things even simpler, we use the `dvlopt.dsim/scale` function which can
scale any percent value:

```clojure
(defn move
  [pixel-a pixel-b ctx]
  (assoc-in ctx
            (path ctx)
            (scale pixel-a
                   pixel-b
                   (ptime ctx))))


(e-conj ctx
        [0]
        [:scene :objects :ball :x]
        (finite 2000
                (partial move
                         200
                         800)))
```

### Samplers and flow deduplication <a name="samplers">

Damn, we did create our moving object but we forgot to sample it. As such, it
will only be sampled (executed) at creation and at completion.

In order to write code that is more flexible, it is often best to sample flows
using an external `sampler`. As always, everything is an event, thus creating a
sampler is an event.

```clojure
(e-conj ctx
        [0]
        [:scene :objects :ball :x]
        (queue (sampler (rank+ (/ 1000
                                  60)))
               (finite 2000
                       (partial move
                                200
                                800)))) 
```

Our sampler will repeatly sample that path every `(/ 1000 60)` units of time.
Why so specific? Our chosen time units are milliseconds and we are drawing an
animation at 60 frames per second. We have to know the state of that object
every 1/60 of a second in order to know what to draw.

However, that would be a poor animation. Probably, you will want to add
gazillions of objects. Caring about sampling each and every element would
surely lead to a burnout. As it turns out, you can create a sampler at any path
and it would actually sample a whole subtree of the flow tree. Actually, for an
animation, knowing we have to sample everything, we can provide no path at all
so that the whole flow tree is sampled at once.

```clojure
(e-conj ctx
        [0 1000000]
        nil
        (sampler (rank+ (/ 1000
                           60))))
```

Convenient, isn't?

In addition, flows are deduplicated. For a given point in time, it is garanteed
that a flow will always be sampled at most once. Meaning that several samplers
can sample the same parts of the flow tree without worrying about a flow being
executed several times for a same point in time, which could lead to a disaster
if it is not idempotent.

The observant reader might have noticed that when scheduling the creation of
our sampler, besides a point in time of 0, we specified a second rank of
1000000. That second rank provide a higher ordering within that point in time
designated by the first rank. We could go on, add a third, and a fourth one,
and many more. However, in practise, two or three levels is the most that is
needed. When our events are independent, we do not even need any further
ranking beyond the point in time.

By adding that high second rank (meaning a very low priority, remember that 0
is the highest priority), we want that "sample-everything" sampler to run last
at the points in time of all the samples it does. This is a safety measure. By
doing so, we are saying: "sample everything that has not been sampled yet". We
do not disturb other samplers, or samplings of specific flows, that might need
to happen in a particular order, but we ensure that everything is sampled at
the end of each point in time we need to draw a frame for.

### Offline and online animations <a name="animation">

Those are not the exact terms employed in the litterature, but they are
meaningful to the programmer.

In an offline simulation, events are planned in the beginning, some parameters
configured, and then the engine does its thing. Events create other events,
usually, and there it goes. As seen, we can use `dvlopt.dsim/historic` on an
engine in order to obtain a lazy sequence of all intermediary steps (points in
time when using a `ptime-engine`). This is perfect for a whole category of
scientific endaveours like optimization problems, or for drawing
non-interactive animations.

In an online simulation, our virtual world interacts with the real world. Games
are a perfect example. For instance, in the browser, we dont know exactly at
what moment the next frame will be drawn, we wait for it to happen, and there
is a clear interaction between the gamer and the virtual world.

When it comes to the huge topic of any form of animation, offline or online
ones, the Clojure community is well endowed by having the
[Quil](http://www.quil.info/) library. Nonetheless, such a library typically
provides the "view" and the "view" only. Using DSim, we have a concrete
framework for the "model" and how state evolves over time, taking care of what
it is that we need to "view".

For instance, here is an example of a automated piano visualizer drawn with
Quil and modelled using DSim where played notes are discrete events creating
flows (anything that moves on screen):

[Shostakovich - E-Minor Prelude](https://youtu.be/XlEnrs0MDes)

## Serialization using `dvlopt/fdat` <a name="serialization">

### Leveraging the `dvlopt/fdat` library <a name="fdat">

There are many uses cases for which we would like to be able to serialize a
`ctx`. Saving the whole state of a game to a file, sharing a simulation with a
colleague, saving a long running simulation once in a while, distributed
computing, etc.

However, how could one serialize such a `ctx`? It contains a whole lot of
events, or flows, a whole lotta opaque functions.

The [dvlopt/fdat](https://github.com/dvlopt/fdat.cljc) library orginates from
the DSim project. It proposes a somewhat novel solution for serializing
functions and other impossible things such as infinite sequences.

`dvlopt.dsim/serializable` provides functions that need to be registered if
serialization is needed. Naturally, it is assumed the user is familiar with
[dvlopt/fdat](https://github.com/dvlopt/fdat.cljc) in the first place.

### When using Transit <a name="transit">

The [dvlopt/fdat](https://github.com/dvlopt/fdat.cljc) library currently
supports [Nippy](https://github.com/ptaoussanis/nippy) as well as Transit. However, Transit does not directly support queues and does not distinguish between sorted maps and unsorted ones.

Transit remains pretty much the only available option when exchanging such data
between Clojure and Clojurescript. The following package provides what is needed
for using it with DSim. [Transit-clj](https://github.com/cognitect/transit-clj)
or [Transit-cljs](https://github.com/cognitect/transit-cljs) must be added to dependencies.

[![Clojars
Project](https://img.shields.io/clojars/v/dvlopt/dsim.transit.svg)](https://clojars.org/dvlopt/dsim.transit)

```clojure
(require '[dvlopt.dsim.transit :as dsim.transit])


(doc dsim.transit/reader-handlers)

(doc dsim.transit/writer-options)
```

Writer options provide `:handlers` for writer as well as the `:transform`
function. `Attention`, when using Clojurescript, you must also explicitly add the latest version of [Transit-js](https://github.com/cognitect/transit-js) to dependencies. This is due to a current bug in the dependency pulling of Transit-cljs which import an earlier version which does not support `:transform`.


## Async, parallelization, and optimization problems <a name="parallel">

Enforcing such strong and concrete notions about time presents us with a
blessing seldom received. Following everything we have discussed, by definition,
within given ranks, all paths are strictly independant. Meaning all paths can
be executed in any order. Meaning it is garanteed they can run in parallel.

Were we bound to Clojure only, parallelization and handling asynchronous events
would be almost trivial to implement. However, we strive to write code portable
for Clojurescript and Javascript has no blocking semantics. Meaning that one
event being asynchronous in our whole universe forces us to go full async.

Before committing to such a radical decision, we prefer waiting for feedback from the community.

Some use cases do not need anything special due to the fact that a `ctx` is
completely immutable. Notably, optimization problems. The main purpose of
simulation is to test likely outcomes given some initial parameters in the hope
we can find the best parameters. It is a matter of exploration. At any
interesting moment, we can take our current `ctx`, change a few things, and
keep running different timelines in parallel, simply on another thread. This is
impossible in simulation libraries were state is mutable. At least, it would
involve full copying everytime.

Furthermore, when running massive simulations and testing many scenarios
exhausting what a single machine can offer, implementing a distributed
environment tends to be extremely time consuming. That is why we took great
care into ensuring that a `ctx` is serializable at any moment (given you
leverage the [dvlopt/fdat](https://github.com/dvlopt/fdat.cljc) library) and thus, easily shared between machines.

## Writing your own specific engine <a name="writing-engine">

The `ptime-engine` is probably what you want and what we have discussed. A `basic-engine` also exists which has no concept of "point in times" nor flows, only events at ranks providing ordering.

They both built upon `dvlopt.dsim/engine*`. An advanced user could build
another specific "time-based engine" as well.

Unless you are a mad scientist or maybe some crazy physicist modelling universes
where time is multi-dimensional, you probably do not need to build your own
thing.

## Last few words <a name="last-words">

If you read the whole thing, these one thousand lines of README, sincere
congratulations.

If you skipped to the end, well, congratulations for being a curious persona.

Believe it or not, there is even more to discover in the API. But rest assured
that we have definitely covered a huge chunk of it. All of this would not have
been possible without the semantics offered by the Clojure programming
language, its immutability and extreme "dynamicity". We believe this library is
a bit of a new twist when it comes to that sort of problems.

That was probably quite a few new concepts. We hope all this information will
grant you any benefit at all even if you do not end up using DSim.


## Run tests <a name="run-tests">

Run all tests (JVM and JS based ones):

```bash
$ ./bin/kaocha
```

Due to some compilation shenaningans, testing both JS environments in the same
run can fail.

For Clojure only:

```bash
$ ./bin/kaocha jvm
```

For Clojurescript on NodeJS, `ws` must be installed:
```bash
$ npm i ws
```
Then:
```
$ ./bin/kaocha node
```

For Clojurescript in the browser (which might need to be already running):
```bash
$ ./bin/kaocha browser
```
## License

Copyright © 2019 Adam Helinski

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
