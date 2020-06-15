(ns dvlopt.dsim

  "Idiomatic, purely-functional discrete event simulation and much more.
  
   See README first in order to make sense of all this. It provides definitions and rationale for concepts.
  
  
   Arity convention for `e-XXX` functions
   ======================================
  
   Arities for `e-XXX` functions follow the same convention. Providing both `ranks` and `path` refers explicitely
   to a prioritized location in the event tree. Without `path`, the path of the currently executing flat event is
   retrieved (ie. acts with the given `ranks` relative to the current `path`). Not providing either refers explicitely
   to the current flat event and its working queue.
   
  "

  {:author "Adam Helinski"}

  (:require [dvlopt.dsim.util     :as dsim.util]
            [dvlopt.fdat          :as fdat #?(:clj  :refer       
                                              :cljs :refer-macros) [?]]
            [dvlopt.rktree        :as rktree]
            [dvlopt.void          :as void])
  #?(:clj (:import (clojure.lang ExceptionInfo
                                 PersistentQueue))))


;;;;;;;;;; API structure (searchable for easy navigation)
;;
;; @[queue]   Queues
;; @[misc]    Miscellaneous functions
;; @[scale]   Scaling numerical values
;; @[ctx]     Generalities about contextes
;; @[events]  Adding, removing, and modifying events
;; @[ngin]    Building time-based event engines
;; @[wq]      Relative to the currently executed queue (aka. the "working queue")
;; @[flows]   Creating and managing flows
;; @[fdat]    Serialization of whole contexts (events and flows included) via the `dvlopt.fdat` library




;;;;;;;;;; MAYBEDO


;; Hack persistent queues so that they can also act as a stack?
;; 
;; The front is implemented as a seq, so prepending is efficient, but the seq is
;; package private. Relying on non-public features is not recommended. On the other
;; hand, it is fairly certain the implementation will stay like this.
;;
;; In CLJS, turns out the seq is readily accessible as nothing is really private.


;; Async + parallelize by ranks?
;;
;; By definition, all events with the same ranking are independent, meaning that they
;; can be parallelized without a doubt if needed. But due to the non-blocking nature of
;; JS, it is hard to find a portable solution without buying completely into core.async,
;; meaning going async all the way...


;;;;;;;;;; Gathering all declarations


(declare e-update
         path
         wq-dissoc
         wq-vary-meta)


;;;;;;;;;; @[queue]  Queues


(defn queue

  "Clojure has persistent queues but no straightforward way to create them.
  
   Here is one."

  ([]

   #?(:clj  PersistentQueue/EMPTY
      :cljs cljs.core/PersistentQueue.EMPTY))


  ([& values]

   (into (queue)
         values)))




(defn queue?

  "Is `x` a persistent queue?
  
   See also [[queue]]."

  [x]

  (instance? #?(:clj  PersistentQueue
                :cljs cljs.core/PersistentQueue)
             x))


;;;;;;;;;; @[misc]  Miscellaneous functions


(defn millis->utime

  "Converts an interval in milliseconds to an arbitrary time unit happening `hz` times per second.

   ```clojure
   ;; Converting milliseconds to frames for an animation.
   ;; We know something lasts 2000 milliseconds and the frame-rate is 60 times per second.
   ;; Hence, it lasts 120 frames.

   (= (millis->utime 2000
                     60)
      120)
   ```"

  [millis hz]

  (long (Math/round (double (* (/ hz
                                  1000)
                               millis)))))


;;;;;;;;;; @[scale]  Linear scaling of numerical values


(defn- -minmax-denorm

  ;; Scale a percent value to an arbitrary range.
  ;;
  ;; Undoes [[minimax-norm]].

  [min-v interval v-norm]

  (double (+ (* v-norm
                interval)
             min-v)))



(defn scale
  
  "Linear scaling of numerical values.

   | Arity | Means |
   |---|---|
   | 3 | Scales a `percent` value to be between `min-v` and (+ `min-v` `interval`) inclusive. &
   | 5 | Scales `v`, between `min-v` and (+ `min-v` `interval`), to be between `scaled-min`v and (+ `scaled-min-v` `scaled-interval`) inclusive. |

   ```clojure
   (scale 200
          100
          0.5)

   250


   (scale 200
          100
          2000
          1000
          2500)

   250
   ```"

  ([min-v interval percent]

   (-minmax-denorm min-v
                   interval
                   percent))

  ([scaled-min-v scaled-interval min-v interval v]

   (-minmax-denorm scaled-min-v
                   scaled-interval
                   (/ (- v
                        min-v)
                      interval))))



(defn minmax-norm

  "Min-max normalization, linearly scales `x` to fit between 0 and 1 inclusive.

   See [[scale]](arity 3), which is the opposite operation.
  
   ```clojure
   (min-max-norm 20
                 10
                 25)

   0.5
   ```"

  [min-v interval v]

  (double (/ (- v
                min-v)
             interval)))


;;;;;;;;; @[ctx]  Generalities about contextes


(defn flow-path

  "Returns the path in the flow tree of the currently executing flow.

   When a flow is created, it is added to the flow tree (`[::flows]` in `ctx`) using [[path]].
   When a flow ends, it is removed from the flow tree.

   Instead of polluting the global state in the `ctx`, if a flow needs some state in order to
   work during its lifetime, this is the perfect place to keep it as it will be removed and
   garbage collected when the flow ends."

  ([ctx]

   (into [::flows]
         (path ctx))))



(defn flowing?

  "Is the given `ctx` or some part of it currently flowing?"

  ([ctx]

   (flowing? ctx
             nil))


  ([ctx path]

   (not (empty? (get-in ctx
                        (cons ::flows
                              path))))))



(defn next-ptime

  "On what ptime is scheduled the next event?
  
   Returns nil if there is none."

  [ctx]

  (ffirst (::events ctx)))



(defn path

  "Returns the path of the currently executing flat event (ie. under `[::e-flat ::path]`)."

  [ctx]

  (::path (::e-flat ctx)))



(defn ptime

  "Returns either the `ptime` of the currently executing flat event (ie. under `[::e-flat ::ptime]`) or
   the global `ptime` in the `ctx` (ie. under `[::ptime]`).
  
  Returns either the ptime at `[::e-flat ::ptime]` (notably useful for [[finite]] or [[sampled-finite]]
   or, if there is none, at [::ptime]."

  [ctx]

  (or (::ptime (::e-flat ctx))
      (::ptime ctx)))



(defn ranks

  "Returns the ranks of the currently executing flat event (ie. under `[::e-flat ::ranks]`)."

  [ctx]

  (::ranks (::e-flat ctx)))



(?
 (defn rank+
 
   "Like [[ranks+]] but optimized for summing only the rank at position `n-rank` (defaulting
    to 0, the first one).

    Commonly used in the context of a [[ptime-engine]] where most of the time we only care about
    ptime (ie. the first rank).

    See [[ranks+]] for rationale.

    ```clojure
    (= [42]
       (rank+ {::e-flat {::ranks [30]}}
              12))
    ```"
 
   ([+rank]

    (rank+ 0
           +rank))


   ([n-rank +rank]

    (? (partial rank+
                n-rank
                +rank)))


   ([n-rank +rank ctx]

    (update (ranks ctx)
            n-rank
            +
            +rank))))



(?
 (defn ranks+
 
   "Returns the ranks of the currently executing flat event summed with the given ones.

    Not providing `ctx` returns a partially applied version.
 
    Some functions such as [[rel-conj]] or [[wq-delay]] take one as argument for scheduling
    something in the future at the same path that the currently executing flat event. Using [[rank+]]
    or [[ranks+]] comes in handy.

    Its sibling, singular [[rank+]], is optimized for summing only one rank.
 
    ```clojure
    (= [10 15 3]
       (ranks+ {::e-flat {::ranks [5 5]}}
               [5 10 3]))
 
    ```"
 
   ([+ranks]
    
    (? (partial ranks+
                +ranks)))


   ([+ranks ctx]

    (rktree/r+ (ranks ctx)
               +ranks))))



(defn reached?

  "Uses [[ptime]] to tell if a certain ptime has been reached."

  [ctx ptime-target]

  (>= (ptime ctx)
      ptime-target))



(defn scheduled?

  "Is there anything scheduled at all or for the given `ranks`?"

  ([ctx]

   (some? (::events ctx)))


  ([ctx ranks]

   (some? (rktree/get (::events ctx)
                      ranks))))


;;;;;;;;;; @[events]  Adding, removing, and modifying events


(defn- -throw-e-mod

  ;; For errors occuring when modifying the event tree.

  [ctx ranks path msg]

  (throw (ex-info msg
                  {::ctx   ctx
                   ::path  path
                   ::ranks ranks})))



(defn e-assoc

  "Schedules an `event`.

   Follows arity convention presented in the namespace description.
  
   | Arity | Means |
   |---|---|
   | 2 | Replaces the current working queue with the given `event`. |
   | 3 | Schedules the `event` in the event tree for the given `ranks` and path returned by [[path]]. |
   | 4 | Full control of when and where in the event tree. |
  
   It is bad practice to associate something such as an empty queue. It means that \"nothing\" is unnecessarily
   scheduled."

  ([ctx event]

   (assoc-in ctx
             [::e-flat
              ::queue]
             (if (queue? event)
               event
               (queue event))))


  ([ctx ranks event]

   (e-assoc ctx
            ranks
            (path ctx)
            event))


  ([ctx ranks path event]

   (update ctx
           ::events
           (fnil rktree/assoc
                 (rktree/tree))
           ranks
           path
           event)))



(defn e-conj

  "Enqueues an `event`, creating a queue if there is none.

   Follows arity convention presented in the namespace description (see also [[e-assoc]]).

   It is bad practice to conj something such as an empty queue. It means that \"nothing\" is unnecessarily
   scheduled."

  ;; Clojure's `conj` can take several values, but this is messing with our arities.

  ([ctx event]

   (e-update ctx
             (fn -e-conj [q]
               (conj q
                     event))))


  ([ctx ranks event]

   (e-conj ctx
           ranks
           (path ctx)
           event))


  ([ctx ranks path event]

   (e-update ctx
             ranks
             path
             (fn -e-conj [node]
               (cond
                 (nil? node)   (queue event)
                 (fn? node)    (queue node
                                      event)
                 (queue? node) (conj node
                                     event)
                 :else         (-throw-e-mod ctx
                                             ranks
                                             path
                                             "Can only `e-conj` to nil or an event"))))))



(defn e-dissoc
  
  "Cancels a scheduled event.

   Follows arity convention presented in the namespace description.

   | Arity | Means |
   |---|---|
   | 1 | Removes the currently executing flat event. |
   | 2 | In the event tree, removes the event located at `ranks` relative to the current path. |
   | 3 | In the event tree, remove the event located at `ranks` and `path`. |"

  ([ctx]

   (dissoc ctx
           ::e-flat))

  ([ctx ranks]

   (e-dissoc ctx
             ranks
             (path ctx)))


  ([ctx ranks path]

   (void/update ctx
                ::events
                (fn -e-dissoc [events]
                  (some-> events
                          (rktree/dissoc ranks
                                         path))))))



(defn e-into

  "Like [[e-conj]], but for a collection of `events`.

   Follows arity convention presented in the namespace description (see also [[e-assoc]]).
  
   Metadata of the given collection is merged with the already existing queue if there is one.

   It is bad practice to add empty events. A queue will be scheduled for nothing."
  
  ([ctx events]
   
   (e-update ctx
             (fn -e-into [q]
               (into (vary-meta q
                                merge
                                (meta events))
                     events))))


  ([ctx ranks events]

   (e-into ctx
           ranks
           (path ctx)
           events))


  ([ctx ranks path events]

   (e-update ctx
             ranks
             path
             (fn -e-into [node]
               (cond
                 (nil? node)   (if (queue? events)
                                 events
                                 (into (with-meta (queue)
                                                  (meta events))
                                       events))
                 (fn? node)    (into (with-meta (queue node)
                                                (meta events))
                                     events)
                 (queue? node) (into (vary-meta node
                                                merge
                                                (meta events))
                                     events)
                 :else         (-throw-e-mod ctx
                                             ranks
                                             path
                                             "Can only `e-into` to nil or an event"))))))



(defn e-get

  "Retrieves a scheduled event.
  
   Follows arity convention presented in the namespace description.

   | Arity | Means |
   |---|---|
   | 1 | Returns the current working queue. |
   | 3 | Returns the event located `path` and prioritized by `ranks` in the event tree. |
   | 4 | As above, but returns `not-found` is instead of nil when there is no event. |"

  ([ctx]

   (get-in ctx
		   [::e-flat
			::queue]))


  ([ctx ranks path]

   (e-get ctx
          ranks
          path
          nil))


  ([ctx ranks path not-found]

   (rktree/get (::events ctx)
               ranks
               path
               not-found)))



(defn e-isolate

  "Isolating means that the current working queue or the requested queue in the event tree
   will be nested in an outer queue.
  
   Follows arity convention presented in the namespace description."

  ([ctx]

   (e-update ctx
             (fn -e-isolate [q]
               (if (empty? q)
                 q
                 (queue q)))))


  ([ctx ranks]

   (e-isolate ctx
              ranks
              (path ctx)))


  ([ctx ranks path]

   (e-update ctx
             ranks
             path
             (fn -e-isolate [event]
               (some-> event
                       queue)))))



(defn e-push

  "Similar to [[e-into]] but works the other way around. Already scheduled events are added to the given
   queue `q` and their metadata data is merged.

   Follows arity convention presented in the namespace description."

  ([ctx q]

   (e-update ctx
             (fn -e-push [q-old]
               (into (vary-meta q
                                merge
                                (meta q-old))
                     q-old))))
           

  ([ctx ranks q]

   (e-push ctx
           ranks
           (path ctx)
           q))


  ([ctx ranks path q]

   (e-update ctx
             ranks
             path
             (fn -e-push [node]
               (cond
                 (nil? node)   q
                 (fn? node)    (into (with-meta (queue node)
                                                (meta q))
                                     q)
                 (queue? node) (into (vary-meta q
                                                merge
                                                (meta node))
                                     node)
                 :else         (-throw-e-mod ctx
                                             ranks
                                             path
                                             "Can only `e-push` to nil or an event"))))))



(defn e-update

  "Seldom used by the user, often used by other `e-XXX` functions.

   Follows arity convention presented in the namespace description.
  
   Works like standard `update` but tailored for the current working queue or the event tree.
   
   Returning nil will dissociate whatever is at that location."

  ([ctx f]

   (void/update-in ctx
                   [::e-flat
                    ::queue]
                   (fn safe-f [wq]
                     (when (nil? wq)
                       (throw (ex-info "No working queue at the moment"
                                       {::ctx ctx})))
                     (f wq))))


  ([ctx ranks f]

   (e-update ctx
             ranks
             (path ctx)
             f))

  ([ctx ranks path f]

   (void/update ctx
                ::events
                (fnil rktree/update
                      (rktree/tree))
                ranks
                path
                f)))



(?
 (defn mirror
 
   "Many discrete events and flows are typically interested in two things: the path they work on and the current
    ptime (ie. the first rank in their ranks, using a [[ptime-engine]]).
   
    Turns `f` into a regular event which accepts a `ctx`. Underneath, calls `(f ctx data-at-path current-ptime)`.
    The result is automatically associated in the `ctx` at the same path.
   
    Notably useful for [[finite]] and [[sampled-finite]], and many regular events for that matter."
 
   ([f]
 
    (? (partial mirror
                f)))
 
 
   ([f ctx]
 
    (let [current-path (path ctx)]
      (assoc-in ctx
                current-path
                (f ctx
                   (get-in ctx
                           current-path)
                   (ptime ctx)))))))



(defn rel-conj

  "Adds `event` in the event tree at the same path as the currently executing flat event, at some future ranks.

   Typically used for scheduling something in the future related to the current flat event.
  
   See [[ranks+]] for understaning `ctx->ranks`. If it returns nil, nothing is scheduled."

  [ctx ctx->ranks event]

  (if-some [ranks (ctx->ranks ctx)]
    (e-conj ctx
            ranks
            event)
    ranks))


;;;;;;;;;; @[ngin]  Building time-based event engines


(defn- -catched

  ;; Used by [[-exec-ef]] which is not recompilable at the REPL (because of reader conditional).

  [ctx q err]

  (let [ctx-2 (wq-dissoc ctx)]
    (or (void/call (::on-error (meta q))
                   {::ctx   ctx-2
                    ::queue q})
        (loop [stack (::stack (::e-flat ctx-2))]
          (if-some [[outer-ctx
                     outer-q]  (peek stack)]
            (or (void/call (::on-error (meta outer-q))
                           {::ctx         outer-ctx
                            ::ctx-inner   ctx-2
                            ::queue       outer-q
                            ::queue-inner q})
                (recur (pop stack)))
            (throw (ex-info "Unhandled exception while running context"
                            {::ctx   ctx-2
                             ::queue q}
                            err)))))))


(defn- -exec-ef

  ;; Executes a single event function.
  ;;
  ;; If it throws, it tries to find an error handler from the present queue or outers one
  ;; in the stack. If an error handler returns nil, meaning it does not know what to do,
  ;; the pursuit continues (cf. [[-catched]]).

  [ctx q eu]

  (try
    (eu ctx)
    (catch #?(:clj  Throwable 
              :cljs js/Error) err
      (-catched ctx
                q
                err))))



(defn- -exec-q

  ;; Executes the given non-empty event `q` containing either unit events or inner queues.
  ;;
  ;; For handling inner queues, instead of using named recursion, tail call recursion is used
  ;; by keeping a virtualized stack in ::e-flat. This has a few advantages, the most important
  ;; one being it becomes easy to stop execution, one simply needs to dissoc the stack.
  ;;
  ;; Empty queues are still added to the stack because they might have an error handler.

  [ctx q]

  (let [event    (peek q)
        q-popped (pop q)]
    (if (queue? event)
      (recur (update-in ctx
                        [::e-flat
                         ::stack]
                        (fnil conj
                              (list))
                        [(wq-dissoc ctx)
                         q-popped])
             event)
      (let [ctx-2 (-exec-ef (e-assoc ctx
                                     q-popped)
                            q-popped
                            event)
            q-2   (e-get ctx-2)]
        (if (empty? q-2)
          (if-some [[stack-2
                     q-outer] (loop [stack (::stack (::e-flat ctx-2))]
                                (when-some [[_ctx-outer
                                             q-outer]   (peek stack)]
                                  (let [stack-2 (pop stack)]
                                    (if (empty? q-outer)
                                      (recur stack-2)
                                      [stack-2
                                       q-outer]))))]
            (recur (update ctx-2
                           ::e-flat
                           void/assoc-strict
                           ::stack
                           (not-empty stack-2))
                   q-outer)
            ctx-2)
          (recur ctx-2
                 q-2))))))



(defn- -exec-e

  ;; Executes an event, which can be a function or a queue of events.
  ;;
  ;; Cf. [[engine*]]

  [ctx ranks path event]

  (cond
    (queue? event) (-exec-q (assoc ctx
                                   ::e-flat
                                   {::path  path
                                    ::ranks ranks})
                            event)
    :else          (let [ctx-2 (event (assoc ctx
                                             ::e-flat
                                             {::path  path
                                              ::queue (queue)
                                              ::ranks ranks}))
                         wq    (e-get ctx)]
                     (if (empty? wq)
                       ctx-2
                       (-exec-q ctx-2
                                wq)))))



(defn- -period-end

  ;; Cf. [[engine*]]

  [ctx]

  (dissoc ctx
          ::e-flat))



(defn engine*

  "Used for building rank-based event engines.
  
   Unless one is building something creative and/or evil, one should feel satisfied with either
   [[basic-engine]] or [[ptime-engine]]. Someone trully interested will study how [[ptime-engine]] is
   built before attempting to use this function for building a specific engine. Will only be useful
   for modelling a universe where time flows differently (multidimensional? some sort of time travel?)

   Returns map containing:

   ```clojure
   {::period-start (fn [ctx]
                     \"Prepares context for running\")
                     
    ::period-end   (fn [ctx]
                     \"Does some clean-up in the context\")
    ::run          (fn
                     ([ctx]
                      \"Pops and runs the next ranked event subtree, returns nil if there are not events\")
                     ([ctx events]
                      \"Ditto, but uses directly the given events (only useful for some optimizations)\"))}
   ```
   
   An engine, if it detects that events need to be processed, must call ::period-start. It can then call
   ::run one or several times, and when all needed events are processed defining some period, the engine
   must call ::period-end. For instance, [[ptime-engine]] considers a single point in time as a period
   during which events can be further ranked."

  []

  {::period-start identity
   ::period-end   -period-end
   ::run          (fn run 

                    ([ctx]
                     (when-some [events (::events ctx)]
                       (run ctx
                            events)))

                    ([ctx events]
                     (rktree/pop-walk events
                                      (fn update-events [events-2]
                                        (void/assoc-strict ctx
                                                           ::events
                                                           events-2))
                                      -exec-e)))})



(defn basic-engine

  "Returns a function `ctx -> ctx` which pops the next ranked events and executes them.

   If the said `ctx` does not have any event, returns nil."

  []

  (let [run*         (engine*)
        period-start (::period-start run*)
        period-end   (::period-end run*)
        run          (::run run*)]
    (fn run-2 [ctx]
      (when-some [events (::events ctx)]
        (-> ctx
            period-start
            (run events)
            (some-> period-end))))))



(defn ptime-engine

  "Like [[basic-engine]], but treats the first rank of events as a ptime (point in time).

   It is essentially a discrete-event simulation engine but serving a whole variety of other
   purposes as described in the README.

   At each run, it executes all events for the next ptime while ensuring that time move forwards.
   An event can schedule other events in the future or, at the earliest, for the same ptime. Throws
   at execution if time misbehaves.

   Current ptime is associated in the context at `::ptime` (see also [[ptime]]).
   
   `options` is a nilable map containing:
  
   | k | v |
   |---|---|
   | ::before | Function ctx -> ctx called right before the first event of the next ptime |
   | ::after | Function ctx -> ctx called after executing all events of a ptime. |

   If a ctx does not have any event, a run returns nil."

  ;; MAYBEDO
  ;;
  ;; ::after
  ;;
  ;; Cleaning up some state for a ptime just as ::e-flat is cleaned up after execution?
  ;; Would it be really useful to share some state between all events on a per ptime basis?
  ;; Or per ranks?
  ;; Probably not...

  ([]

   (ptime-engine nil))


  ([options]

   (let [run*         (engine*)
         period-start (::period-start run*)
         period-end   (::period-end run*)
         run          (::run run*)
         before       (or (::before options)
                          identity)
         before-2     (fn before-2 [ctx ptime]
                        (before (assoc ctx
                                       ::ptime
                                       ptime)))
         after        (or (::after options)
                          identity)
         after-2      (fn after-2 [ctx]
                        (-> ctx
                            period-end
                            after))]
     (fn run-ptime
       
       ([ctx]
        (when-some [events (::events ctx)]
          (let [ptime-next (ffirst events)]
            (if (some->> (::ptime ctx)
                         (<= ptime-next))
              (throw (ex-info "Ptime of events must be > ctx ptime"
                              {::ptime      ptime
                               ::ptime-next ptime-next}))
              (-> ctx
                  period-start
                  (void/assoc ::flows-dedup
                              (::flows ctx))
                  (before-2 ptime-next)
                  (run events)
                  (run-ptime ptime-next))))))

       ([ctx ptime]
        (let [events     (::events ctx)
              ptime-next (ffirst events)]
          (cond
            (or (nil? ptime-next)
                (> ptime-next
                   ptime))        (after-2 (dissoc ctx
                                                   ::flows-dedup))
            (= ptime-next
               ptime)             (recur (run ctx
                                              events)
                                         ptime)
            :else                 (throw (ex-info "Ptime of enqueued events is < current ptime"
                                                  {::ptime      ptime
                                                   ::ptime-next ptime-next})))))))))



(defn- -until

  ;; Helper for function artity 2 produced by [[historic]].

  [ctx ptime-max history]

  (let [next-ptime (next-ptime ctx)]
    (if (and next-ptime
             (<= next-ptime
                 ptime-max))
      (cons ctx
            (lazy-seq
              (-until (first history)
                      ptime-max
                      (rest history))))
      (list ctx))))



(defn historic

  "Given an engine, returns a function `ctx` -> lazy sequence of each run until all events
   are executed.
  
   For instance, given a [[ptime-engine]], each element in the sequence will be a view of
   the `ctx` at a specific ptime."

  [engine]

  (fn run-lazy
    
    ([ctx]
     (take-while some?
                 (rest (iterate engine
                                ctx))))

    ([ctx ptime-max]
     (let [history (run-lazy ctx)]
       (lazy-seq
         (let [ctx-first (first history)]
           (when (and ctx-first
                      (<= (::ptime ctx-first)
                          ptime-max))
             (-until ctx-first
                     ptime-max
                     (rest history)))))))))



(?
 (defn stop
 
   "Removes anything that is currently being executed (if any), all events and all flows,
    meaning there is nothing left to run."
 
   ([]

    stop)


   ([ctx]
 
    (-> ctx
        e-dissoc
        (dissoc ::events)
        (dissoc ::flows)))))


;;;;;;;;;; @[wq]  Relative to the currently executed queue (aka. the "working queue")
;;
;;
;; Manipulating the working queue, creating events to do so, or quering data about it.
;;
;;


(?
 (defn wq-capture
 
   "Captures and saves the rest of the working queue. Next call to [[wq-replay]] or [[wq-sreplay]]
    will replay or clean that captured queue.
   
    This is extremely useful for repeating queues or portion of queues. Without this abilty to capture
    the current state of a queue, it would be tricky to model activities or successions of flows that need
    some repetition.
 
    When called more than once, repetitions are nested. For instance:
 
    ```clojure
    (queue wq-capture
           event-a
           wq-capture
           event-b
           (wq-delay (rank+ 100))
           (wq-sreplay pred-repeat
                       1)
           event-c
           (wq-replay pred-repeat
                      1))
 
    ;; Equivalent to:
 
    (queue event-a
           event-b
           (wq-delay (rank+ 100))
           event-b
           event-c
           event-a
           event-b
           (wq-delay (rank+ 100))
           event-b
           event-c)
    ```"
   
   ([]
 
    wq-capture)
 
 
   ([ctx]
 
    (wq-vary-meta ctx
                  (fn capture [mta]
                    (-> mta
                        (update ::captured
                                (fn save-captured [captured]
                                  (conj (or captured
                                            (list))
                                        (with-meta (e-get ctx)
                                                   nil))))
                        (update ::sreplay
                                (fn state-slot [sreplay]
                                  (if sreplay
                                    (conj sreplay
                                          nil)
                                    (list nil))))))))))



(?
 (defn wq-delay
 
   "Moves the rest of the working queue to the computed ranks in the event tree.
 
    For instance, inducing a 500 time units delay between 2 events:
 
    ```clojure
    (queue event-a
           (wq-delay (rank+ 500))
           event-b)
    ```

    Particularly useful for modelling activities (sequences of events dispatched in time, using a [[ptime-engine]]).
    Knowing that several events logically bound together at the same path have to be scheduled at different ptimes, one
    approach would be to schedule all of them in one go, eargerly. However, that could quickly lead to the event tree
    becoming extremely big. More importantly, if an earlier event fails (eg. `event-a`), future ones (eg. `event-b`) are
    already scheduled and will execute.
 
    By using [[wq-delay]], both problems are solved. All events are part of the same queue, which makes sense, and
    delays reschedule the rest of the queue when needed. An event failing means the queue fails, so the activity stops.
 
    An example of an activity, a customer in a bank, assuming some sort of random delays being provided:
 
    ```
    (queue customer-arrives
           (wq-delay ...)
           customer-handled
           (wq-delay ...)
           customer-leaves)
    ``` 
   
    Unless something more sophisticated is needed, `ctx->ranks` will often be the result of [[rank+]] or
    [[ranks+]]. If `ctx->ranks` returns nil, no delay is incurred."
 
   ([ctx->ranks]
 
    (? (partial wq-delay
                ctx->ranks)))
 
 
   ([ctx->ranks ctx]
   
    (if-some [ranks (ctx->ranks ctx)]
      (wq-dissoc (e-conj ctx
                         ranks
                         (e-get ctx)))
      ctx))))



(defn wq-dissoc

  "Removes the current working queue, stopping effectively its execution."

  [ctx]

  (update ctx
          ::e-flat
          dissoc
          ::queue))



(? 
 (defn wq-do!

  "Calls `side-effect` with the `ctx` to do some side effect. Ignores the result and simply
   returns the unmodified `ctx`."

  ([side-effect]
   
   (partial wq-do!
            side-effect))


  ([side-effect ctx]

   (side-effect)
   ctx)))



(?
 (defn wq-exec
 
   "Executes the given event queue `q` in isolation from the rest of the current working queue.
   
    See also [[e-isolate]]."
 
   ([q]

    (? (partial wq-exec
                q)))
 
 
   ([q ctx]
 
    (let [wq (e-get ctx)]
      (e-assoc ctx
               (if (empty? wq)
                 q
                 (queue wq
                        q)))))))



(defn wq-meta

  "Returns the metadata of the current working queue.
  
   See also [[wq-vary-meta]]."

  [ctx]

  (meta (e-get ctx)))



(?
 (defn pred-repeat
 
   "Example of a predicate meant to be used with [[wq-sreplay]].
   
    The seed provided to [[wq-sreplay]], corresponding here to `n`, is the number of times a captured queue
    will be repeated. For instance, 2 means 3 occurences: the captured queue is first executed, then repeated
    twice.
   
    See [[wq-capture]] for an example."
 
   [_ctx n]
 
   (when (pos? n)
     (dec n))))



(defn- -replay-captured

  ;; Restores the queue that needs to be replayed.

  [ctx]

  (let [mta (wq-meta ctx)]
    (if-some [q (peek (::captured mta))]
      (e-assoc ctx
               (with-meta q
                          mta))
      (throw (ex-info "There is nothing captured to replay"
                      {::ctx ctx})))))



(?
 (defn wq-replay
 
   "When `pred?` returns true after being called with the current `ctx`, replays the last queue captured by 
    [[wq-capture]].
   
    When it returns a falsy value, that last captured queue is removed and the rest of the current working
    queue is executed."
 
   ([pred?]

    (? (partial wq-replay
                pred?)))
 
 
   ([pred? ctx]
 
    (if (pred? ctx)
      (-replay-captured ctx)
      (wq-vary-meta ctx
                    dsim.util/pop-stack
                    ::captured)))))



(?
 (defn wq-sreplay
 
   "Similar to [[wq-replay]] but `pred` is stateful. It is called with the `ctx` and (initially) the `seed`.
    Returning anything but nil is considered as truthy and is stored as state replacing `seed` in the next call.
 
    See [[wq-captured]] for an example with [[pred-repeat]]."
 
   ([pred seed]

    (? (partial wq-sreplay
                pred
                seed)))
 
 
   ([pred seed ctx]
 
    (let [pred-state (first (::sreplay (wq-meta ctx)))]
      (if-let [pred-state-2 (pred ctx
                                  (or pred-state
                                      seed))]
        (-> ctx
            (wq-vary-meta (fn save-state [mta]
                            (update mta
                                    ::sreplay
                                    (fn update-stack [sreplay]
                                      (conj (pop sreplay)
                                            pred-state-2)))))
            -replay-captured)
        (wq-vary-meta ctx
                      (fn clean-state [mta]
                        (-> mta
                            (dsim.util/pop-stack ::captured)
                            (dsim.util/pop-stack ::sreplay)))))))))



(defn wq-vary-meta

  "Uses Clojure's `vary-meta` on the working queue.
  
   When that queue is copied or moved into the future (eg. by calling [[wq-delay]]), it is a convenient way of storing
   some state at the level of a queue which can later be retrieved using [[wq-meta]]. When a queue is garbage-collected,
   so is its metadata."

  [ctx f & args]

  (update-in ctx
             [::e-flat
              ::queue]
             (fn update-meta [q]
               (with-meta q
                          (apply f
                                 (meta q)
                                 args)))))


;;;;;;;;;; @[flows]  Creating and managing flows


(defn end-flow

  "Is mainly used to end an [[infinite]] flow.

   Can also be used inside a [[finite]] or [[sampled-finite]] flow is it needs to end sooner than expected.
  
   When a flow ends, the rest of the queue that was saved when that flow was created will resume. This behavior
   makes it extremely easy to chain flows and events, simply add them to the same queue."
  
  [ctx]

  (let [current-path (path ctx)
        flow-path    (cons ::flows
                           current-path)
        flow-leaf    (get-in ctx
                             flow-path)
        ctx-2        (void/dissoc-in ctx
                                     flow-path)]
    (if-some [q (not-empty (::queue flow-leaf))]
      (assoc-in ctx-2
                [::e-flat
                 ::queue]
                q)
      ctx-2)))



(defn- -sample

  ;; Samples a specific flow.

  [ctx ptime path ranks-init flow]

  (let [ctx-2 (flow (assoc ctx
                           ::e-flat
                           {::path  path
                            ::ranks (assoc ranks-init
                                           0
                                           ptime)}))]
     (if-some [q (not-empty (e-get ctx-2))]
       (-exec-q ctx-2
                q)
       ctx-2)))



(defn- -sample-walk

  ;; Cf. [[sample]]
  ;;
  ;; Walks a node in the flow tree and samples every flow.

  [ctx ptime path node]

  (if-some [flow (::flow node)]
    (-sample ctx
             ptime
             path
             (::ranks-init node)
             flow)
     (reduce-kv (fn deeper [ctx-2 k node-next]
                  (-sample-walk ctx-2
                                ptime
                                (conj path
                                      k)
                                node-next))
                ctx
                node)))



(?
 (defn sample
 
   "When using a [[ptime-engine]], samples the flow associated with the current [[path]].

    Sampling is deduplicated on a per-ptime basis. For a given ptime, it is garanteed a flow will be sampled
    once and only once.
   
    This is especially important considering that a sample event can be scheduled for anywhere in the flow tree.
    For instance, when drawing a frame in an animation, it is way simpler to sample everything at once, scheduling
    a sample event at the root (without any path) rather than scheduling manually every single element that
    needs to be animated.

    Hence, a flow can be sampled at different rates by different interested parties without worrying about
    non-idempotency.
   
    See [[sampler]]."

   ([]
 
    sample)
 
 
   ([ctx]
 
    (let [current-path (path ctx)
          dedup-path   (cons ::flows-dedup
                             current-path)]
      (if (get-in ctx
                  dedup-path)
        (-sample-walk (void/dissoc-in ctx
                                      dedup-path)
                      (::ptime ctx)
                      current-path
                      (get-in ctx
                              (cons ::flows
                                    current-path)))
        ctx)))
 
 
   ([ctx ctx->ranks]
 
    (if-some [ranks (ctx->ranks ctx)]
      (e-conj ctx
              ranks
              (path ctx)
              sample)
      ctx))))



(defn- -f-assoc

  ;; Associates a new flow and everything it needs for resuming the queue later,
  ;; while sampling it for initialization.

  [ctx flow]

  (let [current-path  (path ctx)
        current-ranks (ranks ctx)]
   (-> ctx
       (assoc-in (cons ::flows
                       current-path)
                 {::flow       flow
                  ::ranks-init current-ranks
                  ::queue      (e-get ctx)})
       (-sample (::ptime ctx)
                current-path
                current-ranks
                flow)
       wq-dissoc)))



(?
 (defn infinite 
 
   "When using a [[ptime-engine]], a flow represent some continuous phenomenon, unlike an event which is
    modelled as being instantaneous and having virtually no duration.

    A flow is sampled, updated could we say, using a [[sample]] event.
 
    An \"infinite\" flow is either endless or ends at a moment that is not known in advance (eg. when the context
    satifies some condition not knowing when it will occur). It can be ended using [[end-flow]].
 
    Here is a simple example of an infinite flow that increments a value and schedules samples itself. In other
    words, that value will be incremented every 500 time units until it is randomly decided to stop. Then, the rest
    of the queue is resumed (`event-a` and `event-b` after a delay of 150 time units).
 
    ```clojure
    (dsim/queue (infinite (fn flow [ctx]
                            (let [ctx-2 (update-in ctx
                                                   (path ctx)
                                                   inc)]
                              (if (< (rand)
                                     0.1)
                                (end-flow ctx-2)
                                (rel-conj ctx-2
                                          (rank+ 500)
                                          sample)))))
                (wq-delay (rank+ 150))
                event-a
                event-b)
    ```
    
    Note that when a flow is created, it saves the rest of the working queue and will resume execution when it is done.
    This designs allows for simply building complex sequences of flows and events, including delays if needed (see
    [[wq-delay]]) and repetitions (see [[capture]]).
   
    When created, a flow is sampled right away for initialization. At each sample, a relative ptime is available
    under `[::e-flat ::ptime]]` (returned by [[ptime]]), starting with 0 at the ptime of creation.
   
    See also [[flow-path]].
    ```"
 
   ([flow]
 
    (? (partial infinite
                flow)))
 

   ([flow ctx]
 
    (-f-assoc ctx
              (let [start (::ptime ctx)]
                (fn relative-flow [ctx-2]
                  (flow (assoc-in ctx-2
                                  [::e-flat
                                   ::ptime]
                                  (- (::ptime ctx-2)
                                     start)))))))))



(defn- -finite

  ;; Cf. [[finite]] and [[sampled-finite]]

  [ctx duration flow after-sample]

  (let [start      (::ptime ctx)
        end        (+ start
                      duration)
        norm-ptime (partial minmax-norm
                            start
                            duration)]
    (-> ctx
        (-f-assoc (fn normalized-flow [ctx-2]
                    (let [e-ptime (norm-ptime (::ptime ctx-2))
                          ctx-3   (flow (assoc-in ctx-2
                                                  [::e-flat
                                                   ::ptime]
                                                  e-ptime))]
                      (if (>= e-ptime
                              1)
                        (end-flow (update ctx-3
                                          ::e-flat
                                          dissoc
                                          ::ptime))
                        (if after-sample
                          (after-sample ctx-3
                                        end)
                          ctx-3)))))
        (rel-conj (rank+ duration)
                  sample))))



(?
 (defn finite
 
   "Similar to [[infinite]]. However, the flow is meant to last as long as the given `duration`.
 
    Knowing the `duration` means [[end-flow]] will be called automatically after that interval of time. Also,
    before each sample, the ptime is linearly normalized to a value between 0 and 1 inclusive. In simpler terms,
    the value at `[::e-flat ::ptime]` (also returned by [[ptime]]) is the percentage of completion for that flow.
 
    Samples are automatically scheduled at creation for initialization and at the end for clean-up."
 
   ([duration flow]
 
    (? (partial finite
                duration
                flow)))
 
 
   ([duration flow ctx]
 
    (-finite ctx
             duration
             flow
             nil))))



(?
 (defn sampled-finite
 
   "Just like [[finite]] but eases the process of repeatedly sampling the flow.
   
    After each sample, starting at initialization, schedules another one using `ctx->ranks` (see the commonly
    used [[rank+]] and [[ranks+]]), maxing out the ptime at the ptime of completion so that the forseen interval will
    not be exceeded."
 
   ([ctx->ranks duration flow]
 
    (? (partial sampled-finite
                ctx->ranks
                duration
                flow)))
 
 
   ([ctx->ranks duration flow ctx]
  
    (-finite ctx
               duration
               flow
               (fn schedule-sampling [ctx-2 ptime-end]
                 (if-some [ranks (ctx->ranks ctx-2)]
                   (e-conj ctx-2
                           ranks
                           sample)
                   ctx-2))))))



(? 
 (defn- -sampler
 
   ;; Cf. [[sampler]]
 
   ([ctx->ranks path]

    (? (partial -sampler
                ctx->ranks
                path)))


   ([ctx->ranks path ctx]
 
    (let [ctx-2 (sample ctx)]
      (if-let [ranks (and (not (identical? ctx-2
                                           ctx))
                          (ctx->ranks ctx-2))]
        (e-conj ctx-2
                ranks
                path
                (-sampler ctx->ranks
                          path))
        ctx-2)))))



(?
 (defn sampler
 
   "A [[sampler]] event schedules a [[sample]] at future ranks computed by 'ctx->ranks'. It will
    continue to do so until there is nothing flowing anymore for that [[path]] or 'ctx->ranks' returns nil.

    It is commonly used for sampling repeatedly a subtree of flows rather than a specific flow. For
    instance, one could use a sampler for drawing every frame of an animation. Supposing a single ptime
    represents one millisecond and we draw at 60 frames per second:

    ```clojure
    (e-conj ctx
            [0 1000000]
            nil
            (sampler (rank+ (/ 1000
                               60))))
    ```

    Two things are interesting. First, besides scheduling for ptime 0, the sampler is scheduled for a
    secondary rank of 1000000, meaning all futures samples it performs, being rescheduled with that
    same secondary rank, will have a very low priority. Second, we did not provide a path, meaning we want
    sampling to occur for the whole flow tree (ie. all existing flows).

    Sometimes, specific flow samples are ordered using further ranks beyond the ptime. Had we scheduled that
    sampler without that secondary rank, all future samples would run with the highest priority within ptimes,
    sampling the whole tree at once, thus disrupting samples that were already ordered for those ptimes in
    a more specific order.
   
    Ensuring that this sampler executes last garantees ordering of all sample events. Remember that
    samples are deduplicated, meaning that when a sample ordered by the sampler executes, it will execute
    only flows that have not been sampled yet."
 
   ([ctx->ranks]
 
    (? (partial sampler
                ctx->ranks)))
 
 
   ([ctx->ranks ctx]
 
    (if-some [ranks (ctx->ranks ctx)]
      (let [current-path (path ctx)]
        (e-conj ctx
                ranks
                current-path
                (-sampler ctx->ranks
                          current-path)))
      ctx))))


;;;;;;;;;; @[fdat]  Serialization of whole contexts (events and flows included) via the `dvlopt.fdat` library


(def serializable

  "It can be particularly useful being able to serialize a `ctx` in order to save if to a file or sending
   it over the wire (saving the whole state of a game, saving long running simulations, sharing them, etc).
  
   However, how could one serialize all those event functions? The answer to that problem is an external library
   called `dvlopt/fdat`. The following functions of this API are indeed serializable using `dvlopt/fdat`:

   - finite
   - infinite
   - mirror
   - pred-repeat
   - rank+
   - ranks+
   - sample
   - sampled-finite
   - sampler
   - stop
   - wq-capture
   - wq-delay
   - wq-do!
   - wq-exec
   - wq-replay
   - wq-sreplay
  
   [https://github.com/dvlopt/fdat.cljc](https://github.com/dvlopt/fdat.cljc)"

  [-sampler
   finite
   infinite
   mirror
   pred-repeat
   rank+
   ranks+
   sample
   sampled-finite
   sampler
   stop
   wq-capture
   wq-delay
   wq-do!
   wq-exec
   wq-replay
   wq-sreplay])
