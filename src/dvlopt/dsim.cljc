(ns dvlopt.dsim

  "Idiomatic, purely-functional discrete event simulation and more.
  
   See README first in order to make sense of all this. It provides definitions and rationale for concepts."

  {:author "Adam Helinski"}

  (:require [dvlopt.dsim.ranktree :as dsim.ranktree]
            [dvlopt.dsim.util     :as dsim.util]
            [dvlopt.void          :as void])
  #?(:clj (:import (clojure.lang ExceptionInfo
                                 PersistentQueue))))




;;;;;;;;;; API structure (searchable for easy navigation)
;;
;; @[datastruct]  Data structures
;; @[misc]        Miscellaneous functions
;; @[scale]       Scaling numerical values
;; @[ctx]         Generalities about contextes
;; @[events]      Adding, removing, and modifying events
;; @[timevecs]    Handling timevecs
;; @[jump]        Moving a context through time
;; @[wq]          Relative to the currently executed queue (aka. the "working queue")
;; @[op]          Operation handling
;; @[flows]       Creating and managing flows




;;;;;;;;;; MAYBEDO


;;  Hack persistent queues so that they can also act as a stack?
;;  
;;  The front is implemented as a seq, so prepending is efficient, but the seq is
;;  package private. Relying on non-public features is not recommended. On the other
;;  hand, it is fairly certain the implementation will stay like this.


;;  Parallelize by timevec?
;;
;;  By definition, all events for a given timevec are independent, meaning that they
;;  can be parallelized without a doubt if needed.




;;;;;;;;;; Gathering all declarations


(declare e-update
         op-std
         path
         wq-vary-meta)




;;;;;;;;;; @[datastruct]  Data structures


(defn queue

  "Clojure has persistent queues but no easy way to create them.
  
   Here is one."

  ([]

   #?(:clj  PersistentQueue/EMPTY
      :cljs cljs.core/PersistentQueue.EMPTY))


  ([& values]

   (into (queue)
         values)))




(defn queue?

  "Is `x` a persistent queue?"

  [x]

  (instance? #?(:clj  PersistentQueue
                :cljs cljs.core/PersistentQueue)
             x))




;;;;;;;;;; @[misc]  Miscellaneous functions


(defn millis->utime

  "Converts an interval in milliseconds to an arbitrary time unit happening `hz` times per second.

   ```clojure
   ;; Coneverting milliseconds to frames for an animation.
   ;; We know something lasts 2000 milliseconds and the frame-rate is 60 times per second.
   ;; Hence, it lasts 120 frames.

   (millis->utime 2000
                  60)
   
   120
   ```"

  [millis hz]

  (long (Math/round (double (* (/ hz
                                  1000)
                               millis)))))





;;;;;;;;;; @[scale]  Scaling numerical values


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

   Arity 3: scales a `percent` value to be between `min-v` and (+ `min-v` `interval`) inclusive.

   Arity 5: scales `v`, between `min-v` and (+ `min-v` `interval`), to be between `scaled-min`v and
            (+ `scaled-min-v` `scaled-interval`) inclusive.

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

   See [[scale]] Arity 3, which is the opposite operation.
  
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


(defn f-path

  "Returns a new path locating the given one in the flow tree, as a sequence."

  ([]

   ::flows)


  ([path]

   (cons (f-path)
         path)))




(defn flowing?

  "Is the given context or some part of it currently in transition?"

  ([ctx]

   (flowing? ctx
             nil))


  ([ctx path]

   (not (empty? (get-in ctx
                        (cons ::flows
                              path))))))



(defn next-ptime

  "On what ptime is scheduled the next event, if there is one?"

  [ctx]

  (ffirst (::events ctx)))




(defn path

  "Returns the path associated at [::e-flat ::path]."

  [ctx]

  (::path (::e-flat ctx)))




(defn ptime

  "Returns either the ptime at [::e-flat ::ptime] (notably useful for [[f-finite]] or [[f-sampled]]
   or, if there is none, at [::ptime]."

  [ctx]

  (or (::ptime (::e-flat ctx))
      (::ptime ctx)))




(defn reached?

  "Uses [[ptime]] to tell if a certain ptime has been reached."

  [ctx ptime-target]

  (>= (ptime ctx)
      ptime-target))




(defn scheduled?

  "Is there anything scheduled at all or for a given timevec?"

  ([ctx]

   (not (empty? (get ctx
                     ::events))))


  ([ctx timevec]

   (not (empty? (get-in ctx
                        [::events
                         timevec])))))




(defn timevec

  "Returns the timevec at [::e-flat ::timevec]."

  [ctx]

  (::timevec (::e-flat ctx)))




;;;;;;;;;; @[events]  Adding, removing, and modifying events


(defn- -throw-e-mod

  ;; For errors occuring when modifying the event tree.

  [ctx timevec path msg]

  (throw (ex-info msg
                  {::ctx     ctx
                   ::timevec timevec
                   ::path    path})))









(defn- -e-valid

  ;; Safe guard, throws if an event is empty.

  [event]

  
  event)




(defn e-assoc

  "Schedule an `event`.
  

   Arity 2: Replaces the current working queue.
  
   Arity 3: Schedules the `event` in the event tree for the given `timevec` and path returned by [[path]].
  
   Arity 4: Full control of when and where in the event tree.
  

   Such arities are common when it comes to `e-XXX` functions, where providing providing both a `timevec`
   and a `path` refers explicitly to the event tree. Providing only the `timevec` also, but the `path` is
   retrieved in ::e-flat. Providing neither explicitely refers to the current working queue, hence will not work
   as intended when a queue is not being executed.

   It is bad practice to associate something such as an empty queue. It means that \"nothing\" is unnecessarily
   scheduled."

  ([ctx event]

   (assoc-in ctx
             [::e-flat
              ::queue]
             (if (queue? event)
               event
               (queue event))))


  ([ctx timevec event]

   (e-assoc ctx
            timevec
            (path ctx)
            event))


  ([ctx timevec path event]

   (update ctx
           ::events
           (fnil dsim.ranktree/assoc
                 (dsim.ranktree/tree))
           timevec
           path
           event)))




(defn e-conj

  "Enqueues an `event`.

   Arities follow the same convention as [[e-assoc]].

   It is bad practice to conj something such as an empty queue. It means that \"nothing\" is unnecessarily
   scheduled."

  ;; Clojure's `conj` can take several values, but this is messing with our arities.

  ([ctx event]

   (e-update ctx
             (fn -e-conj [q]
               (conj q
                     event))))


  ([ctx timevec event]

   (e-conj ctx
           timevec
           (path ctx)
           event))


  ([ctx timevec path event]

   (e-update ctx
             timevec
             path
             (fn -e-conj [node]
               (cond
                 (nil? node)   (queue event)
                 (fn? node)    (queue node
                                      event)
                 (queue? node) (conj node
                                     event)
                 :else         (-throw-e-mod ctx
                                             timevec
                                             path
                                             "Can only `e-conj` to nil or an event"))))))




(defn e-dissoc
  
  "Cancel a scheduled event.
  
   Arity 1: Removes the current working queue.
  
   Arity 3: Remove the event located at `timevec` and `path` in the event tree."

  ([ctx]

   (update ctx
           ::e-flat
           dissoc
           ::queue))


  ([ctx timevec path]

   (void/update ctx
                ::events
                (fn -e-dissoc [events]
                  (some-> events
                          (dsim.ranktree/dissoc timevec
                                                path))))))




(defn e-into

  "Like [[e-conj]], but for a collection of `events`.
  
   Metadata of the given collection is merged with the already existing queue if there is one.

   It is bad practice to add empty events. A queue will be scheduled for nothing."
  
  ([ctx events]
   
   (e-update ctx
             (fn -e-into [q]
               (into (vary-meta q
                                merge
                                (meta events))
                     events))))


  ([ctx timevec events]

   (e-into ctx
           timevec
           (path ctx)
           events))


  ([ctx timevec path events]

   (e-update ctx
             timevec
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
                                             timevec
                                             path
                                             "Can only `e-into` to nil or an event"))))))




(defn e-get

  "Retrieves a scheduled event.
  
   Arity 1: Returns the current working queue.

   Arity 3: Returns the event located at `timevec` and `path` in the event tree."

  ([ctx]

   (e-get ctx
          nil))


  ([ctx not-found]

   (get-in ctx
           [::e-flat
            ::queue]
           not-found))


  ([ctx timevec path]

   (e-get ctx
          timevec
          path
          nil))


  ([ctx timevec path not-found]

   (dsim.ranktree/get (::events ctx)
                      timevec
                      path
                      not-found)))




(defn e-isolate

  "Isolating means that the current working queue or the requested queue in the event tree
   will be nested in an outer queue.
  
   Arities follow similar convention as [[e-assoc]]."

  ([ctx]

   (e-update ctx
             (fn -e-isolate [q]
               (if (empty? q)
                 q
                 (queue q)))))


  ([ctx timevec]

   (e-isolate ctx
              timevec
              (path ctx)))


  ([ctx timevec path]

   (e-update ctx
             timevec
             path
             (fn -e-isolate [event]
               (some-> event
                       queue)))))




(defn e-push

  "Similar to [[e-into]] but works the other way around. Already scheduled events are added to the given
   queue `q` and their metadata data is merged."

  ([ctx q]

   (e-update ctx
             (fn -e-push [q-old]
               (into (vary-meta q
                                merge
                                (meta q-old))
                     q-old))))
           

  ([ctx timevec q]

   (e-push ctx
           timevec
           (path ctx)
           q))


  ([ctx timevec path q]

   (e-update ctx
             timevec
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
                                             timevec
                                             path
                                             "Can only `e-push` to nil or an event"))))))




(defn e-update

  "Seldom used by the user, often used by other `e-XXX` functions.
  
   Works like standard `update` but tailored for the current working queue or the event tree.
  
   Arities follow similar convention as [[e-assoc]].
  
   "

  ([ctx f]

   (void/update-in ctx
                   [::e-flat
                    ::queue]
                   (fn safe-f [wq]
                     (when (nil? wq)
                       (throw (ex-info "No working queue at the moment"
                                       {::ctx ctx})))
                     (f wq))))


  ([ctx timevec f]

   (e-update ctx
             timevec
             (path ctx)
             f))

  ([ctx timevec path f]

   (void/update ctx
                ::events
                (fnil dsim.ranktree/update
                      (dsim.ranktree/tree))
                timevec
                path
                f)))




;;;;;;;;;; @[timevecs]  Handling timevecs


(defn timevec+

  "Adds all dimension in `dtimevec` to `timevec`
  
   The first dimension in `dtimevec` denoting a ptime cannot be negative as one cannot travel
   back in time.
  
   ```clojure
   (timevec+ [0 5]
             [10 10 10])

   [10 15 10]
   ```"

  [timevec dtimevec]

  (when (some-> (first dtimevec)
                neg?)
    (throw (ex-info "Cannot add negative time interval to timevec"
                   {::dtimevec dtimevec
                    ::timevec  timevec})))
  (dsim.ranktree/r+ timevec
                    dtimevec))




(defn wq-timevec+

  "Like [[timevec+]] but fetches the timevec from the `ctx`.
  
   Not providing the `ctx` returns a function ctx -> timevec (often useful for function
   such as [[wq-delay]]."

  ([dtimevec]

   (fn ctx->timevec [ctx]
     (wq-timevec+ ctx
                  dtimevec)))


  ([ctx dtimevec]

   (timevec+ (timevec ctx)
             dtimevec)))




;;;;;;;;;; @[jump]  Moving a context through time


(defn- -exec-q

  ;; Executes the given event `q`.
  ;;
  ;; Fugly, but works.
  ;;
  ;; MAYBDO. Try-catch at the level of every single event unit.
  ;;         Less efficient than at the queue level, but allow for tracking the context down
  ;;         to the last known state. Is it useful though?


  ([e-handler ctx q]

   (-exec-q e-handler
            ctx
            q
            identity))


  ([e-handler ctx q after-q]

   (let [event  (peek q)
         q-2    (pop q)
         [ctx-2
          q-3]  (try
                  (if (queue? event)
                    [(-exec-q e-handler
                              ctx
                              event
                              (fn restore-outer [ctx]
                                (assoc-in ctx
                                          [::e-flat
                                           ::queue]
                                          q-2)))
                     q-2]
                    (let [ctx-2 (e-handler (assoc-in ctx
                                                     [::e-flat
                                                      ::queue]
                                                     q-2)
                                           event)]
                      [ctx-2
                       (e-get ctx-2)]))
                  
                  (catch ExceptionInfo err
                    (let [err-data (ex-data err)]
                      (if-some [on-error (::on-error (meta q-2))]
                        (let [ctx-2 (e-handler (void/assoc {::ctx   ctx
                                                            ::error err}
                                                           ::ctx-inner (::ctx err-data))
                                               on-error)]
                          [ctx-2
                           (e-get ctx-2)])
                        (throw (if (contains? err-data
                                              ::ctx)
                                 err
                                 (ex-info (ex-message err)
                                          (assoc err-data
                                                 ::ctx
                                                 (e-dissoc ctx))
                                          (ex-cause err)))))))
                  (catch #?(:clj  Throwable
                            :cljs js/Error)
                         e
                    (if-some [on-error (::on-error (meta q-2))]
                      (let [ctx-2 (e-handler {:ctx   ctx
                                              :error e}
                                             e-handler)]
                        [ctx-2
                         (e-get ctx-2)])
                      (throw (ex-info "Throwing in the last computed context"
                                      {::ctx (e-dissoc ctx)}
                                      e)))))]
     (if (empty? q-3)
       (after-q ctx-2)
       (recur e-handler
              ctx-2
              q-3
              after-q)))))




(defn- -exec-e

  ;; Executes an event.
  ;;
  ;; Cf. [[engine*]]

  [e-handler ctx ranks path event]

  (if (queue? event)
     (-exec-q e-handler
              (assoc ctx
                     ::e-flat
                     {::path    path
                      ::timevec ranks})
              event)
     (let [ctx-2 (e-handler (assoc ctx
                                   ::e-flat
                                   {::path    path
                                    ::queue   (queue)
                                    ::timevec ranks})
                            event)
           wq    (e-get ctx)]
       (if (empty? wq)
         ctx-2
         (-exec-q e-handler
                  ctx-2
                  wq)))))



(defn- -period-end

  ;; Cf. [[engine*]]

  [ctx]

  (-> ctx
      (dissoc ::e-flat)
      (vary-meta (fn clean-e-handler [mta]
                   (not-empty (dissoc mta
                                      ::e-handler))))))



(defn engine*

  "Used for building rank-based event engines.
  
   Unless one is building something creative and/or evil, one should feel satisfied with either
   [[engine]] or [[engine-ptime]]. Someone trully interested will study how [[engine-ptime]] is
   built before attempting to use this function.
  
   For options, see [[engine]].

   Returns map containing:

   ```clojure
   {::period-start (fn [ctx]
                     \"Prepares context for running\")
                     
    ::period-end   (fn [ctx]
                     \"Does so clean-up in the context\")
    ::run          (fn
                     ([ctx]
                      \"Pops and run the next ranked event subtree\")
                     ([ctx events]
                      \"Ditto, but uses directly the given events (useful for some optimizations)\"))}
   ```
   
   An engine, if it detects that events need to be processed, must call ::period-start. It can then call
   ::run one or several times, and when all needed events are processed, the engine must call ::period-end."

  ;; The event handler provided by the user (typically the result of [[op-applier]], if any, needs
  ;; to figure in the ctx metadata. Everytime a ctx is returned to the user after some period, it must
  ;; be removed.
  ;;
  ;; The main purpose of event handler is for the ctx to be serializable. Hence, keeping the event handler
  ;; in the metadata defeats that purpose.

  ([]

   (engine* nil))


  ([options]

   (let [e-handler (or (::e-handler options)
                       (fn ef-handler [ctx f]
                         (f ctx)))]
     {::period-start (fn period-start [ctx]
                       (vary-meta ctx
                                  assoc
                                  ::e-handler
                                  e-handler))
      ::period-end   -period-end
      ::run          (fn run 

                       ([ctx]
                        (when-some [events (::events ctx)]
                          (run ctx
                               events)))

                       ([ctx events]
                        (dsim.ranktree/pop-walk ctx
                                                events
                                                (fn reattach-tree [ctx events-2]
                                                  (void/assoc-strict ctx
                                                                     ::events
                                                                     events-2))
                                                (partial -exec-e
                                                         e-handler))))})))




(defn engine

  "Returns a function ctx -> ctx which pops the next ranked events and executes them.

   If the said ctx does not have any event, returns nil.
  
   `options` is a nilable map containing:
  
   | k | v |
   |---|---|
   | ::e-handler | Event handler, only needed if events are data (see [[op-applier]]). |"

  ([]

   (engine nil))


  ([options]

   (let [run*         (engine* options)
         period-start (::period-start run*)
         period-end   (::period-end run*)
         run          (::run run*)]
     (fn run-2 [ctx]
       (when-some [events (::events ctx)]
         (-> ctx
             period-start
             (run events)
             (some-> period-end)))))))




(defn engine-ptime

  "Like [[engine]], but treats the first rank of event as a ptime (point in time).

   At each run, it executes all events for the next ptime while ensuring that time move forwards.
   An event can schedule other events in the future or, at the earliest, for the same ptime.

   Current ptime is associated in the context at ::ptime (see also [[ptime]]).
   
   `options` is a nilable map containing:
  
   | k | v |
   |---|---|
   | ::before | Function ctx -> ctx called right before the first event of the next ptime |
   | ::after | Functoion ctx -> ctx called after executing all events of a ptime. |
   | ::e-handler | See [[engine]]. |"

   
  ;; MAYBEDO. ::after
  ;;          Cleaning up some state for a ptime just as ::e-flat is cleaned up after execution?
  ;;          Would it be really useful to share some state between all events on a per ptime basis?
  ;;          Or per timevec?
  ;;          Probably not...


  ([]

   (engine-ptime nil))


  ([options]

   (let [run*         (engine* options)
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
                  (before-2 ptime-next)
                  (run events)
                  (run-ptime ptime-next))))))

       ([ctx ptime]
        (if-some [events (::events ctx)]
          (let [ptime-next (ffirst events)]
            (cond
              (> ptime-next
                 ptime)     (after-2 ctx)
              (= ptime-next
                 ptime)     (recur (run ctx
                                        events)
                                   ptime)
              :else         (throw (ex-info "Ptime of enqueued events is < current ptime"
                                            {::ptime      ptime
                                             ::ptime-next ptime-next}))))
          (after-2 ctx)))))))




(defn historic

  "Given an engine, returns a function ctx -> lazy sequence of each run until all events
   are executed.
  
   For instance, given an [[engine-ptime]], each element in the sequence will be a ptime."

  [engine]

  (fn run-lazy [ctx]
    (take-while some?
                (rest (iterate engine
                               ctx)))))




;;;;;;;;;; @[wq]  Relative to the currently executed queue (aka. the "working queue")
;;
;;
;; Manipulating the working queue, creating events to do so, or quering data about it.
;;
;;
;; Arities are redundant but more user-friendly and API-consistent than partial application.
;; Let us not be too smart by imagining some evil macro.
;;


(defn wq-breaker

  "Removes the working queue if `pred?`, called with the current `ctx`, returns true."

  ([pred?]

   (fn event [ctx]
     (wq-breaker ctx
                 pred?)))



  ([ctx pred?]

   (if (pred? ctx)
     ctx
     (e-dissoc ctx))))




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
          (wq-delay (wq-timevec+ [100]))
          (wq-sreplay wq-pred-repeat
                      1)
          event-c
          (wq-replay wq-pred-repeat
                     1))

   ;; Equivalent to:

   (queue event-a
          event-b
          (wq-delay (wq-timevec+ [100]))
          event-b
          event-c
          event-a
          event-b
          (wq-delay (wq-timevec+ [100]))
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
                                   (list nil)))))))))




(defn wq-copy

  "Copies the current working queue to a future timevec in the event tree.
  
   Because this is Clojure, the queue is not actually copied as it is immutable.

   Unless something more sophisticated is needed, `ctx->timevec` will often be the result of [[wq-timevec+]].
   For instance, scheduling for 500 time units from now."

  ([ctx->timevec]

   (fn event [ctx]
     (wq-copy ctx
               ctx->timevec)))


  ([ctx ctx->timevec]

   (e-conj ctx
           (ctx->timevec ctx)
           (e-get ctx))))




(defn wq-delay

  "Moves the rest of the working queue to a future timevec in the event tree.
  
   See also [[wq-copy]]."

  ([ctx->timevec]

   (fn event [ctx]
     (wq-delay ctx
                ctx->timevec)))


  ([ctx ctx->timevec]
  
   (e-dissoc (wq-copy ctx
                      ctx->timevec))))




(defn wq-do!

  "Calls `side-effect` with the `ctx` to do some side effect. Ignores the result and simply
   returns the unmodified `ctx`."

  ([side-effect]

   (fn event [ctx]
     (wq-do! ctx
             side-effect)))


  ([ctx side-effect]

   (side-effect)
   ctx))




(defn wq-exec

  "Executes the given event queue `q` in isolation from the rest of the working queue.
  
   See also [[e-isolate]]."

  ([q]

   (fn event [ctx]
     (wq-exec ctx
              q)))


  ([ctx q]

   (let [wq (e-get ctx)]
     (e-assoc ctx
              (if (empty? wq)
                q
                (queue wq
                       q))))))




(defn wq-meta

  "Returns the metadata of the working queue.
  
   See also [[wq-vary-meta]]."

  [ctx]

  (meta (e-get ctx)))




(defn wq-mirror

  "Many discrete events are typically interested in two things: the path they work on and the current ptime.
  
   Turns `f` into a regular event which accepts a `ctx`. Underneath, calls (f ctx data-at-path current-ptime).
   The result is automatically associated in the `ctx` at the same path."

  ([f]

   (fn event-2 [ctx]
     (wq-mirror ctx
                f)))


  ([ctx f]

   (let [path' (path ctx)]
     (assoc-in ctx
               path'
               (f (get-in ctx
                          path')
                  (ptime ctx))))))




(defn wq-pred-repeat

  "Example of a predicate meant to be used with [[wq-sreplay]].
  
   The seed provided to [[wq-sreplay]], corresponding here to `n`, is the number of times a captured queue
   will be repeated. For instance, 2 means 3 occurences: the captured queue is first executed, then repeated
   twice.
  
   See [[wq-capture]] for an example."

  [_ctx n]

  (when (pos? n)
    (dec n)))




(defn- -replay-captured

  ;; Restore the queue that needs to be replayed.

  [ctx]

  (let [mta (wq-meta ctx)]
    (if-some [q (peek (::captured mta))]
      (e-assoc ctx
               (with-meta q
                          mta))
      (throw (ex-info "There is nothing captured to replay"
                      {::ctx ctx})))))




(defn wq-replay

  "When `pred?` returns true after being called with the current `ctx`, replays the last queue captured using
   [[wq-capture]].
  
   When it returns a falsy value, that last captured queue is removed."

  ([pred?]

   (fn event [ctx]
     (wq-replay ctx
                pred?)))


  ([ctx pred?]

   (if (pred? ctx)
     (-replay-captured ctx)
     (wq-vary-meta ctx
                   (fn release-captured [mta]
                     (dsim.util/pop-stack mta
                                          ::captured))))))




(defn wq-sreplay

  "Similar to [[wq-replay]] but `pred` is stateful. It is called with the `ctx` and (initially) the `seed`.
   Returning anything but nil is considered as truthy and is stored as state replacing `seed` in the next call.

   See [[wq-captured]] for an example with [[wq-pred-repeat]]."

  ([pred seed]

   (fn event [ctx]
     (wq-sreplay ctx
                 pred
                 seed)))


  ([ctx pred seed]

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
                           (dsim.util/pop-stack ::sreplay))))))))




(defn wq-vary-meta

  "Uses Clojure's `vary-meta` on the working queue. (Unlike, does not accept variadic arguments to apply).
  
   When that queue is copied or moved into the future (eg. by calling [[wq-delay]]), it is a convenient way of storing
   some state at the level of a queue which can later be retrieved using [[wq-meta]]. When a queue is garbage collected,
   so is its metadata."

  ([f]

   (fn event [ctx]
     (wq-vary-meta ctx
                   f)))


  ([ctx f & args]

   (update-in ctx
              [::e-flat
               ::queue]
              vary-meta
              f)))




;;;;;;;;;; @[op]  Operation handling


(defn op-applier

  "Prepares a function that can be used as ::e-handler (see [[engine]], [[engine-time]]).
  
   The purpose is to represent unit events as data instead of functions so that a context is fully
   serializable when needed.

   A data representation of an event, called an `operation`, has the following format:

   ```clojure
   [:some-keyword & args]
   ```

   `k->f` is a map keyword -> event function. The resulting event handler will use it to find the appropriate
   event function and apply to it arguments from the `operation`.

   See [[op-std]] for a map of event function automatically injected."

  [k->f]

  (let [k->f-2 (merge k->f
                      op-std)]
    (fn e-handler
      
      ([k]

       (or (get k->f-2
                k)
           (throw (ex-info "Function not found for operation"
                           {::op-k k}))))

      ([ctx [k & args :as op]]

       (apply (e-handler k)
              ctx
              args)))))




(defn op-exec

  "During a jump (see [[jump-until]] or [[history]]), can be called within an event in order the execute
   the given `op`.
  
   See also [[op-applier]]."

  [ctx op]

  (if-some [e-handler (::e-handler (meta ctx))]
    (e-handler ctx
               op)
    (throw (ex-info "No operation handler has been provided"
                    {::ctx ctx}))))




(def op-std

  "Map of keyword -> event function automatically injected when calling [[op-applier]].

   It contains the following useful `wq-XXX` functions:

   ```clojure
   :dvlopt.dsim/breaker
   :dvlopt.dsim/capture
   :dvlopt.dsim/delay
   :dvlopt.dsim/do!
   :dvlopt.dsim/exec
   :dvlopt.dsim/mirror
   :dvlopt.dsim/pred-repeat
   :dvlopt.dsim/replay
   :dvlopt.dsim/sreplay
   :dvlopt.dsim/timevec+
   ```

   There are meant to mimick normal function calls. For instance, assuming ::event-a and
   ::event-b have been provided to `op-applier`:

   ```clojure
   (queue [:dvlopt.dsim/capture]
          [::event-a
          [:dvlopt.dsim/delay [:dvlopt.dsim/timevec+ [100]]]
          [::event-b 42 :some-arg]
          [:dvlopt.dsim/sreplay [:dvlopt.dsim/pred-repeat]
                                2])

   ;; Is the data equivalent of (assuming `event-b` encapsulates args in a closure):

   (queue wq-capture
          event-a
          (wq-delay (wq-timevec+ [100]))
          event-b
          (wq-sreplay wq-pred-repeat
                      2))
   ```"

  {::breaker     (fn event [ctx op-pred?]
                   (wq-breaker ctx
                               (fn pred? [ctx]
                                 (op-exec ctx
                                          op-pred?))))
   ::capture     wq-capture
   ::delay       (fn event [ctx op-ctx->timevec]
                   (wq-delay ctx
                             (fn ctx->timevec [ctx]
                               (op-exec ctx
                                        op-ctx->timevec))))
   ::do!         (fn event [ctx op-side-effect]
                   (wq-do! ctx
                           (fn side-effect-2 [ctx]
                             (op-exec ctx
                                      op-side-effect))))
   ::exec        wq-exec
   ::mirror      (fn event [ctx op-mirror]
                   (wq-mirror ctx
                              (fn mirror [data ptime]
                                (op-exec data
                                         (conj op-mirror
                                               ptime)))))
   ::pred-repeat wq-pred-repeat
   ::replay      (fn event [ctx op-pred?]
                   (wq-replay ctx
                              (fn pred? [ctx]
                                (op-exec ctx
                                         op-pred?))))
   ::sreplay     (fn event [ctx op-pred seed]
                   (wq-sreplay ctx
                               (fn pred [ctx state]
                                 (op-exec ctx
                                          (conj op-pred
                                                state)))
                               seed))
   ::timevec+    wq-timevec+})




;;;;;;;;;; @[flows]  Creating and managing flows


(def rank-flows

  "This rank is automatically inserted when a sample is scheduled.
  
   See also [[f-sample]]."

  (long 1e9))




(defn f-end

  "Is mainly used to end an an infinite flow (see [[f-infinite]]).

   Can also be used inside a finite flow is it needs to end sooner than expected (see [[f-finite]] and [[f-sampled]]).
  
   Resumes the execution of the rest of the queue when the flow was created.
  
   See [[f-infinite]] for an example."

  [ctx]

  (let [flow-path (f-path (path ctx))
        flow-leaf (get-in ctx
                          flow-path)
        ctx-2     (void/dissoc-in ctx
                                  flow-path)]
    (if-some [q (not-empty (::queue flow-leaf))]
      (-exec-q (::e-handler (meta ctx-2))
               (assoc-in ctx-2
                         [::e-flat
                          ::timevec]
                         (assoc (::timevec-init flow-leaf)
                                0
                                (first (timevec ctx-2))))
               q)
      ctx-2)))




(defn- -f-sample*

  ;; Cf. [[f-sample*]]

  [ctx ptime path node]

  (or (void/call (::flow node)
                 (assoc ctx
                        ::e-flat
                        {::path    path
                         ::timevec (assoc (::timevec-init node)
                                          0
                                          ptime)}))
      (reduce-kv (fn deeper [ctx-2 k node-next]
                   (-f-sample* ctx
                               ptime
                               (conj path
                                     k)
                               node-next))
                 ctx
                 node)))




(defn f-sample*

  "Kept public for extreme use cases. For the vast majority, users should rely on [[f-sample]].
  
   Directly samples all flows or a subtree at the given `path`."

  ([ctx]

   (f-sample* ctx
              (path ctx)))


  ([ctx path]

   (-f-sample* (e-dissoc ctx)
               (first (timevec ctx))
               path
               (get-in ctx
                       (f-path path)))))




(defn f-sample

  "Schedules a sample, now or at the given `timevec`, at the `path` of the current flat event or the given one.

   The given `path` need not to point to a specific flow, it can point to a subtree which will then be walked to
   find all flows. This is more efficient than scheduling a sample for all flows individually. For instance, when
   drawing an animation frame, one can provide an empty path, which is indeed more efficient and easier than
   scheduling every single element each frame.
  
   The way it works garantees deduplication and that is why it should be used instead of [[f-sample*]]. Without
   deduplication, flows might be sampled more than once for a given timevec which not only is inefficient, it corrupts
   data if some flows are not idempotent. Scheduling a sample using this function is always idempotent.

   See also [[f-infinite]]."

  ([]

   (fn event [ctx]
     (f-sample ctx)))


  ([ctx]

   (f-sample ctx
             (timevec ctx)))


  ([ctx timevec]

   (f-sample ctx
             timevec
             (path ctx)))


  ([ctx timevec path]

   (update ctx
           ::events
           (fnil dsim.ranktree/update
                 (dsim.ranktree/tree))
           (into [(first timevec)
                  rank-flows]
                 (rest timevec))
           (fn deduplicate-sampling [node]
             (dsim.util/assoc-shortest node
                                       path
                                       f-sample*)))))




(defn- -f-assoc

  ;; Associates a new flow and everything it needs for resuming the queue later.

  ([ctx flow]

   (-f-assoc ctx
             flow
             nil))


  ([ctx flow hmap]

   (-> ctx
       (assoc-in (f-path (path ctx))
                 (merge hmap
                        {::flow         flow
                         ::timevec-init (timevec ctx)
                         ::queue        (e-get ctx)}))
       e-dissoc)))




(defn f-infinite 

  "A flow is akin to an event. While events happen at one particular timevec and have no concept of duration,
   flows last for an interval of time. They are sampled when needed, decided by the user, by using [[f-sample]].

   An \"infinite\" flow is either endless or ends at a moment that is not known in advance (eg. when the context
   satifies some condition not knowing when it will occur). It can be ended using `f-end`.

   Here is a simple example of an infinite flow that increments a value and schedules samples itself. In other
   words, that value will be incremented every 500 time units until it is randomly decided to stop:

   ```clojure
   (dsim/queue (f-infinite (fn flow [ctx]
                             (let [ctx-2 (update-in ctx
                                                    (path ctx)
                                                    inc)]
                               (if (< (rand)
                                      0.1)
                                 (f-end ctx-2)
                                 (f-sample ctx-2
                                           (wq-timevec+ ctx-2
                                                        [500]))))))
               (wq-delay (wq-timevec+ [150]))
               event-a
               event-b)
   ```
   
   Note that when a flow is created, it saves the rest of the working queue and will resume execution when it is done.
   This designs allows for simply building complex sequences of flows and events, including delays if needed (see
   [[wq-delay]]) and repetitions (see [[capture]]).i
  
   When created, a flow is automatically sampled at the same ptime for initialization."

  ([flow]

   (fn event [ctx]
     (f-infinite ctx
                 flow)))


  ([ctx flow]

   (-> ctx
       (-f-assoc flow)
       f-sample)))




(defn- -f-finite

  ;; Cf. [[f-finite]]
  ;;
  ;; Todo. void, assoc-some

  [ctx after-sample duration flow]

  (let [ptime       (::ptime ctx)
        ptime-end   (+ ptime
                       duration)
        norm-ptime  (partial minmax-norm
                             ptime
                             duration)]
    (-> ctx
        (-f-assoc (fn norm-flow [ctx]
                    (let [e-ptime (norm-ptime (::ptime ctx))
                          ctx-2   (flow (assoc-in ctx
                                                  [::e-flat
                                                   ::ptime]
                                                  e-ptime))]
                      (if (>= e-ptime
                              1)
                        (f-end (update ctx-2
                                       ::e-flat
                                       dissoc
                                       ::ptime))
                        (after-sample ctx-2
                                      ptime-end)))))
        f-sample 
        (f-sample (update (timevec ctx)
                          0
                          +
                          duration)))))




(defn f-finite

  "Similar to [[f-infinite]]. However, the flow is meant to last as long as the given `duration`.

   Knowing the `duration` means [[f-end]] will be called automatically after that interval of time. Also,
   before each sample, the ptime is linearly normalized to a value between 0 and 1 inclusive. In simpler terms,
   the value at [::e-flat ::ptime] (also returned by [[ptime]]) is the percentage of completion for that flow.

   Samples are automatically scheduled at creation for initialization and at the end for clean-up."

  ([duration flow]

   (fn event [ctx]
     (f-finite ctx
               duration
               flow)))


  ([ctx flow duration]

   (-f-finite ctx
              identity
              duration
              flow)))




(defn f-sampled

  "Just like [[f-finite]] but eases the process of repeatedly sampling the flow.
  
   After each sample, starting at initialization, schedules another one using `ctx->timevec` (see the commonly
   used [[wq-timevec+]]), maxing out the ptime at the ptime of completion so that the forseen interval will
   not be exceeded."

  ([ctx->timevec duration flow]

   (fn event [ctx]
     (f-sampled ctx
                ctx->timevec
                duration
                flow)))


  ([ctx ctx->timevec duration flow]

   (-f-finite ctx
              (fn schedule-sampling [ctx ptime-end]
                (let [timevec-sample (ctx->timevec ctx)]
                  (if (and timevec-sample
                           (< (first timevec-sample)
                              ptime-end))
                    (f-sample ctx
                              timevec-sample)
                    ctx)))
              duration
              flow)))
