(ns dvlopt.dsim

  "Idiomatic, purely-functional discrete event simulation.
  

   A transition is pure stepwise function gradually modifying some arbitrary state map. Transitions are part of the state itself
   and are located under the `transition-key` key. They can be organized in arbitrarily nested maps. It is both common and desired
   for transitions to mirror the data they act upon :


     {dvlopt.dsim/transition-key {:asteroids {42 {:x ...
                                                  :y ...}}}
      :asteroids {42 {:x 450
                      :y 1420}}}

   This pattern is so common that in this example, [:asteroids 42 :x] would be called the `data-path` of the :x transition of
   asteroid 42. Such a transition accepts 3 arguments: a state map, its data-path, and a step. It returns a new state which, although
   not enforced, should somehow modify the :x value of asteroid 42.
  
   For doing so, a transition is created by providing an `on-step` function which also accepts 3 arguments: a state map, the data-path,
   and a percentage of completion. This percentage depends on the first step of the transition, how many steps it lasts, and the
   current step:  (current-step - first-step) / n-steps.

   After reaching 100%, if it was provided in the first place, the `on-complete` function of the transition is called. It accepts
   4 arguments: the current state map, the data-path, the completion step, and the current step. It is useful when action must be
   taken after a transition, for instance for creating a new one. If some steps are missed or skipped, the completion step and the
   current step will not match. Hence it is useful to provide both. Completed transitions are removed automatically.

     Cf. `infinite`
         `once`
         `repeating`

   A poly-transition is a higher-order transition composed of several transitions. At the end of each sub-transition, the poly-transition
   takes care of creating the next one at the right moment. Hence, it would be easy to animate asteroid 42 to sequentially move in
   different directions, or to sequentially rotate in some complex manner. It is also trivial to create nested poly-transitions.

     Cf. `poly`
         `poly-infinite`
         `poly-repeating`


   Scaling a percentage to a value such as the :x position of an asteroid is facilitated by using `scale` and `fn-scale`. It is often
   needed for a transition to behave non-linearly. This can be simply done by modifying the percentage of completion, which is a linear
   progression, to be non-linear. For example, if an asteroid has to move faster and faster along the :x axis from 500 to 1000 pixels in a
   100 steps starting from step 0:

     (dvlopt.dsim/once 0
                       100
                       (let [scale' (dvlopt.dsim/fn-scale 500
                                                          1000)]
                         (fn on-step [state data-path percent]
                           (assoc-in state
                                     data-path
                                     (scale' (Math/pow percent
                                                       2))))))

   The most straightforward way to add or remove transitions to a state is by using `merge-transitions`. A series of helpers for `on-step`
   and `on-complete` functions is provided. For example, improving the last example and removing the asteroid when done :

     (dvlopt.dsim/once 0
                       100
                       (dsim/fn-mirror-percent (comp (dvlopt.dsim/fn-scale 500
                                                                           1000)
                                                     #(Math/pow %
                                                                2)))
                       dsim/remove-pre-data)


   The most basic way of moving a state to some step is done by using `move`. `move-seq` facilitates the process of iteratively moving
   through a sequence of steps. However, the most useful way is probably `move-events` which also takes into account events happening at
   some particular steps, each modifying the state is some way. Any non-trivial simulation involves such events."

  {:author "Adam Helinski"})




;;;;;;;;;; For keeping alphabetical or logical order


(declare poly-infinite
         poly-repeating
         transition-key
         transition-path)




;;;;;;;;;; Utilities


(defn dissoc-in

  "Deep dissoc, natural counterpart of Clojure's `assoc-in`.
  
   Empty maps are removed.
  
  
   Ex. (dissoc-in {:a {:b 42}
                   :c :ok}
                  [:a :b])
  
       => {:c :ok}"

  [hmap [k & ks :as path]]

  (if-not ks
    (dissoc hmap
            k)
    (let [hmap-rest (dissoc-in (get hmap
                                    k)
                               ks)]
      (if (empty? hmap-rest)
        (dissoc hmap
                k)
        (assoc hmap
               k
               hmap-rest)))))




(defn deep-merge

  "Deep merges two maps."

  [hmap-1 hmap-2]

  (merge-with (fn select-value [v-1 v-2]
                (if (and (map? v-1)
                         (map? v-2))
                  (deep-merge v-1
                              v-2)
                  v-2))
              hmap-1
              hmap-2))




(defn last-step

  "Simplify provides the last step of a transition given its first-step and the number of steps it lasts."

  [first-step n-steps]

  (+ first-step
     (dec n-steps)))




(defn millis->n-steps

  "Computes the number of steps needed for completing a transition in `millis` milliseconds for a phenomenon,
   such as the frame-rate, happening `hz` per second.
  
  
   Ex. Computing the number of frames needed in order to last 2000 milliseconds with a frame-rate of 60.

       (millis->n-steps 2000
                        60)
  
       => 120"

  [millis hz]

  (long (Math/round (double (* (/ hz
                                  1000)
                               millis)))))




;;;;;;;;;; Scaling percents and values


(defn- -scale-percent

  ;; Scale a percent value to an arbitrary range.

  [scaled-a scaled-delta percent]

  (+ (* percent
        scaled-delta)
     scaled-a))




(defn- -scale

  ;; Scale an arbitrary value to another range.

  [scaled-a scaled-delta a delta x]

  (-scale-percent scaled-a
                  scaled-delta
                  (/ (- x 
                        a)
                     delta)))



(defn scale

  "3 args : scales a `percent` value to a value between `scaled-a` and `scaled-b`.

   5 args : scales the `x` value between `a` and `b` to be between `scaled-a` and `scaled-b`.

  
   Ex. (scale 0
              1000
              0.5)

       => 500


       (scale 0
              1000
              0
              100
              50)

       => 500"

  ([scaled-a scaled-b percent]

   (-scale-percent scaled-a
                   (- scaled-b
                      scaled-a)
                   percent))


  ([scaled-a scaled-b a b x]

   (-scale scaled-a
           (- scaled-b
              scaled-a)
           a
           (- b
              a)
           x)))




(defn fn-scale

  "Exactly like `scale` but does not accept a value to scale. Instead, returns a function which does so.
  
   Particularly useful when working with the percentage of completion of transitions."

  ([scaled-a scaled-b]
   
   (let [scaled-delta (- scaled-b
                         scaled-a)]
     (fn scale-percent
        
       ([percent]

        (-scale-percent scaled-a
                        scaled-delta
                        percent))


       ([_state _data-path percent]

        (scale-percent percent)))))


  ([scaled-a scaled-b a b]

   (let [delta        (- b
                         a)
         scaled-delta (- scaled-b
                         scaled-a)]
     (fn scale' 

       ([x]

        (-scale scaled-a
                scaled-delta
                a
                delta
                x))


       ([_state _data-path x]

        (scale' x))))))




;;;;;;;;;; Helpers for transitions and state management


(defn fn-assoc-data

  "Returns a function assoc'ing the given data at the data-path of a transition.
  
   Useful when some steps might be skipped but it is needed for a transition to reach 100%. For instance,
   during a live animation, a frame will probably not be drawn at the exact millisecond a transition should
   complete but some milliseconds later. The returned function can be used as an `on-complete` function
   so that the state will always reflect the last step of such a transition."

  [data]

  (fn assoc-data

    ([state data-path]

     (assoc-in state
               data-path
               data))


    ([state data-path _step]

     (assoc-data state
                 data-path))


    ([state data-path _completion-step _step]
     (assoc-data state
                 data-path))))




(defn fn-mirror

  "Given an `on-step` function returning some arbitrary value instead of a new state, returns an `on-step`
   function assoc'ing this value at the data-path in the state.
  
   Idiomatic."

  [map-percent]

  (fn mirror-on-step [state data-path percent]
    (assoc-in state
              data-path
              (map-percent state
                           data-path
                           percent))))




(defn fn-mirror-percent

  "Behaves just like `fn-mirror` but the function provided in the first place simply maps a percent value to
   an arbitrary one instead of being an `on-step` function.
  
   Small convenient helper when the current state and data-path are not needed."

  [map-only-percent]

  (fn-mirror (fn only-percent [_state _data-path percent]
               (map-only-percent percent))))







(defn- -in-transition?

  ;; Checks if there are any transitions.

  [subtree]

  (or (and (map? subtree)
           (not (empty? subtree)))
      (some? subtree)))




(defn in-transition?

  "Is the given state or some part of it currently in transition?"

  ([state]

   (-in-transition? (get state
                         transition-key)))


  ([state data-path]

   (-in-transition? (get-in state
                            (transition-path data-path)))))




(defn merge-transitions

  "Deep merges the provided - often nested - map of transitions in the given state.
  
   Very useful for adding or removing several transitions at once. Indeed, nil values are simply removed when moving the state."

  [state transitions]

  (update state
          transition-key
          deep-merge
          transitions))




(defn pipe-complete

  "Given a collection of `on-complete` functions, returns an `on-complete` function piping arguments into this collection.
  
   Nil values are simply filtered-out."

  [on-completes]

  (let [on-completes' (filterv some?
                               on-completes)]
    (case (count on-completes')
      0 nil
      1 (first on-completes')
      2 (let [[on-complete-1
               on-complete-2] on-completes']
          (fn piped-on-complete [state data-path completion-step step]
            (-> state
                (on-complete-1 data-path
                               completion-step
                               step)
                (on-complete-2 data-path
                               completion-step
                               step))))
      (fn reduce-on-complete [state data-path completion-step step]
        (reduce (fn next-on-complete [state' local-on-complete]
                  (local-on-complete state'
                                     data-path
                                     completion-step
                                     step))
                state
                on-completes')))))




(defn remove-data

  "Uses `dissoc-in` for removing what is at some data-path.
  
   More useful when used as an `on-complete` function and the data needs to be cleaned once the transition completes."

  ([state data-path]

   (dissoc-in state
              data-path))

  ([state data-path _percent]

   (remove-data state
                data-path))


  ([state data-path _completion-step _step]

   (remove-data state
                data-path)))




(defn remove-transition

  "Removes a transition given the data-path."

  ([state data-path]

   (dissoc-in state
              (transition-path data-path)))


  ([state data-path _percent]

   (remove-transition state
                      data-path))


  ([state data-path _completion-step _step]

   (remove-transition state
                      data-path)))





(defn remove-pre-data

  "A vast majority of modeling involves some form of entities. It is also very common for such entities to be removed once all
   their transitions completes, meaning they cannot evolve anymore. This function, used as an `on-complete` function, does exactly that.

   For instance, modeling asteroids as {:asteroids {42 {:x 542
                                                        :y 1000}}} having :x and :y transitions.

   Once it cannot move anymore, an asteroid must be cleaned (ie. removed from the state). By providing this function as an `on-complete`
   function to every :x and :y transition garantees that. It will use `dissoc-in` for removing [:asteroids 42] once it does not have
   any transitions anymore."

  ([state data-path]

   (let [subtree-path (drop-last data-path)]
     (if (in-transition? state
                         subtree-path)
       state
       (dissoc-in state
                  subtree-path))))


  ([state data-path _percent]

   (remove-pre-data state
                    data-path))


  ([state data-path _completion-step _step]

   (remove-pre-data state
                    data-path)))




(def transition-key

  "All transitions belonging to a state map must be under this key."

  ::transitions)




(defn transition-path

  "Given a data-path, returns a transition-path"

  [data-path]

  (cons transition-key
        data-path))




(defn without-transitions

  "Returns the given state without its transitions."

  [state]

  (dissoc state
          transition-key))




;;;;;;;;;; Creating transitions


(defn- -complete-transition

  ;; Completes a mono-transition.

  ;; TODO. `transition` not needed?!

  [state data-path completion-step step transition on-complete]

  (let [state' (remove-transition state
                                  data-path)]
    (if on-complete
      (on-complete state'
                   data-path
                   completion-step
                   step)
      state')))




(defn infinite

  "Returns a transition endlessly repeating cycles of `n-steps` steps.
  
   Obviously, it does not need an `on-complete` function."

  [first-step n-steps on-step]

  (let [last-cycle-step (dec n-steps)]
    (fn infinite-transition [state data-path step]
     (if (>= step
             first-step)
       (on-step state
                data-path
                (double (/ (rem (- step
                                   first-step)
                                n-steps)
                           last-cycle-step)))
       state))))




(defn fn-infinite

  "Returns a function returning an infinite transition.

   Useful for poly-transitions.

  
   Cf. `infinite`
       `poly`"

  [n-steps on-step]

  (fn make-infinite

    ([state first-step]

     (make-infinite state
                    first-step
                    nil))


    ([_state first-step _on-complete]

     (infinite first-step
               n-steps
               on-step))))




(defn once

  "Returns a transition lasting `n-steps` steps."

  ([first-step n-steps on-step]

   (once first-step
         n-steps
         on-step
         nil))


  ([first-step n-steps on-step on-complete]

   (let [last-step'  (last-step first-step
                                n-steps)
         delta-steps (- last-step'
                        first-step)]
     (fn once-transition [state data-path step]
       (if (>= step
               first-step)
         (if (<= step
                 last-step')
           (on-step state
                    data-path
                    (double (/ (- step
                                  first-step)
                               delta-steps)))
           (-complete-transition state
                                 data-path
                                 (inc last-step')
                                 step
                                 once-transition
                                 on-complete))
         state)))))




(defn fn-once

  "Returns a function returning a transition.
  
   Useful for poly-transitions.

  
   Cf. `once`
       `poly`"

  ([n-steps on-step]

   (fn-once n-steps
            on-step
            nil))


  ([n-steps on-step on-complete]

   (fn make-once

     ([state first-step]

      (make-once state
                 first-step
                 nil))


     ([_state first-step on-complete-2]

      (once first-step
            n-steps
            on-step
            (pipe-complete [on-complete
                            on-complete-2]))))))




(defn- -validate-n-times

  ;; Ensures repeating transitions happen more than once.

  [n-times]

  (when (<= n-times
            1)
    (throw (IllegalArgumentException. "`n-times` must be > 1"))))




(defn repeating

  "Returns a transition repeating `n-steps` steps `n-times` times."

  ([first-step n-times n-steps on-step]

   (repeating first-step
              n-times
              n-steps
              on-step
              nil))


  ([first-step n-times n-steps on-step on-complete]

   (-validate-n-times n-times) 
   (let [last-cycle-step (dec n-steps)]
     (fn repeating-transition [state data-path step]
       (if (>= step
               first-step)
         (let [delta-first (- step
                              first-step)]
           (if (< (quot delta-first
                        n-steps)
                   n-times)
             (on-step state
                      data-path
                      (double (/ (rem delta-first
                                      n-steps)
                                 last-cycle-step)))
             (-complete-transition state
                                   data-path
                                   (+ first-step
                                      (* n-times
                                         n-steps))
                                   step
                                   repeating-transition
                                   on-complete)))
         state)))))




(defn fn-repeating

  "Returns a function returning a repeating transition.
  
   Useful for poly-transitions.
  
  
   Cf. `repeating`
       `poly`"

  ([n-times n-steps on-step]

   (fn-repeating n-times
                 n-steps
                 on-step
                 nil))


  ([n-times n-steps on-step on-complete]

   (fn make-repeating

     ([state first-step]

      (make-repeating state
                      first-step
                      nil))


     ([_state first-step on-complete-2]

      (repeating first-step
                 n-times
                 n-steps
                 on-step
                 (pipe-complete [on-complete
                                 on-complete-2]))))))




(defn- -assoc-next-transition

  ;; Assoc'es the given transition and also realizes it for the given step, thus returning a new state.
  ;;
  ;; Helper for poly-transitions.

  [state data-path step transition]

  (transition (assoc-in state
                        (transition-path data-path)
                        transition)
              data-path
              step))




(defn poly

  "Returns a poly-transition which will follow a collection of functions producing transitions.

   Those functions will be provided with the state at the moment the transition is created, the first-step 
   of this transition and an `on-complete` function which must not be ignored. This `on-complete` function
   ensures that the next transition, if there is one, will be created."

  ([state first-step fn-transitions]

   (poly state
         first-step
         fn-transitions
         nil))


  ([state first-step fn-transitions on-complete]

   (when-some [fn-transition (first fn-transitions)]
     (fn-transition state
                    first-step
                    (fn on-complete' [state' data-path completion-step step]
                      (if-some [next-transition (poly state'
                                                      completion-step
                                                      (rest fn-transitions)
                                                      on-complete)]
                        (-assoc-next-transition state'
                                                data-path
                                                step
                                                next-transition)
                        (if on-complete
                          (on-complete state'
                                       data-path
                                       completion-step
                                       step)
                          state')))))))




(defn fn-poly

  "Returns a function returning a poly-transition.
  
   Useful for nested poly-transitions.
  
  
   Cf. `poly`"

  ([fn-transitions]

   (fn-poly fn-transitions
            nil))


  ([fn-transitions on-complete]

   (fn make-poly

     ([state first-step]

      (make-poly state
                 first-step
                 nil))


     ([state first-step on-complete-2]

      (poly state
            first-step
            fn-transitions
            (pipe-complete [on-complete
                            on-complete-2]))))))




(defn- -poly-infinite

  ;; Helper for `poly-infinite`.

  [state first-step all-fn-transitions fn-transitions]

  (when-some [fn-transition (first fn-transitions)]
    (fn-transition state
                   first-step
                   (fn endless-cycle [state' data-path completion-step step]
                     (-assoc-next-transition state'
                                             data-path
                                             step
                                             (or (-poly-infinite state'
                                                                 completion-step
                                                                 all-fn-transitions
                                                                 (rest fn-transitions))
                                                 (poly-infinite state'
                                                                completion-step
                                                                all-fn-transitions)))))))




(defn poly-infinite

  "Union of `infinite` and `poly`. Returns a poly-transition endlessly repeating."

  [state first-step fn-transitions]

  (-poly-infinite state
                  first-step
                  fn-transitions
                  fn-transitions))




(defn fn-poly-infinite

  "Returns a function returning an infinite poly-transition.
  
   Useful for nested poly-transitions.
  
  
   Cf. `poly`
       `poly-infinite`"

  [fn-transitions]

  (fn make-poly-infinite
    
    ([state first-step]

     (make-poly-infinite state
                         first-step
                         nil))


    ([state first-step _on-complete]

     (poly-infinite state
                    first-step
                    fn-transitions))))




(defn- -poly-repeating

  ;; Helper for `poly-repeating`.

  [state first-step n-times all-fn-transitions fn-transitions on-complete]

  (when-some [fn-transition (first fn-transitions)]
    (let [n-times' (dec n-times)]
      (fn-transition state
                     first-step
                     (fn repeating-cycle [state' data-path completion-step step]
                       (if-some [next-transition (or (-poly-repeating state'
                                                                      completion-step
                                                                      n-times
                                                                      all-fn-transitions
                                                                      (rest fn-transitions)
                                                                      on-complete)
                                                     (when (> n-times'
                                                              0)
                                                       (-poly-repeating state'
                                                                        completion-step
                                                                        n-times'
                                                                        all-fn-transitions
                                                                        all-fn-transitions
                                                                        on-complete)))]
                         (-assoc-next-transition state'
                                                 data-path
                                                 step
                                                 next-transition)
                         (if on-complete
                           (on-complete state'
                                        data-path
                                        completion-step
                                        step)
                           state')))))))




(defn poly-repeating

  "Union of `repeating` and `poly`. Returns a poly-transition repeating `n-times`."

  ([state first-step n-times fn-transitions]

   (poly-repeating state
                   first-step
                   n-times
                   fn-transitions
                   nil))


  ([state first-step n-times fn-transitions on-complete]
   
   (-validate-n-times n-times)
   (-poly-repeating state
                    first-step
                    n-times
                    fn-transitions
                    fn-transitions
                    on-complete)))




(defn fn-poly-repeating

  "Returns a function returning a repeating poly-transition.
 
   Useful for nested poly-transitions.
  
  
   Cf. `poly`
       `poly-repeating`"

  ([n-times fn-transitions]

   (fn-poly-repeating n-times
                      fn-transitions
                      nil))


  ([n-times fn-transitions on-complete]

   (fn make-poly

     ([state first-step]

      (make-poly state
                 first-step
                 nil))


     ([state first-step on-complete-2]

      (poly-repeating state
                      first-step
                      fn-transitions
                      (pipe-complete [on-complete
                                      on-complete-2]))))))




;;;;;;;;;; Moving states through steps


(defn- -recur-move

  ;; Helper for `move`.

  [state step path transition]

  (cond
    (fn? transition)  (transition state
                                  path
                                  step)
    (map? transition) (reduce-kv (fn recur-over [state k transition]
                                   (-recur-move state
                                                step
                                                (conj path
                                                      k)
                                                transition))
                                 state
                                 transition)
    (nil? transition) (dissoc-in state
                                 (transition-path path))))




(defn move

  "Moves a state to the given step (ie. returns a new state representing the given state at the given step).
  
   It belongs to the user to ensure steps are not skipped if it is the needed behavior."

  [state step]

  (reduce-kv (fn recur-over [state k transition]
               (-recur-move state
                            step
                            [k]
                            transition))
             state
             (get state
                  transition-key)))




(defn move-seq

  "Returns a lazy sequence of [state' step] by iteratively moving the given state following the given sequence of steps.
  
   Stops as soon as it reaches the end of `step-seq` or it detects there no more transitions, meaning it is useless to
   continue."

  [state step-seq]

  (lazy-seq
    (when-let [step (and (not (empty? (get state
                                           transition-key)))
                         (first step-seq))]
      (let [state' (move state
                         step)]
        (cons [state'
               step]
              (move-seq state'
                        (rest step-seq)))))))




(def step-key

  "When using `move-events`, each event must have a step assigned by this key."

  ::step)




(defn move-events

  "Merely moving a state is often not enough. Typically, some events happen which modify the state in some way, add, remove,
   or replace transitions. This function does exactly that.
  
   It behaves like `move-seq` in that it moves an initial state following a sequence of steps. However, the state is also modified
   by following a sequence of events and an event handler.
  
   An event is an arbitrary map which describes something happening at some arbitrary step. As such, it must have a step associated
   under the `step-key` key. Events are assumed to be sorted in chronological order (ie. by step).

   `handle-event` takes the current state and an event, it must return a new state. It is called everytime an event happens.
  

   Like `move-seq`, returns a lazy sequence of [state' step]."

  ;; A bit ugly, but functional and somewhat efficient.

  [state step-seq events handle-event]

  (lazy-seq
    (if-some [events' (seq events)]
      (when-some [step (first step-seq)]
        (loop [events'2 events'
               state'   state]
          (let [event (first events'2)]
            (if (<= (or (get event
                             step-key)
                        (throw (IllegalArgumentException. "Event does not have a step")))

                    step)
              (let [state-after-event (handle-event state'
                                                    event)]
                (if-some [next-events (next events'2)]
                  (recur next-events
                         state-after-event)
                  (let [moved-state (move state-after-event
                                          step)]
                    (cons [moved-state
                           step]
                          (move-seq moved-state
                                    (rest step-seq))))))
              (let [moved-state (move state'
                                      step)]
                (cons [moved-state
                       step]
                      (move-events moved-state
                                   (rest step-seq)
                                   events'2
                                   handle-event)))))))
      (move-seq state
                step-seq))))
