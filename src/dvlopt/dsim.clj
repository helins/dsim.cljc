(ns dvlopt.dsim

  " ? "

  {:author "Adam Helinski"})




;;;;;;;;;; For keeping alphabetical or logical order


(declare poly-infinite
         poly-repeating
         transition-key
         transition-path)




;;;;;;;;;; Utilities


(defn dissoc-in

  ""

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

  ""

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

  [first-step n-steps]

  (+ first-step
     (dec n-steps)))




(defn millis->n-steps

  ""

  [millis hz]

  (long (Math/round (double (* (/ hz
                                  1000)
                               millis)))))




;;;;;;;;;; Scaling percents and values


(defn- -scale-percent

  ""

  [scaled-a scaled-delta percent]

  (+ (* percent
        scaled-delta)
     scaled-a))




(defn- -scale

  ;;

  [scaled-a scaled-delta a delta x]

  (-scale-percent scaled-a
                  scaled-delta
                  (/ (- x 
                        a)
                     delta)))



(defn scale

  ""

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

  ""

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

  ""

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

  ""

  [map-percent]

  (fn mirror-on-step [state data-path percent]
    (assoc-in state
              data-path
              (map-percent state
                           data-path
                           percent))))




(defn fn-mirror-percent

  ""

  [map-only-percent]

  (fn-mirror (fn only-percent [_state _data-path percent]
               (map-only-percent percent))))




(defn fn-on-complete

  ""

  [on-complete-vec]

  (let [on-complete-vec' (filterv some?
                                  on-complete-vec)]
    (case (count on-complete-vec')
      0 nil
      1 (first on-complete-vec')
      2 (let [[on-complete-1
               on-complete-2] on-complete-vec']
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
                on-complete-vec')))))




(defn- -in-transition?

  ;;

  [subtree]

  (or (and (map? subtree)
           (not (empty? subtree)))
      (some? subtree)))




(defn in-transition?

  ""

  ([state]

   (-in-transition? (get state
                         transition-key)))


  ([state path]

   (-in-transition? (get-in state
                            (transition-path path)))))




(defn merge-transitions

  ""

  [state transitions]

  (update state
          transition-key
          deep-merge
          transitions))




(defn remove-data

  ""

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

  ""

  ([state data-path]

   (dissoc-in state
              (transition-path data-path)))


  ([state data-path _percent]

   (remove-transition state
                      data-path))


  ([state data-path _completion-step _step]

   (remove-transition state
                      data-path)))





(defn remove-subtree

  ""

  ([state data-path]

   (let [subtree-path (drop-last data-path)]
     (if (in-transition? state
                         subtree-path)
       state
       (dissoc-in state
                  subtree-path))))


  ([state data-path _percent]

   (remove-subtree state
                   data-path))


  ([state data-path _completion-step _step]

   (remove-subtree state
                   data-path)))




(def transition-key

  ""

  ::transitions)




(defn transition-path

  ""

  [data-path]

  (cons transition-key
        data-path))




(defn without-transitions

  ""

  [state]

  (dissoc state
          transition-key))




;;;;;;;;;; Creating transitions


(defn- -complete-transition

  ;;

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

  ;;

  [first-step n-steps on-step]

  (let [last-cycle-step (dec n-steps)]
    (fn infinite-transition [state data-path step]
     (if (>= step
             first-step)
       (on-step state
                data-path
                (/ (rem (- step
                           first-step)
                        n-steps)
                   last-cycle-step))
       state))))




(defn fn-infinite

  ""

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

  ;;

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
                    (/ (- step
                          first-step)
                       delta-steps))
           (-complete-transition state
                                 data-path
                                 (inc last-step')
                                 step
                                 once-transition
                                 on-complete))
         state)))))




(defn fn-once

  ""

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
            (fn-on-complete [on-complete
                             on-complete-2]))))))




(defn repeating

  ;;

  ([first-step n-times n-steps on-step]

   (repeating first-step
              n-times
              n-steps
              on-step
              nil))


  ([first-step n-times n-steps on-step on-complete]

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
                      (/ (rem delta-first
                              n-steps)
                         last-cycle-step))
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

  ;;

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
                 (fn-on-complete [on-complete
                                  on-complete-2]))))))




(defn- -assoc-next-transition

  ;;

  [state data-path step transition]

  (transition (assoc-in state
                        (transition-path data-path)
                        transition)
              data-path
              step))




(defn poly


  ""

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

  ""

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
            (fn-on-complete [on-complete
                             on-complete-2]))))))




(defn- -poly-infinite

  ""

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

  ""

  [state first-step fn-transitions]

  (-poly-infinite state
                  first-step
                  fn-transitions
                  fn-transitions))




(defn fn-poly-infinite

  ""

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

  ;;

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
                                                       (poly-repeating state'
                                                                       completion-step
                                                                       n-times'
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

  ""

  ([state first-step n-times fn-transitions]

   (poly-repeating state
                   first-step
                   n-times
                   fn-transitions
                   nil))


  ([state first-step n-times fn-transitions on-complete]

   (-poly-repeating state
                    first-step
                    n-times
                    fn-transitions
                    fn-transitions
                    on-complete)))




(defn fn-poly-repeating

  ""

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
                      (fn-on-complete [on-complete
                                       on-complete-2]))))))




;;;;;;;;;; Moving states through steps


(defn- -recur-move

  ;;

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

  ""

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

  ""

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

  ""

  ::step)




(defn move-events

  ""

  ;; A bit ugly, but functional and somewhat efficient.

  ;; TODO. Should throw when ::step is missing from an event ?

  [state step-seq events handle-event]

  (lazy-seq
    (if-some [events' (seq events)]
      (when-some [step (first step-seq)]
        (loop [events'2 events'
               state'   state]
          (let [event (first events'2)]
            (if (<= (get event
                         step-key)
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
