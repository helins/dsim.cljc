(ns dvlopt.dsim

  " ? "

  {:author "Adam Helinski"})




;;;;;;;;;; For keeping alphabetical or logical order


(declare 
         transition-key
         transition-path
         poly-infinite
         poly-repeating
         )
  ;in-transition?
         ;move
         ;n-steps
         ;poly-transition
         ;transition-path
         ;
         ;poly-infinite
         ;poly-repeating)




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




(defn millis->n-steps

  ""

  [millis hz]

  (long (Math/round (double (* (/ hz
                                  1000)
                               millis)))))




;;;;;;;;;; Scaling percents


(defn percent->scalar

  ""

  ([value-range percent]

   (* percent
      value-range))


  ([min-value max-value percent]

   (+ (percent->scalar (- max-value
                          min-value)
                       percent)
      min-value)))




(defn fn-percent->scalar

  ""

  ([value-range]

   (fn to-scalar [percent]
     (* percent
        value-range)))


  ([min-value max-value]

   (let [delta (- max-value
                  min-value)]
     (fn to-scalar
       
       ([percent]

        (+ (* percent
              delta)
           min-value))


       ([_state _data-path percent]

        (to-scalar percent))))))




;;;;;;;;;; Helpers for transitions


(defn data-path

  ""

  [transition-path]

  (rest transition-path))




(defn fn-assoc-data

  ""

  [data]

  (fn assoc-data

    ([state data-path]

     (assoc-data state
                 data-path
                 nil))


    ([state data-path _step]

     (assoc-data state
                 data-path
                 nil
                 nil))


    ([state data-path _completion-step _step]

     (assoc-in state
               data-path
               data))))




;(defn fn-map-percent
;
;  ""
;
;  [map-percent]
;
;  (fn fn-on-step [on-step]
;    (fn on-step' [state data-path percent]
;      (on-step state
;               data-path
;               (map-percent percent)))))





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
      (fn on-complete [state data-path completion-step step]
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




(defn last-step

  [first-step n-steps]

  (+ first-step
     (dec n-steps)))




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

   (remove-data state
                data-path
                nil
                nil))


  ([state data-path _completion-step _step]

   (dissoc-in state
              data-path)))




(defn remove-transition

  ""

  ([state data-path]

   (remove-transition state
                      data-path
                      nil
                      nil))


  ([state data-path _completion-step _step]

   (dissoc-in state
              (transition-path data-path))))




(defn remove-subtree

  ""

  ([state data-path]

   (remove-subtree state
                   data-path
                   nil
                   nil))


  ([state data-path _completion-step _step]

   (let [subtree-path (drop-last data-path)]
     (if (in-transition? state
                         subtree-path)
       state
       (dissoc-in state
                  subtree-path)))))




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

    ([first-step]

     (make-infinite first-step
                    nil))


    ([first-step _on-complete]

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

     ([first-step]

      (make-once first-step
                 nil))


     ([first-step on-complete-2]

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

     ([first-step]

      (make-repeating first-step
                      nil))


     ([first-step on-complete-2]

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

  ([first-step fn-transitions]

   (poly first-step
         fn-transitions
         nil))


  ([first-step fn-transitions on-complete]

   (when-some [fn-transition (first fn-transitions)]
     (fn-transition first-step
                    (fn on-complete' [state data-path completion-step step]
                      (if-some [next-transition (poly completion-step
                                                      (rest fn-transitions)
                                                      on-complete)]
                        (-assoc-next-transition state
                                                data-path
                                                step
                                                next-transition)
                        (if on-complete
                          (on-complete state
                                       data-path
                                       completion-step
                                       step)
                          state)))))))




(defn fn-poly

  ""

  ([fn-transitions]

   (fn-poly fn-transitions
            nil))


  ([fn-transitions on-complete]

   (fn make-poly

     ([first-step]

      (make-poly first-step
                 nil))


     ([first-step on-complete-2]

      (poly first-step
            fn-transitions
            (fn-on-complete [on-complete
                             on-complete-2]))))))




(defn- -poly-infinite

  ""

  [first-step all-fn-transitions fn-transitions]

  (when-some [fn-transition (first fn-transitions)]
    (fn-transition first-step
                   (fn ??? [state data-path completion-step step]
                     (-assoc-next-transition state
                                             data-path
                                             step
                                             (or (-poly-infinite completion-step
                                                                 all-fn-transitions
                                                                 (rest fn-transitions))
                                                 (poly-infinite completion-step
                                                                all-fn-transitions)))))))




(defn poly-infinite

  ""

  [first-step fn-transitions]

  (-poly-infinite first-step
                  fn-transitions
                  fn-transitions))




(defn fn-poly-infinite

  ""

  [fn-transitions]

  (fn make-poly-infinite
    
    ([first-step]

     (make-poly-infinite first-step
                         nil))


    ([first-step _on-complete]
     (poly-infinite first-step
                    fn-transitions))))




(defn- -poly-repeating

  ;;

  [first-step n-times all-fn-transitions fn-transitions on-complete]

  (when-some [fn-transition (first fn-transitions)]
    (let [n-times' (dec n-times)]
      (fn-transition first-step
                     (fn ??? [state data-path completion-step step]
                       (if-some [next-transition (or (-poly-repeating completion-step
                                                                      n-times
                                                                      all-fn-transitions
                                                                      (rest fn-transitions)
                                                                      on-complete)
                                                     (when (> n-times'
                                                              0)
                                                       (poly-repeating completion-step
                                                                       n-times'
                                                                       all-fn-transitions
                                                                       on-complete)))]
                         (-assoc-next-transition state
                                                 data-path
                                                 step
                                                 next-transition)
                         (if on-complete
                           (on-complete state
                                        data-path
                                        completion-step
                                        step)
                           state)))))))




(defn poly-repeating

  ""

  ([first-step n-times fn-transitions]

   (poly-repeating first-step
                   n-times
                   fn-transitions
                   nil))


  ([first-step n-times fn-transitions on-complete]

   (-poly-repeating first-step
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

     ([first-step]

      (make-poly first-step
                 nil))


     ([first-step on-complete-2]

      (poly-repeating first-step
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
