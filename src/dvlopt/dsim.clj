(ns dvlopt.dsim

  " ? "

  {:author "Adam Helinski"})




;;;;;;;;;; For keeping alphabetical or logical order


(declare in-transition?
         move
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




(defn millis->n-steps

  ""

  [millis hz]

  (long (Math/round (double (* (/ hz
                                  1000)
                               millis)))))




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

   (let [value-range (- max-value
                        min-value)]
     (fn to-scalar [percent]
       (+ (* percent
             value-range)
          min-value)))))




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

     (assoc-in state
               data-path
               data))))




(defn fn-mirror

  ""

  [map-percent]

  (fn mirror-on-step [state data-path percent]
    (assoc-in state
              data-path
              (map-percent state
                           data-path
                           percent))))




(defn fn-on-complete

  ""

  [on-complete-vec]

  (let [on-complete-vec' (filterv some?
                                  on-complete-vec)]
    (case (count on-complete-vec')
      0 nil
      1 (first on-complete-vec')
      (fn on-complete [state data-path step]
        (reduce (fn next-on-complete [state' local-on-complete]
                  (local-on-complete state'
                                     data-path
                                     step))
                state
                on-complete-vec')))))




(defn hard-remove-subtree

  ""

  ([state data-path]

   (hard-remove-subtree state
                        data-path
                        nil))


  ([state data-path _step]

   (-> state
       (dissoc-in data-path)
       (dissoc-in (transition-path data-path)))))




(defn last-step

  ""

  [start n-steps]

  (+ start
     (dec n-steps)))




(defn remove-data

  ""

  ([state data-path]

   (remove-data state
                data-path
                nil))


  ([state data-path _step]

   (dissoc-in state
              data-path)))




(defn remove-transition

  ""

  ([state data-path]

   (remove-transition state
                      data-path
                      nil))


  ([state data-path _step]

   (dissoc-in state
              (transition-path data-path))))




(defn remove-subtree

  ""

  ([state data-path]

   (remove-subtree state
                   data-path
                   nil))


  ([state data-path _step]

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




(defn in-transition?

  ""

  ([state]

   (not (empty? (get state
                     transition-key))))


  ([state path]

   (let [transition (get-in state
                            (transition-path path))]
     (or (and (map? transition)
              (not (empty? transition)))
         (some? transition)))))




;;;;;;;;;; Creating transitions


(defn- -call-on-complete

  ;;

  [state data-path step on-complete]

  (if on-complete
    (on-complete state
                 data-path
                 step)
    state))
  



(defn- -remove-on-complete

  ;;

  [state data-path step on-complete]

  (-call-on-complete (dissoc-in state
                                (transition-path data-path))
                     data-path
                     step
                     on-complete))




(defn- -endless-transition

  ;;

  [first-step n-steps on-step]

  (let [last-cycle-step (dec n-steps)]
    (fn state-at-step [state data-path step]
     (if (>= step
             first-step)
       (on-step state
                data-path
                (/ (rem (- step
                           first-step)
                        n-steps)
                   last-cycle-step))
       state))))




(defn- -regular-transition

  ;;

  [first-step n-steps on-step complete-transition on-complete]

  (let [last-step'  (last-step first-step
                               n-steps)
        delta-steps (- last-step'
                       first-step)]
    (fn state-at-step [state data-path step]
      (if (>= step
              first-step)
        (if (<= step
                last-step')
          (on-step state
                   data-path
                   (/ (- step
                         first-step)
                      delta-steps))
          (complete-transition state
                               data-path
                               step
                               on-complete))
        state))))




(defn- -repeating-transition

  ;;

  [n-times first-step n-steps on-step complete-transition on-complete]

  (let [last-cycle-step (dec n-steps)]
    (fn state-at-step [state data-path step]
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
            (complete-transition state
                                 data-path
                                 step
                                 on-complete)))
        state))))




(defn- -transition

  ""

  [first-step steps on-step complete-transition on-complete]

  (let [n-steps (last steps)]
    (when (<= n-steps
              0)
      (throw (IllegalArgumentException. "Number of steps must be > 0")))
    (condp identical?
           (first steps)
      :endless (-endless-transition first-step
                                    n-steps
                                    on-step)
      :once    (-regular-transition first-step
                                    n-steps
                                    on-step
                                    complete-transition
                                    on-complete)
      :repeat  (-repeating-transition (second steps)
                                      first-step
                                      n-steps
                                      on-step
                                      complete-transition
                                      on-complete))))




(defn transition

  ""

  ([first-step steps on-step]

   (transition first-step
               steps
               on-step
               nil))


  ([first-step steps on-step on-complete]

   (-transition first-step
                steps
                on-step
                -remove-on-complete
                on-complete)))




(defn poly-transition

  ""

  [first-step transition-vectors]

  (if-some [[steps
             on-step
             on-complete] (first transition-vectors)]
    (if (identical? (first steps)
                    :endless)
      (transition first-step
                  steps
                  on-step
                  on-complete)
      (-transition first-step
                   steps
                   on-step
                   (fn complete-transition [state data-path step on-complete]
                     (if-some [next-transition (poly-transition (+ first-step
                                                                   (last steps))
                                                                (rest transition-vectors))]
                       (-call-on-complete (-> state
                                              (assoc-in (transition-path data-path)
                                                        next-transition)
                                              (move step))
                                          data-path
                                          step
                                          on-complete)
                       (-remove-on-complete state
                                            data-path
                                            step
                                            on-complete)))
                   on-complete))
    nil))




(defn merge-transitions

  ""

  [state transitions]

  (update state
          transition-key
          deep-merge
          transitions))




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
                                 path)))




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
