(ns dvlopt.dsim

  " ? "

  {:author "Adam Helinski"})




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




;;;;;;;;;; Creating transitions


(defn last-step

  ""

  [start n-steps]

  (+ start
     (dec n-steps)))




(defn- -percent

  ;;

  [delta first-step current-step]

  (/ (- current-step
        first-step)
     delta))




(defn transition

  ""

  ([first-step last-step on-step]

   (transition first-step
               last-step
               on-step
               nil))


  ([first-step last-step on-step on-complete]

   (let [delta (- last-step
                  first-step)]
     (fn state-at-step [state path step]
       (if (>= step
               first-step)
         (if (<= step
                 last-step)
           (on-step state
                    path
                    (-percent delta
                              first-step
                              step))
           (let [state' (dissoc-in state
                                   path)]
             (if on-complete
               (on-complete state'
                            path)
               state')))
         state)))))




(defn in-mirror

  ""

  ([state k-transitions path first-step last-step map-percent]

   (in-mirror state
              k-transitions
              path
              first-step
              last-step
              map-percent
              nil))


  ([state k-transitions path first-step last-step map-percent on-complete]

   (assoc-in state
             (cons k-transitions
                   path)
             (transition first-step
                         last-step
                         (fn on-step [state' transition-path percent]
                           (assoc-in state'
                                     path
                                     (map-percent state'
                                                  transition-path
                                                  percent)))
                         on-complete))))




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
                                 transition)))




(defn move

  ""

  [state k-transitions step]

  (let [path [k-transitions]]
    (reduce-kv (fn ??? [state k transition]
                 (-recur-move state
                              step
                              (conj path
                                    k)
                              transition))
               state
               (get state
                    k-transitions))))




(defn move-seq

  [state k-transitions step-seq]

  (lazy-seq
    (when-let [step (and (not (empty? (get state
                                           k-transitions)))
                         (first step-seq))]
      (let [state' (move state
                         k-transitions
                         step)]
        (cons [state'
               step]
              (move-seq state'
                        k-transitions
                        (rest step-seq)))))))




(defn move-events

  ""

  ;; A bit ugly, but functional and somewhat efficient.

  ;; TODO. Should throw when ::step is missing from an event ?

  [state k-transitions step-seq events handle-event]

  (lazy-seq
    (if-some [events' (seq events)]
      (when-some [step (first step-seq)]
        (loop [events'2 events'
               state'   state]
          (let [event (first events'2)]
            (if (<= (::step event)
                    step)
              (let [state-after-event (handle-event state'
                                                    event)]
                (if-some [next-events (next events'2)]
                  (recur next-events
                         state-after-event)
                  (let [moved-state (move state-after-event
                                          k-transitions
                                          step)]
                    (cons [moved-state
                           step]
                          (move-seq moved-state
                                    k-transitions
                                    (rest step-seq))))))
              (let [moved-state (move state'
                                      k-transitions
                                      step)]
                (cons [moved-state
                       step]
                      (move-events moved-state
                                   k-transitions
                                   (rest step-seq)
                                   events'2
                                   handle-event)))))))
      (move-seq state
                k-transitions
                step-seq))))
