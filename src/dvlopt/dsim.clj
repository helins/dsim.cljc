(ns dvlopt.dsim

  " ? "

  {:author "Adam Helinski"})




;;;;;;;;;; Utilities


(defn millis->n-steps

  ""

  [millis hz]

  (long (Math/round (double (* (/ hz
                                  1000)
                               millis)))))




;;;;;;;;;; Transitions


(defn last-step

  ""

  [start n-steps]

  (+ start
     (dec n-steps)))




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
                    (/ (- step
                          first-step)
                       delta))
           (let [state' (dissoc-in state
                                   path)]
             (if on-complete
               (on-complete state'
                            path)
               state')))
         state)))))




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
        (cons [state' step]
              (move-seq state'
                        k-transitions
                        (rest step-seq)))))))
