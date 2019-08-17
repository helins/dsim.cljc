(ns dvlopt.dsim

  " ? "

  {:author "Adam Helinski"}

  (:require [dvlopt.void :as void]))





;;;;;;;;;; Utilities


(defn millis->n-steps

  ""

  [millis hz]

  (long (Math/round (double (* (/ hz
                                  1000)
                               millis)))))




;;;;;;;;;; Transitions


(defn remove-transition

  ""

  [state attribute]

  (let [attribute->transition (dissoc (::attribute->transition state)
                                      attribute)]
    (if (empty? attribute->transition)
      (dissoc state
              ::attribute->transition)
      (assoc state
             ::attribute->transition
             attribute->transition))))




(defn transition

  ""

  ([attribute start n-steps on-step]

   (transition attribute
               start
               n-steps
               on-step
               nil))


  ([attribute start n-steps on-step on-complete]

   (let [end   (+ start
                  (dec n-steps))
         delta (- end
                  start)]
     (fn compute-value [state step]
       (if (>= step
               start)
         (if (<= step end)
           (on-step state
                    (/ (- step
                          start)
                       delta))
           (let [state' (remove-transition state
                                           attribute)]
             (if on-complete
               (on-complete state')
               state')))
         state)))))






(defn add-transition

  ""

  ([entity attribute start n-steps on-step]

   (add-transition entity
                   attribute
                   start
                   n-steps
                   on-step
                   nil))

  ([entity attribute start n-steps on-step on-complete]

   (assoc-in entity
             [::attribute->transition
              attribute]
             (transition attribute
                         start
                         n-steps
                         on-step
                         on-complete))))




(defn advance

  ""

  [state step]

  (reduce-kv (fn next-state [state' _attribute transition]
               (let [state'2 (transition state
                                         step)]
                 (if (nil? state'2)
                   (reduced nil)
                   state'2)))
             state
             (::attribute->transition state)))




(defn advance-all

  ""

  [id->state step]

  (persistent! (reduce-kv (fn advance-state [id->state' id state]
                            (if-some [state' (advance state
                                                      step)]
                              (assoc! id->state'
                                      id
                                      state')
                              id->state'))
                          (transient {})
                          id->state)))
