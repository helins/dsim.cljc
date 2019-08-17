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


(defn remove-on-complete

  ""

  [_state]

  nil)




(defn remove-transition

  ""

  [state tag]

  (let [tag->transition (dissoc (::tag->transition state)
                                tag)]
    (if (empty? tag->transition)
      (dissoc state
              ::tag->transition)
      (assoc state
             ::tag->transition
             tag->transition))))




(defn transition

  ""

  ([tag start n-steps on-step]

   (transition tag
               start
               n-steps
               on-step
               nil))


  ([tag start n-steps on-step on-complete]

   (let [end   (+ start
                  (dec n-steps))
         delta (- end
                  start)]
     (fn state-at-step [state step]
       (if (>= step
               start)
         (if (<= step
                 end)
           (on-step state
                    (/ (- step
                          start)
                       delta))
           (let [state' (remove-transition state
                                           tag)]
             (if on-complete
               (on-complete state')
               state')))
         state)))))




(defn add-transition

  ""

  ([entity tag start n-steps on-step]

   (add-transition entity
                   tag
                   start
                   n-steps
                   on-step
                   nil))

  ([entity tag start n-steps on-step on-complete]

   (assoc-in entity
             [::tag->transition
              tag]
             (transition tag
                         start
                         n-steps
                         on-step
                         on-complete))))




(defn move

  ""

  [state step]

  (reduce-kv (fn apply-transition [state' _tag transition]
               (let [state'2 (transition state'
                                         step)]
                 (if (nil? state'2)
                   (reduced nil)
                   state'2)))
             state
             (::tag->transition state)))




(defn move-all

  ""

  [id->state step]

  (persistent! (reduce-kv (fn move-entity [id->state' id state]
                            (if-some [state' (move state
                                                   step)]
                              (assoc! id->state'
                                      id
                                      state')
                              id->state'))
                          (transient {})
                          id->state)))
