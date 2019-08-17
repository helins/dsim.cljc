(ns dvlopt.dsim-test

  ""

  {:author "Adam Helinski"}

  (:require [clojure.test :as t]
            [dvlopt.dsim  :as dsim]))




;;;;;;;;;;


(def n-steps

  ;;

  10)




(defn state

  ;;

  ([]

   (state nil))


  ([on-complete]

   (dsim/add-transition {}
                        :foo
                        0
                        n-steps
                        (fn on-step [s progress]
                          (assoc s
                                 :foo
                                 progress))
                        on-complete)))




(t/deftest add-and-remove

  (let [s (state)]
    (t/is (contains? (::dsim/tag->transition s)
                     :foo)
          "Should contain the :foo transition")
    (t/is (not (contains? (dsim/remove-transition s
                                                  :foo)
                          ::dsim/tag->transition))
          "State should not have any transition anymore")))




(t/deftest move-one-transition

  (let [s (state)]
    (t/is (= s
             (dsim/move s
                        -1))
          "State should not change because the transition has not started yet")
    (t/is (zero? (:foo (dsim/move s
                                  0)))
         "Progress should be at 0 at the start of the transition")
    (let [s-last (dsim/move s
                            (dec n-steps))]
      (t/is (= (:foo s-last)
               1)
            "Progress should be at 100% since we have reached the last step of the transition")
      (let [s-done (dsim/move s-last
                              n-steps)]
        (t/is (not (contains? s-done
                              ::dsim/tag->transition))
              "State should not have any transition now")
        (t/is (= (:foo s-done)
                 1)
              "The :foo value should be there and reflect the fact the transition is done"))))
    (t/is (identical? (dsim/move (state (fn on-complete [_state]
                                          :finished))
                                 n-steps)
                      :finished)
          "On complete should be called successfully when presented with a step >= last step"))




(t/deftest move-two-transitions

    (let [bar-n-steps     (dec n-steps)
          s               (dsim/add-transition (state)
                                               :bar
                                               0
                                               bar-n-steps
                                               (fn on-step [s progress]
                                                 (assoc s
                                                        :bar
                                                        progress)))
          s-bar-done      (-> s
                              (dsim/move (dec bar-n-steps))
                              (dsim/move bar-n-steps))
          tag->transition (::dsim/tag->transition s-bar-done)]
      (t/is (not (contains? tag->transition
                            :bar))
            "The :bar transition should be finished and cleaned")
      (t/is (= (:bar s-bar-done)
               1)
            "The :bar value should still be there from the last transition and report that the transition is now completed")
      (t/is (contains? tag->transition
                       :foo)
            "The :foo transition should still be there and ongoing")))




(t/deftest move-all

  (let [id->s {:entity (state (fn on-complete [_s]
                                nil))}]
    (t/is (= (:foo (:entity (-> id->s
                                (dsim/move-all (- n-steps
                                                  2))
                                (dsim/move-all (dec n-steps)))))
             1)
          "The :foo transition of the :entity should be at its last step")
    (t/is (empty? (dsim/move-all id->s
                                 n-steps))
          "The entity should be remove from the map when it is done")))
