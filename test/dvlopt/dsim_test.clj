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
    (t/is (contains? (::dsim/attribute->transition s)
                     :foo)
          "Should contain the :foo transition")
    (t/is (not (contains? (dsim/remove-transition s
                                                  :foo)
                          ::dsim/attribute->transition))
          "State should not have any transition anymore")))




(t/deftest advance-one-transition

  (let [s (state)]
    (t/is (= s
             (dsim/advance s
                           -1))
          "State should not change because the transition has not started yet")
    (t/is (zero? (:foo (dsim/advance s
                                     0)))
         "Progress should be at 0 at the start of the transition")
    (let [s-last (dsim/advance s
                               (dec n-steps))]
      (t/is (= (:foo s-last)
               1)
            "Progress should be at 100% since we have reached the last step of the transition")
      (let [s-done (dsim/advance s-last
                                 n-steps)]
        (t/is (not (contains? s-done
                              ::dsim/attribute->transition))
              "State should not have any transition now")
        (t/is (= (:foo s-done)
                 1)
              "The :foo value should be there and reflect the fact the transition is done"))))
    (t/is (identical? (dsim/advance (state (fn on-complete [_state]
                                             :finished))
                                    n-steps)
                      :finished)
          "On complete should be called successfully when presented with a step >= last step"))




(t/deftest advance-two-transitions

    (let [bar-n-steps           (dec n-steps)
          s                     (dsim/add-transition (state)
                                                     :bar
                                                     0
                                                     bar-n-steps
                                                     (fn on-step [s progress]
                                                       (assoc s
                                                              :bar
                                                              progress)))
          s-bar-done            (-> s
                                    (dsim/advance (dec bar-n-steps))
                                    (dsim/advance bar-n-steps))
          attribute->transition (::dsim/attribute->transition s-bar-done)]
      (t/is (not (contains? attribute->transition
                            :bar))
            "The :bar transition should be finished and cleaned")
      (t/is (= (:bar s-bar-done)
               1)
            "The :bar value should still be there from the last transition and report that the transition is now completed")
      (t/is (contains? attribute->transition
                       :foo)
            "The :foo transition should still be there and ongoing")))




(t/deftest advance-all

  (let [id->s {:entity (state (fn on-complete [_s]
                                nil))}]
    (t/is (= (:foo (:entity (-> id->s
                                (dsim/advance-all (- n-steps
                                                     2))
                                (dsim/advance-all (dec n-steps)))))
             1)
          "The :foo transition of the :entity should be at its last step")
    (t/is (empty? (dsim/advance-all id->s
                                    n-steps))
          "The entity should be remove from the map when it is done")))
