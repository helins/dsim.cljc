(ns dvlopt.dsim-test

  ""

  {:author "Adam Helinski"}

  (:require [clojure.test :as t]
            [dvlopt.dsim  :as dsim]))




;;;;;;;;;;


(t/deftest dissoc-in

  (let [hmap {:a {:b {:c 42}}
              :d {:e 42
                  :f 42}
              :g 42}]
    (t/is (not (contains? (dsim/dissoc-in hmap
                                          [:a :b :c])
                          :a))
          "Nested maps with no values should be removed")
    (t/is (not (contains? (dsim/dissoc-in hmap
                                          [:a])
                          :a))
          "Removing a first-level key should indeed get rid of everything behind")
    (t/is (= (get-in (dsim/dissoc-in hmap
                                     [:d :e])
                     [:d :f])
             42)
          "Removing one key in a nested map should not have an effect on any other key")
    (t/is (not (contains? (dsim/dissoc-in hmap
                                          [:g])
                          :g))
          "Should work with non-nested values")

    (t/is (identical? (dsim/dissoc-in hmap
                                     nil)
                      hmap)
          "Specifying no path should not have any effect at all")))




(defn on-step

  ;;

  [state path progress]

  (assoc-in state
            (rest path)
            progress))




(t/deftest move

  (let [state {::transitions {:a {:x (dsim/transition 0
                                                      5
                                                      on-step)
                                  :y (dsim/transition 0
                                                      10
                                                      on-step
                                                      (fn on-complete [state path]
                                                        (dsim/dissoc-in state
                                                                        (rest path))))}
                              :b (dsim/transition 0
                                                  15
                                                  on-step)}}
        state-5 (dsim/move state
                           ::transitions
                           5)]
    (t/is (= (:x (:a state-5))
             1)
          "Value of :x should reflect that the transition is complete")
    (t/is (= (double (:y (:a state-5)))
             0.5)
          "Value of :y should reflecht that the transition is half-done")
    (let [state-6 (dsim/move state-5
                             ::transitions
                             6)
          a-6     (:a (::transitions state-6))]
      (t/is (not (contains? a-6
                            :x))
            "The :x transition should be completed and gone")
      (t/is (contains? a-6
                       :y)
            "The :y transition should still be going on")
      (t/is (contains? (::transitions state-6)
                       :b)
            "The :b transition should still be going on")
      (let [state-11 (dsim/move state-6
                                ::transitions
                                11)]
        (t/is (not (contains? (:a (::transitions state-11))
                              :y))
              "The :y transition should be completed and gone")
        (t/is (not (contains? (:a state-11)
                              :y))
              "Any value related to the :y transition should have disappeared")))))




(t/deftest move-seq

  (let [state {::transitions {:x (dsim/transition 0
                                                  10
                                                  on-step)}}]
    (t/is (= (dsim/move-seq state
                            ::transitions
                            (range))
             {:x 1})
          "Transition should be finished and cleaned")))
