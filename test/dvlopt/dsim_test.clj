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




(t/deftest deep-merge

  (let [merged (dsim/deep-merge {:a :before
                                 :b {:c :before
                                     :d {:e :before}}}
                                {:a :after
                                 :b {:c :after
                                     :d :after}
                                 :e {:f :after}})]
    (t/are [path]
           (identical? :after
                       (get-in merged
                               path))
      [:a]
      [:b :c]
      [:b :d]
      [:e :f])))




(defn on-step

  ;;

  [_state _path percent]

  percent)



(t/deftest repeating-transition

  (let [states       (dsim/move-seq {dsim/transition-key {:n-cycles (dsim/repeating-transition 3
                                                                                               0
                                                                                               10
                                                                                               (fn [state data-path percent]
                                                                                                 (if (= percent
                                                                                                        1)
                                                                                                   (update-in state
                                                                                                              data-path
                                                                                                              inc)
                                                                                                   state)))}
                                     :n-cycles 0}
                                    (range))
        cycle-counts (map (comp :n-cycles
                                first)
                          states)]
    (t/is (= 0
             (nth cycle-counts
                  5))
          "Transition should be still in the first cycle")
    (t/is (= 1
             (nth cycle-counts
                  9))
          "First cycle should be completed")
    (t/is (= 2
             (nth cycle-counts
                  19))
          "Second cycle should be completed")
    (t/is (= 3
             (nth cycle-counts
                  29))
          "Third and last cycle should be completed")
    (t/is (= 31
             (count states))
          "There should be 31 steps, 3 x 10 + 1 for `on-complete`")
    (t/is (not (dsim/transitions? (first (last states))))
          "When finished, the transition should be removed")))




(def state

  ;;

  (-> {}
      (dsim/in-mirror [:a :x]
                      0
                      5
                      on-step)
      (dsim/in-mirror [:a :y]
                      0
                      10
                      on-step
                      (fn on-complete [state data-path _step]
                        (dsim/dissoc-in state
                                        data-path)))
      (dsim/in-mirror [:b]
                      0
                      15
                      on-step)))




(t/deftest move

  (let [state-5 (dsim/move state
                           5)]
    (t/is (= 1
             (:x (:a state-5)))
          "Value of :x should reflect that the transition is complete")
    (t/is (= 0.5
             (double (:y (:a state-5))))
          "Value of :y should reflecht that the transition is half-done")
    (let [state-6 (dsim/move state-5
                             6)
          a-6     (get-in state-6
                          (dsim/transition-path [:a]))]
      (t/is (not (contains? a-6
                            :x))
            "The :x transition should be completed and gone")
      (t/is (contains? a-6
                       :y)
            "The :y transition should still be going on")
      (t/is (contains? (get state-6
                            dsim/transition-key)
                       :b)
            "The :b transition should still be going on")
      (let [state-11 (dsim/move state-6
                                11)]
        (t/is (not (contains? (get-in state-11
                                      (dsim/transition-path [:a]))
                              :y))
              "The :y transition should be completed and gone")
        (t/is (not (contains? (:a state-11)
                              :y))
              "Any value related to the :y transition should have disappeared")))))




(t/deftest move-seq

  (t/is (empty? (dsim/move-seq state
                               nil))
        "Without any step, the sequence of states should be empty")
  (let [state-seq (dsim/move-seq state
								 (range 6))]
	(t/is (every? true?
				  (map (fn equal? [[state-at-step step]]
					     (= state-at-step
							(dsim/move state
									   step)))
					   state-seq))
  		  "For those N steps, the current state does not depend on the previous one, hence following a sequence of states should match
           jumping directly from state 0 to state N.")))




(t/deftest move-events

  (t/is (empty? (dsim/move-events state
                                  nil
                                  nil
                                  nil))
        "Without any step, the sequence of states should be empty")
  (t/is (= (dsim/move-events state
                             (range)
                             nil
                             nil)
           (dsim/move-seq state
                          (range)))
        "Without any event, `move-events` should behave just like `move-seq`")
  (let [events       (for [i (range 5)]
                       {dsim/step-key i})
        handle-event (fn handle-event [state event]
                       (assoc state
                              ::value
                              (get event
                                   dsim/step-key)))]
    (t/is (= [{::value 4}
              4]
             (last (dsim/move-events {}
                                     (range)
                                     events
                                     handle-event)))
          "Events should be handled regardless of transitions")
    (t/is (= 1
             (-> (dsim/move-events (dsim/in-mirror {}
                                                   [:x]
                                                   0
                                                   10
                                                   on-step)
                                   (range)
                                   events
                                   handle-event)
                 last
                 first
                 :x))
          "The :x transition should finish even though there are less events than the number of steps required for completion")
    (let [[half-done-state
           step]           (last (dsim/move-events (dsim/in-mirror {}
                                                                   [:x]
                                                                   0
                                                                   3
                                                                   on-step)
                                                   (range 2)
                                                   events
                                                   handle-event))]
      (t/is (= 1
               step)
            "Move should stop at the last step")
      (t/is (< (:x half-done-state)
               1)
            "The transition should not be finished"))))




(t/deftest remove-mirror

  (t/is {}
        (dsim/remove-mirror (-> (dsim/in-mirror {}
                                                [:x]
                                                0
                                                5
                                                on-step)
                                (dsim/move 3))
                            [:x]
                            true)))




(t/deftest remove-data

  (t/is (not (contains? (-> (dsim/in-mirror {}
                                            [:x]
                                            0
                                            5
                                            on-step
                                            dsim/remove-data)
                            (dsim/move 5)
                            (dsim/move 6))
                        :x))))




(t/deftest remove-entity

  (t/is (not (contains? (-> (dsim/in-mirror {:a {:y 42}}
                                            [:a :x]
                                            0
                                            5
                                            on-step
                                            dsim/remove-entity)
                            (dsim/move 5)
                            (dsim/move 6))
                        :a))))




(t/deftest fn-assoc-data

  (t/is (= :done
           (-> (dsim/in-mirror {}
                               [:x]
                               0
                               5
                               on-step
                               (dsim/fn-assoc-data :done))
               (dsim/move 6)
               :x))))




(t/deftest fn-on-complete

  (let [on-complete (dsim/fn-on-complete [dsim/remove-entity
                                          (dsim/fn-assoc-data :after)])]
    (t/is (= {:entity {:x :after}}
             (on-complete {:entity {:x :before}}
                          [:entity :x]
                          nil)))))
