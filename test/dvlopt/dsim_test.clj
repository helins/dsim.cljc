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
           (identical? (get-in merged
                               path)
                       :after)
      [:a]
      [:b :c]
      [:b :d]
      [:e :f])))




(defn on-step

  ;;

  [state path progress]

  (assoc-in state
            (rest path)
            progress))




(def state

  ;;

 {::transitions {:a {:x (dsim/transition 0
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
                                     on-step)}})




(t/deftest move

  (let [state-5 (dsim/move state
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

  (t/is (empty? (dsim/move-seq state
                               ::transitions
                               nil))
        "Without any step, the sequence of states should be empty")
  (let [state-seq (dsim/move-seq state
								 ::transitions
								 (range 6))]
	(t/is (every? true?
				  (map (fn equal? [[state-at-step step]]
					     (= state-at-step
							(dsim/move state
									   ::transitions
									   step)))
					   state-seq))
  		  "For those N steps, the current state does not depend on the previous one, hence following a sequence of states should match
           jumping directly from state 0 to state N.")))




(t/deftest move-events

  (t/is (empty? (dsim/move-events state
                                  ::transitions
                                  nil
                                  nil
                                  nil))
        "Without any step, the sequence of states should be empty")
  (t/is (= (dsim/move-events state
                             ::transitions
                             (range)
                             nil
                             nil)
           (dsim/move-seq state
                          ::transitions
                          (range)))
        "Without any event, `move-events` should behave just like `move-seq`")
  (let [events       (for [i (range 5)]
                       {::dsim/step i})
        handle-event (fn handle-event [state event]
                       (assoc state
                              ::value
                              (::dsim/step event)))]
    (t/is (= (last (dsim/move-events {}
                                     ::transitions
                                     (range)
                                     events
                                     handle-event))
             [{::value 4}
              4])
          "Events should be handled regardless of transitions")
    (t/is (= (-> (dsim/move-events {::transitions {:x (dsim/transition 0
                                                                       10
                                                                       on-step)}}
                                   ::transitions
                                   (range)
                                   events
                                   handle-event)
                 last
                 first
                 :x)
             1)
          "The :x transition should finish even though there are less events than the number of steps required for completion")
    (let [[half-done-state
           step]           (last (dsim/move-events {::transitions {:x (dsim/transition 0
                                                                                       3
                                                                                       on-step)}}
                                                   ::transitions
                                                   (range 2)
                                                   events
                                                   handle-event))]
      (t/is (= step
               1)
            "Move should stop at the last step")
      (t/is (< (:x half-done-state)
               1)
            "The transition should not be finished"))))




(t/deftest in-mirror

  (t/is (= (:x (-> {}
                   (dsim/in-mirror ::transitions
                                   [:x]
                                   0
                                   10
                                   (fn map-percent [_state _path percent]
                                     percent))
                   (dsim/move ::transitions
                              10)
                   (dsim/move ::transitions
                              11)))
           1)))
