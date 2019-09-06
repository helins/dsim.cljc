(ns dvlopt.dsim-test

  ""

  {:author "Adam Helinski"}

  (:require [clojure.test :as t]
            [dvlopt.dsim  :as dsim]))




;;;;;;;;;; Utilities


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




;;;;;;;;;; Helpers for transitions


(def mirror-on-step

  (dsim/fn-mirror (fn only-percent [_state _data-path percent]
                    percent)))




(t/deftest fn-assoc-data

  (t/is (= :done
           (-> {dsim/transition-key {:x (dsim/once 0
                                                   5
                                                   mirror-on-step
                                                   (dsim/fn-assoc-data :done))}}
               (dsim/move 6)
               :x))))




(t/deftest fn-on-complete

  (let [on-complete (dsim/fn-on-complete [dsim/remove-subtree
                                          (dsim/fn-assoc-data :after)])]
    (t/is (= {:entity {:x :after}}
             (on-complete {:entity {:x :before}}
                          [:entity :x]
                          nil
                          nil)))))




(t/deftest in-transition?

  (let [transition (dsim/once 0
                              10
                              nil)
        state      {dsim/transition-key {:a transition
                                         :b {:c transition}
                                         :d nil}}]
    (t/are [path]
           (true? (dsim/in-transition? state
                                       path))
      [:a]
      [:b :c])
    (t/is (false? (dsim/in-transition? state
                                       [:d])))))




(t/deftest remove-data

  (t/is (not (contains? (-> {dsim/transition-key {:x (dsim/once 0
                                                                5
                                                                mirror-on-step
                                                                dsim/remove-data)}}
                            (dsim/move 5)
                            (dsim/move 6))
                        :x))))




(t/deftest remove-subtree

  (t/is (not (contains? (-> {:a                  {:y 42}
                             dsim/transition-key {:a {:x (dsim/once 0
                                                                    5
                                                                    mirror-on-step
                                                                    dsim/remove-subtree)}}}
                            (dsim/move 5)
                            (dsim/move 6))
                        :a))))




;;;;;;;;;; Transitions


(defn assoc-completion-step

  ""

  [state _data-path completion-step _step]

  (assoc state
         :completion-step
         completion-step))




(defn only-percents

  [state+steps]

  (map (comp :percent
             first)
       state+steps))




(defn- -test-percent-cycle

  ;;

  [percents]

  (t/are [step percent]
            (= (double percent)
               (double (nth percents
                            step)))
       0  0
       2  0.2
       5  0.5
       7  0.7
       10 1))



(defn- -test-cycle-equality

  [state+steps n-cycles n-steps]

  (t/is (every? (fn equal-cycles? [[cycle-1 cycle-2]]
                  (if cycle-2
                    (= cycle-1
                       cycle-2)
                    true))
                (take (dec n-cycles)
                      (partition 2
                                 (partition n-steps
                                            (map (comp dsim/without-transitions
                                                       first)
                                                 state+steps)))))
        "All repeating cycles should obviously produce the same data"))




(defn- -test-finite-transition

  ;;

  [state+steps n-steps]

  (let [last-state (first (last state+steps))]
    (t/is (= (inc n-steps)
             (count state+steps))
          "There should be N-STEPS states + 1 completion step")
    (t/is (not (dsim/in-transition? last-state))
          "After running all steps, the transition should be cleared")
    (t/is (= n-steps
             (:completion-step last-state))
          "Completion step should be equal the the number of steps (because steps start from 0)")))




(t/deftest infinite

  (let [state+steps (dsim/move-seq {dsim/transition-key {:percent (dsim/infinite 0
                                                                                 11
                                                                                 mirror-on-step)}}
                                   (range))]
    (-test-percent-cycle (only-percents state+steps))
    (-test-cycle-equality state+steps
                          11
                          11)))




(t/deftest once

  (let [state+steps (dsim/move-seq {dsim/transition-key {:percent (dsim/once 0
                                                                             11
                                                                             mirror-on-step
                                                                             assoc-completion-step)}}
                                   (range))
        percents    (only-percents state+steps)
        last-state  (first (last state+steps))]
    (-test-finite-transition state+steps
                             11)
    (-test-percent-cycle percents)))




(t/deftest repeating

  (let [state+steps (dsim/move-seq {dsim/transition-key {:percent (dsim/repeating 0
                                                                                  2
                                                                                  11
                                                                                  mirror-on-step
                                                                                  assoc-completion-step)}}
                                   (range))]
    (-test-finite-transition state+steps
                             22)
    (-test-percent-cycle (only-percents state+steps))
    (-test-cycle-equality state+steps
                          2
                          11)))




(def fn-transitions

  ;;

  (let [fn-on-step  (fn fn-on-step [i-transition]
                      (fn on-step [state _data-path percent]
                        (merge state
                               {:i-transition i-transition
                                :percent      percent})))]
   [(dsim/fn-once 11
                  (fn-on-step 0))
    (dsim/fn-repeating 2
                       11
                       (fn-on-step 1))]))




(t/deftest poly

  (let [state+steps (dsim/move-seq {dsim/transition-key {:x (dsim/poly 0
                                                                       fn-transitions
                                                                       assoc-completion-step)}}
                                   (range))]
    (-test-finite-transition state+steps
                             33)
    (doseq [transition-cycle (partition 11
                                        state+steps)]
      (-test-percent-cycle (only-percents transition-cycle)))
    (-test-cycle-equality (drop 11
                                state+steps)
                          2
                          11)))




(t/deftest poly-infinite

  (let [state+steps (dsim/move-seq {dsim/transition-key {:x (dsim/poly-infinite 0
                                                                                fn-transitions)}}
                                   (range))]
    (-test-cycle-equality state+steps
                          10
                          33)))




(t/deftest poly-repeating

  (let [state+steps (dsim/move-seq {dsim/transition-key {:x (dsim/poly-repeating 0
                                                                                 2
                                                                                 fn-transitions
                                                                                 assoc-completion-step)}}
                                   (range))]
    (-test-finite-transition state+steps
                             66)
    (-test-cycle-equality state+steps
                          2
                          33)))




(t/deftest nested-poly

  (let [state+steps (dsim/move-seq {dsim/transition-key {:x (dsim/poly 0
                                                                       (conj fn-transitions
                                                                             (dsim/fn-poly fn-transitions
                                                                                           assoc-completion-step)))}}
                                   (range))]
    (-test-finite-transition state+steps
                             66)
    (-test-cycle-equality state+steps
                          2
                          33)))
















;;;;;;;;;; Moving states


(def moving-state

  ;;

  (dsim/merge-transitions {}
                          {:a {:x (dsim/once 0
                                             5
                                             mirror-on-step)
                               :y (dsim/once 0
                                             9
                                             mirror-on-step
                                             dsim/remove-data)}
                           :b (dsim/once 0
                                         15
                                         mirror-on-step)}))




(t/deftest move

  (t/is (not (contains? (get (dsim/move {dsim/transition-key {:x nil}}
                                        0)
                             dsim/transition-key)
                        :x))
        "A nil transition should be removed")
  (let [state-5 (dsim/move moving-state
                           4)]
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

  (t/is (empty? (dsim/move-seq moving-state
                               nil))
        "Without any step, the sequence of states should be empty")
  (let [state-seq (dsim/move-seq moving-state
								 (range 5))]
	(t/is (every? true?
				  (map (fn equal? [[state-at-step step]]
					     (= (dsim/without-transitions state-at-step)
							(dsim/without-transitions (dsim/move moving-state
									                             step))))
					   state-seq))
  		  "For those N steps, the current state does not depend on the previous one, hence following a sequence of states should match
           jumping directly from state 0 to state N.")))




(t/deftest move-events

  (t/is (empty? (dsim/move-events moving-state
                                  nil
                                  nil
                                  nil))
        "Without any step, the sequence of states should be empty")
  (t/is (= (dsim/move-events moving-state
                             (range)
                             nil
                             nil)
           (dsim/move-seq moving-state
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
             (-> (dsim/move-events {dsim/transition-key {:x (dsim/once 0
                                                                       10
                                                                       mirror-on-step)}}
                                   (range)
                                   events
                                   handle-event)
                 last
                 first
                 :x))
          "The :x transition should finish even though there are less events than the number of steps required for completion")
    (let [[half-done-state
           step]           (last (dsim/move-events {dsim/transition-key {:x (dsim/once 0
                                                                                       3
                                                                                       mirror-on-step)}}
                                                   (range 2)
                                                   events
                                                   handle-event))]
      (t/is (= 1
               step)
            "Move should stop at the last step")
      (t/is (< (:x half-done-state)
               1)
            "The transition should not be finished"))))
