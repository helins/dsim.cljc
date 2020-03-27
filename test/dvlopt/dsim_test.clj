(ns dvlopt.dsim-test

  {:author "Adam Helinski"}

  (:require [clojure.test                    :as t]
            [clojure.test.check.clojure-test :as tt]
            [clojure.test.check.generators   :as gen]
            [clojure.test.check.properties   :as prop]
            [dvlopt.dsim                     :as dsim]))




;;;;;;;;;; Handling trees


(t/deftest dissoc-in


  (t/is (= {:d 42}
           (dsim/dissoc-in {:a {:b {:c 42}}
                            :d 42}
                           [:a :b :c]))
        "Dissoc'ing only at the path and nothing else")


  (t/is (= {:a {:e 42}}
           (dsim/dissoc-in {:a {:b {:c {:d 42}}
                                :e 42}}
                           [:a :b :c :d]))
        "Recursively dissoc empty map only")


  (let [tree {:a {:b {:c 42}}}]
    (t/is (= tree
             (dsim/dissoc-in tree
                             [:a :b :c :d]))
          "Dissoc'ing a path deeper than the tree does not do anything because there is nothing to dissoc"))


  (t/is (= {:a {:b 42}}
           (dsim/dissoc-in {:a {:b 42
                                :c {:d 42}}}
                           [:a :c]))
        "Dissoc'ing a branch does not do anything else")


  (t/is (= {:a {:b 42}}
           (dsim/dissoc-in {:a {:b 42}}
                           [:c :d]))
        "Dissoc'ing a non-existing path does not do anything")


  (t/is (= {:a 42}
           (dsim/dissoc-in {:a 42}
                           []))
        "Dissoc'ing an empty path does not do anything")


  (t/is (= {}
           (dsim/dissoc-in {}
                           [:a :b]))
        "Dissoc'ing an empty map does not do anything"))




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
      [:e :f]))


  (t/is (= {:a {:b :after}}
           (dsim/deep-merge {:a {:b :before
                                 :c {:d :before}}}
                            {:a {:b :after
                                 :c {:d nil}}}))
        "Nil/empty values are recursively dissoc'ed"))




(t/deftest assoc-shortest


  (t/is (= {:a {:b :ok}}
           (dsim/assoc-shortest {:a {:b 42}}
                                [:a :b]
                                :ok))
        "Should behave like regular `assoc-in` when the path lead to a leaf")


  (t/is (= {:a {:b 42}}
           (dsim/assoc-shortest {:a {:b 42}}
                                [:a :b :c]
                                :fail))
        "Should not do anything when the path exceeds a leaf")


  (t/is (= {:a {:b :ok}}
           (dsim/assoc-shortest {:a {:b {:c 42}}}
                                [:a :b]
                                :ok))
        "Should assoc when the path is shorther than an existing leaf"))




(t/deftest merge-shortest


  (let [tree {:a {:b 42}
              :c {:d 42}
              :e {:f {:g 42}}}]
    (t/is (= (-> tree
                 (dsim/assoc-shortest [:a :b]
                                      :ok)
                 (dsim/assoc-shortest [:c :d :e-2]
                                      :fail)
                 (dsim/assoc-shortest [:e :f]
                                      :ok))
             (dsim/merge-shortest tree
                                  {:a {:b :ok}
                                   :c {:d {:e-2 :fail}}
                                   :e {:f :ok}}))
          "Should behave exactly like calling `assoc-shortest` repeatedly")))




;;;;;;;;;; Time utilities


(t/deftest millis->utime


  (t/is (= 120
           (dsim/millis->utime 2000
                               60))
        "Simple example, 2 seconds at 60 frames/second is 120 frames"))




;;;;;;;;;; Scaling numerical values


(t/deftest scale


  (t/are [percent scaled]
         (= (double scaled)
            (double (dsim/scale 0
                                100
                                percent)))
    0    0
    0.25 25
    0.5  50
    1    100)


  (t/are [value scaled]
         (= (double scaled)
            (double (dsim/scale 0
                                1000
                                0
                                1000
                                value)))
    0    0
    250  250
    500  500
    1000 1000)


  (t/are [value scaled]
         (= (double scaled)
            (double (dsim/scale 0
                                1000
                                0
                                100
                                value)))
    0   0
    25  250
    50  500
    100 1000)


  (t/are [value scaled]
         (= (double scaled)
            (double (dsim/scale 0
                                100
                                0
                                1000
                                value)))
    0    0
    250  25
    500  50
    1000 100)


  (t/are [value scaled]
         (= (double scaled)
            (double (dsim/scale -100
                                100
                                0
                                1000
                                value)))
    0    -100
    250  -75
    500  -50
    1000 0)


  (t/are [value scaled]
         (= (double scaled)
            (double (dsim/scale 0
                                100
                                0
                                -1000
                                value)))
    0     0
    -250  25
    -500  50
    -1000 100))




(t/deftest minmax-norm


  (t/are [value norm]
         (= (double norm)
            (double (dsim/minmax-norm 0
                                      100
                                      value)))
    0   0
    25  0.25
    50  0.5
    100 1))




;;;;;;;;; Generalities about contextes


(t/deftest empty-event?


  (t/is (true? (dsim/empty-event? nil))
        "Nil is empty")

  (t/is (true? (dsim/empty-event? (dsim/queue)))
        "An empty queue has nothing to execute")

  (t/is (true? (dsim/empty-event? (dsim/queue (dsim/queue))))
        "Nested empty queues have nothing to execute")

  (t/is (false? (dsim/empty-event? (fn event [ctx])))
        "A function is not empty")

  (t/is (false? (dsim/empty-event? (dsim/queue (fn event [ctx]))))
        "A queue with a function is not empty")

  (t/is (false? (dsim/empty-event? (dsim/queue (dsim/queue (fn event [ctx])))))
        "Nested queues with a function are not empty"))




(t/deftest reached?

  ;; Tests `ptime` as well.

  (let [ctx {::dsim/ptime 10}]
    (t/is (true? (dsim/reached? ctx
                                10)))
    (t/is (true? (dsim/reached? ctx
                                5)))
    (t/is (false? (dsim/reached? ctx
                                 15))))


  (let [ctx {::dsim/e-flat {::dsim/ptime 10}}]
    (t/is (true? (dsim/reached? ctx
                                10)))
    (t/is (true? (dsim/reached? ctx
                                5)))
    (t/is (false? (dsim/reached? ctx
                                 15)))))




;;;;;;;;;; Adding, removing, and modifying events
;;
;;
;; These functions are not tested explicitly because they are heavily used by other ones we do test
;; or because their implementation is really straightforward :
;;
;;   `e-dissoc`
;;   `e-get`
;;   `e-update`
;;


(defn event

  ;; Mock event.

  [ctx]

  ctx)




(def timevec

  [1000])




(def path

  [:a :b :c])




(t/deftest e-assoc


  (t/is (= (dsim/queue event)
           (dsim/e-get (dsim/e-assoc dsim/ctx
                                        event)))
        "In the working queue")


  (t/is (= event
           (dsim/e-get (dsim/e-assoc dsim/ctx
                                     timevec
                                     path
                                     event)
                       timevec
                       path))
        "In the event tree"))




(t/deftest e-conj


  (let [q (dsim/queue event
                      event)]


    (t/is (= q
             (dsim/e-get (-> dsim/ctx
                             (dsim/e-assoc event)
                             (dsim/e-conj event))))
          "In the working queue")


    (t/is (= q
             (dsim/e-get (-> dsim/ctx
                             (dsim/e-assoc timevec
                                           path
                                           event)
                             (dsim/e-conj timevec
                                          path
                                          event))
                         timevec
                         path))
          "In the event tree to a function")


    (t/is (= q
             (dsim/e-get (-> dsim/ctx
                             (dsim/e-assoc timevec
                                           path
                                           (dsim/queue event))
                             (dsim/e-conj timevec
                                          path
                                          event))
                         timevec
                         path))
          "In the event tree to a queue"))


  (t/is (= (dsim/queue event)
           (dsim/e-get (dsim/e-conj dsim/ctx
                                    timevec
                                    path
                                    event)
                       timevec
                       path))
        "In the event tree to nil"))




(defn full=

  ;; = with meta as well.

  [a b]

  (and (= a
          b)
       (= (meta a)
          (meta b))))




(t/deftest e-into


  (let [mta-1    {:a 42}
        q        (with-meta (dsim/queue)
                            mta-1)
        mta-2    {:b 42}
        events   (with-meta [event
                             event]
                            mta-2)
        q-target (with-meta (dsim/queue event
                                        event)
                            (merge mta-1
                                   mta-2))]


    (t/is (full= q-target
                 (dsim/e-get (-> dsim/ctx
                                 (dsim/e-assoc q)
                                 (dsim/e-into events))))
          "In the working queue")


    (t/is (full= (with-meta (into (dsim/queue)
                                  events)
                            mta-2)
                 (dsim/e-get (dsim/e-into dsim/ctx
                                          timevec
                                          path
                                          events)
                             timevec
                             path))
          "In the event tree to nil")


    (t/is (full= (with-meta (dsim/queue event
                                        event
                                        event)
                            mta-2)
                 (dsim/e-get (-> dsim/ctx
                                 (dsim/e-assoc timevec
                                               path
                                               event)
                                 (dsim/e-into timevec
                                              path
                                              events))
                             timevec
                             path))
          "In the event tree to a function")


    (t/is (full= q-target
                 (dsim/e-get (-> dsim/ctx
                                 (assoc-in (dsim/e-path timevec
                                                        path)
                                           q)
                                 (dsim/e-into timevec
                                              path
                                              events))
                             timevec
                             path))
          "In the event tree to a queue")))




(t/deftest e-isolate


  (let [q-1 (dsim/queue (dsim/queue event))
        q-2 (dsim/queue q-1)]

    (t/is (= q-2
             (dsim/e-get (dsim/e-isolate (dsim/e-assoc dsim/ctx
                                                       q-1))))
          "In the working queue")

    (t/is (= q-2
             (dsim/e-get (dsim/e-isolate (dsim/e-assoc dsim/ctx
                                                       timevec
                                                       path
                                                       q-1)
                                         timevec
                                         path)
                         timevec
                         path))
          "In the event tree")))




(t/deftest e-pop


  (let [q (dsim/queue event)]

    (t/is (empty? (dsim/e-get (dsim/e-pop (dsim/e-assoc dsim/ctx
                                                        q))))
          "In the working queue")

    (t/is (empty? (dsim/e-get (dsim/e-pop (dsim/e-assoc dsim/ctx
                                                        timevec
                                                        path
                                                        q)
                                          timevec
                                          path)
                              timevec
                              path))
          "In the event tree")))




(t/deftest e-push


  (let [mta-1    {:a 42}
        q        (with-meta (dsim/queue)
                            mta-1)
        mta-2    {:b 42}
        events   (with-meta (dsim/queue event
                                        event)
                            mta-2)
        q-target (with-meta (dsim/queue event
                                        event)
                            (merge mta-2
                                   mta-1))]


    (t/is (full= q-target
                 (dsim/e-get (-> dsim/ctx
                                 (dsim/e-assoc q)
                                 (dsim/e-push events))))
          "In the working queue")


    (t/is (full= events
                 (dsim/e-get (dsim/e-into dsim/ctx
                                          timevec
                                          path
                                          events)
                             timevec
                             path))
          "In the event tree to nil")


    (t/is (full= (conj events
                       event)
                 (dsim/e-get (-> dsim/ctx
                                 (dsim/e-assoc timevec
                                               path
                                               event)
                                 (dsim/e-into timevec
                                              path
                                              events))
                             timevec
                             path))
          "In the event tree to a function")


    (t/is (full= q-target
                 (dsim/e-get (-> dsim/ctx
                                 (assoc-in (dsim/e-path timevec
                                                        path)
                                           q)
                                 (dsim/e-into timevec
                                              path
                                              events))
                             timevec
                             path))
          "In the event tree to a queue")))




;;;;;;;;;; Timevecs


(t/deftest timevec+

  (t/is (= [1 1 1]
           (dsim/timevec+ [0 0 0]
                          [1 1 1]))
        "Same number of dimensions")

  (t/is (= [1 1 1]
           (dsim/timevec+ [0 0]
                          [1 1 1]))
        "Dtimevec has more dimensions")

  (t/is (= [1 1 1]
           (dsim/timevec+ [0 0 1]
                          [1 1 0]))
        "Timevec has more dimensions")

  (t/is (thrown? Throwable
                 (dsim/timevec+ [0 0 0]
                                [-1]))
        "Adding a negative ptime will throw"))




;;;;;;;;;; Moving a context through time


(def event-inc

  (dsim/wq-mirror (fn event [n _ptime]
                    (inc n))))



(defn init-ctx

  [n]

  (merge dsim/ctx
         {:after  n
          :before n
          :n      n}))



(defn jump-ctx

  [n]

  (assoc (init-ctx n)
         ::dsim/ptime
         n))




(def jump-options

  {::dsim/before-ptime #(update %
                                :before
                                inc)
   ::dsim/after-ptime  #(update %
                                :after
                                inc)})




(defn discrete-time-event

  ;;

  [ctx path n rank event]

  (reduce (fn add-event [ctx-2 n]
            (dsim/e-conj ctx-2
                         [n rank]
                         path
                         event))
          ctx
          (range 1
                 (inc n))))



(defn discrete-time-ctx

  ;; Initialize a ctx advancing discreetly and incrementing at `path`.

  [path n]

  (discrete-time-event (init-ctx 0)
                       path
                       n
                       0
                       event-inc))




(t/deftest jump

  ;; Testing both `jump` and `jump-to-end`, both are implemented using `jump-until`.


  (t/is (= (init-ctx 0)
           (dsim/jump (init-ctx 0)
                      jump-options)
           (dsim/jump-to-end (init-ctx 0)
                             jump-options))
        "Nothing happens when there is no event scheduled")


  (let [ctx (dsim/e-conj (init-ctx 0)
                         [1]
                         [:n]
                         event-inc)
        end (dsim/jump-to-end ctx
                              jump-options)]

    (t/is (= (jump-ctx 1)
             (dsim/jump ctx
                        jump-options)
             end)
          "Jumping to the end is like jumpting to the next ptime when there is only one")

    (t/is (not (dsim/scheduled? end))
          "Context should be stable at the end"))


  (let [n   100
        ctx (discrete-time-ctx [:n]
                               n)]
    (t/is (= (jump-ctx n)
             (reduce (fn single-jump [ctx-2 _]
                       (dsim/jump ctx-2
                                  jump-options))
                     ctx
                     (range n))
             (dsim/jump-to-end ctx
                               jump-options))
          "Jumping event by event is like jumping to the end"))


  (let [ctx (-> (init-ctx 0)
                (dsim/e-conj [1 0]
                             [:n]
                             event-inc)
                (dsim/e-conj [1 1]
                             [:m :n]
                             (fn event [ctx]
                               (let [[target
                                      source] (dsim/path ctx)]
                                 (assoc ctx
                                        target
                                        (inc (get ctx
                                                  source)))))))]
    (t/is (= (assoc (jump-ctx 1)
                    :m
                    2)
             (dsim/jump ctx
                        jump-options)
             (dsim/jump-to-end ctx
                               jump-options))
          "Respecting ranks"))


  (t/is (= {:pred true}
           (dsim/jump-until (dsim/e-conj (init-ctx 0)
                                         [1]
                                         [:n]
                                         event-inc)
                            (fn pred [ctx _ptime-last _ptime-next]
                              {:pred true})
                            jump-options))))




(defn test-every-ptime

  ;; Test every single historic ptime.

  [init-ctx h]

  (some? (reduce (fn test-ptime [ctx-2 historic-ptime]
                     (let [ctx-3 (dsim/jump ctx-2
                                            jump-options)]
                       (if (= ctx-3
                              historic-ptime)
                         ctx-3
                         (reduced nil))))
                   init-ctx
                   h)))




(t/deftest history


  (t/is (empty? (dsim/history (init-ctx 0)
                              jump-options))
        "Nothing happens if there is no event scheduled")


  (let [n   100
        ctx (discrete-time-ctx [:n]
                               n)
        h   (dsim/history ctx
                          jump-options)]


    (t/is (not (dsim/scheduled? (last h)))
          "Context should be stable at the end")


    (t/is (= (jump-ctx 100)
             (dsim/jump-to-end ctx
                               jump-options)
             (last h))
          "The last historic ptime is the same as the result of jumping to the end")


    (t/is (test-every-ptime ctx
                            h)
          "Should contain each historic ptime, a lazy equivalent of earger repeated calls"))


  (let [n   100
        ctx (-> (discrete-time-ctx [:n]
                                   n)
                (discrete-time-event [:m]
                                     n
                                     1
                                     event-inc)
                (assoc :m
                       0))
        h  (dsim/history ctx
                         jump-options)]

    (t/is (= n
             (count h))
          "100 x 2 events per ptime on different ranks = 100 historical ptimes")

    (t/is (test-every-ptime ctx
                            h)
          "Lazy and eager jumps should agree on what constitutes a single ptime")))




;;;;;;;;;; Working queues
;;
;;
;; These functions are not tested explicitly because they are heavily used by other ones we do test
;; or because their implementation is really straightforward :
;;
;;   `wq-breaker`
;;   `wq-capture`
;;   `wq-conj`
;;   `wq-copy`
;;   `wq-do!`
;;   `wq-meta`
;;   `wq-mirror`
;;   `wq-vary-meta`
;;


(t/deftest wq-delay


  (let [delay-1u (dsim/wq-delay (dsim/wq-timevec+ [1]))
        h        (dsim/history (dsim/e-assoc (init-ctx 0)
                                             [1]
                                             [:n]
                                             (dsim/queue event-inc
                                                         delay-1u
                                                         event-inc
                                                         delay-1u
                                                         event-inc))

                               jump-options)]

    (t/is (= 3
             (count h))
          "A delay splits the queue into two ptimes everytime")

    (t/is (= (jump-ctx 3)
             (last h))
          "Inducing a delay has the same end result as scheduling everything in advance")))




(t/deftest wq-execute


  (let [q-inner (dsim/queue event-inc
                            event-inc)]
    (t/is (= (dsim/history (dsim/e-conj (init-ctx 0)
                                        [0]
                                        [:n]
                                        (dsim/queue q-inner))
                           jump-options)
             (dsim/history (dsim/e-conj (init-ctx 0)
                                        [0]
                                        [:n]
                                        (dsim/queue (dsim/wq-execute q-inner)))
                           jump-options))
          "Executing dynamicaly an inner queue has the same end result as nesting it in advance")))









(t/deftest wq-replay


  (let [n     10
        pred? (fn pred? [ctx]
                (< (get-in ctx
                           (dsim/path ctx))
                   n))]

    (let [h (dsim/history (dsim/e-conj (init-ctx 0)
                                       [1]
                                       [:n]
                                       (dsim/queue dsim/wq-capture
                                                   event-inc
                                                   (dsim/wq-replay pred?))))]
      (t/is (= 1
               (count h))
            "Everything should be replayed during the same ptime")

      (t/is (= 10
               (:n (last h)))
            "N should be incremented to 10"))


    (let [h (dsim/history (dsim/e-conj (init-ctx 0)
                                       [1]
                                       [:n]
                                       (dsim/queue dsim/wq-capture
                                                   event-inc
                                                   (dsim/wq-delay (dsim/wq-timevec+ [1]))
                                                   (dsim/wq-replay pred?))))]
      (t/is (= 11
               (count h))
            "Every replay happens at a future timepoint + an additional ptime for deciding to stop")

      (t/is (= 10
               (:n (last h)))
            "Adding delays does not impact computation"))))




(defn event-writer

  ([x]

   (fn event [ctx]
     (event-writer ctx
                   x)))


  ([ctx x]
 
   (update-in ctx
              (dsim/path ctx)
              conj
              x)))




(t/deftest wq-sreplay

  (t/is (= [:out :in :in :out :out :in :in :out]
           (:writer (dsim/jump-to-end (-> dsim/ctx
                                          (dsim/e-conj [1]
                                                       [:writer]
                                                       (dsim/queue dsim/wq-capture
                                                                   (event-writer :out)
                                                                   dsim/wq-capture
                                                                   (event-writer :in)
                                                                   (dsim/wq-sreplay dsim/wq-pred-repeat
                                                                                    1)
                                                                   (event-writer :out)
                                                                   (dsim/wq-sreplay dsim/wq-pred-repeat
                                                                                    1)))
                                          (assoc :writer
                                                 [])))))
        "An inner loop within an outer one"))




;;;;;;;;;; flows


(defn test-stability

  [end]

  (t/is (and (not (dsim/scheduled? end))
             (not (dsim/flowing? end)))
        "Context should be stable at the end (no events + no flows)"))




(t/deftest f-infinite

  (let [n   100
        h   (dsim/history (dsim/e-conj (init-ctx 0)
                                       [1]
                                       [:n]
                                       (dsim/f-infinite (fn flow [ctx]
                                                          (let [path  (dsim/path ctx)
                                                                ctx-2 (update-in ctx
                                                                                 path
                                                                                 inc)]
                                                            (if (< (get-in ctx-2
                                                                           path)
                                                                   n)
                                                              (dsim/f-sample ctx-2
                                                                             (dsim/wq-timevec+ ctx
                                                                                               [1]))
                                                              (dsim/f-end ctx-2)))))))
        end (last h)]

    (t/is (= n
             (:n end))
          "Flow should stop when agreed")

    (t/is (= n
             (count h))
          "Flow is moving through discrete time")

    (test-stability end)))




(def timevec+1

  (dsim/wq-timevec+ [1]))




(t/deftest f-sampled

  ;; Tests `f-finite` as well.


  (let [n   100
        h   (dsim/history (dsim/e-conj (init-ctx 0)
                                       [0]
                                       [:n]
                                       (dsim/f-sampled timevec+1
                                                       (dec n)
                                                       (fn flow [ctx]
                                                         (update-in ctx
                                                                    (dsim/path ctx)
                                                                    inc)))))
        end (last h)]

    (t/is (= n
             (count h))
          "Is sampled as many time requested, incrementing ptime discretely from 0 to 100")

    (t/is (= n
             (:n end))
          "Flow should end on expected result")

    (test-stability end))


  (let [h (dsim/history (dsim/e-conj (assoc (init-ctx 0)
                                            :writer
                                            [])
                                     [0]
                                     [:writer]
                                     (dsim/queue dsim/wq-capture
                                                 (dsim/f-sampled timevec+1
                                                                 2
                                                                 (event-writer :a))
                                                 (dsim/f-infinite (fn flow [ctx]
                                                                    (dsim/f-end ((event-writer :b) ctx))))
                                                 (dsim/wq-delay timevec+1)
                                                 (dsim/f-sampled timevec+1
                                                                 1
                                                                 (event-writer :c))
                                                 (dsim/wq-sreplay dsim/wq-pred-repeat
                                                                  2))))]

    (t/is (= (apply concat
                    (repeat 3
                            [:a :a :a :b :c :c]))
             (:writer (last h)))
          "Mixing and repeating flows should be flawless")

    (t/is (= [[:a]
              [:a :a]
              [:a :a :a :b]
              [:a :a :a :b :c]
              [:a :a :a :b :c :c :a]
              [:a :a :a :b :c :c :a :a]
              [:a :a :a :b :c :c :a :a :a :b]
              [:a :a :a :b :c :c :a :a :a :b :c]
              [:a :a :a :b :c :c :a :a :a :b :c :c :a]
              [:a :a :a :b :c :c :a :a :a :b :c :c :a :a]
              [:a :a :a :b :c :c :a :a :a :b :c :c :a :a :a :b]
              [:a :a :a :b :c :c :a :a :a :b :c :c :a :a :a :b :c]
              [:a :a :a :b :c :c :a :a :a :b :c :c :a :a :a :b :c :c]]
             (map :writer
                  h))
          "Respecting the timing of transitions between flows")))












(comment




;;;;;;;;;; Transitions


(defn assoc-completion-step

  ;;

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

  ;;

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

  (let [state+steps (dsim/move-seq {dsim/transition-key {:x (dsim/poly {}
                                                                       0
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

  (let [state+steps (dsim/move-seq {dsim/transition-key {:x (dsim/poly-infinite {}
                                                                                0
                                                                                fn-transitions)}}
                                   (range))]
    (-test-cycle-equality state+steps
                          10
                          33)))




(t/deftest poly-repeating

  (let [state+steps (dsim/move-seq {dsim/transition-key {:x (dsim/poly-repeating {}
                                                                                 0
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

  (let [state+steps (dsim/move-seq {dsim/transition-key {:x (dsim/poly {}
                                                                       0
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
    (t/is (= 1.0
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
    (t/is (= 1.0
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




;;;;;;;;;; Priority maps


(defn priority-symbol

  [priority]

  (symbol (str "s"
               priority)))




(tt/defspec with-priority

  ;; Ensuring a priority is well assigned to a symbol.

  (prop/for-all [priority gen/int]
    (= (dsim/priority (dsim/with-priority (priority-symbol priority)
                                          priority))
       priority)))




(tt/defspec priority-map

  ;; Ensuring keys are being sorted by priorities.

  (prop/for-all [priorities (gen/vector gen/int)]
    (= (vals (reduce (fn add-key [priority-map priority]
                       (assoc priority-map
                              (dsim/with-priority (priority-symbol priority)
                                                  priority)
                              priority))
                     dsim/priority-map
                     priorities))
       (not-empty (sort (into #{}
                             priorities))))))



)
