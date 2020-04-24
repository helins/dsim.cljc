(ns dvlopt.dsim-test

  {:author "Adam Helinski"}

  (:require [clojure.test :as t]
            [dvlopt.dsim  :as dsim]))




;;;;;;;;;; Utilities for tests


(defn full=

  ;; = with meta as well.

  [a b]

  (and (= a
          b)
       (= (meta a)
          (meta b))))




;;;;;;;;;; Time utilities


(t/deftest millis->utime


  (t/is (= 120
           (dsim/millis->utime 2000
                               60))
        "Simple example, 2 seconds at 60 frames/second is 120 frames")

  (t/is (= 0
           (dsim/millis->utime 0
                               60))
        "Converting 0 milliseconds is always 0")

  (t/is (= 0
           (dsim/millis->utime 2000
                               0))
        "A phenomenon that does not happen does not last"))





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




(def event-inc

  (dsim/wq-mirror (fn event [n _ptime]
                    (inc n))))




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




(def ranks

  [1000])




(def path

  [:a :b :c])




(t/deftest e-assoc


  (t/is (= (dsim/queue event)
           (dsim/e-get (dsim/e-assoc {}
                                     event)))
        "In the working queue")


  (t/is (= event
           (dsim/e-get (dsim/e-assoc {}
                                     ranks
                                     path
                                     event)
                       ranks
                       path))
        "In the event tree"))




(t/deftest e-conj


  (let [q (dsim/queue event
                      event)]


    (t/is (= q
             (dsim/e-get (-> {}
                             (dsim/e-assoc event)
                             (dsim/e-conj event))))
          "In the working queue")


    (t/is (= q
             (dsim/e-get (-> {}
                             (dsim/e-assoc ranks
                                           path
                                           event)
                             (dsim/e-conj ranks
                                          path
                                          event))
                         ranks
                         path))
          "In the event tree to a function")


    (t/is (= q
             (dsim/e-get (-> {}
                             (dsim/e-assoc ranks
                                           path
                                           (dsim/queue event))
                             (dsim/e-conj ranks
                                          path
                                          event))
                         ranks
                         path))
          "In the event tree to a queue"))


  (t/is (= (dsim/queue event)
           (dsim/e-get (dsim/e-conj {}
                                    ranks
                                    path
                                    event)
                       ranks
                       path))
        "In the event tree to nil"))




(t/deftest e-into


  (let [mta-1    {:a 42}
        q        (with-meta (dsim/queue event)
                            mta-1)
        mta-2    {:b 42}
        events   (with-meta [event
                             event]
                            mta-2)
        q-target (with-meta (dsim/queue event
                                        event
                                        event)
                            (merge mta-1
                                   mta-2))]


    (t/is (full= q-target
                 (dsim/e-get (-> {}
                                 (dsim/e-assoc q)
                                 (dsim/e-into events))))
          "In the working queue")


    (t/is (full= (with-meta (into (dsim/queue)
                                  events)
                            mta-2)
                 (dsim/e-get (dsim/e-into {}
                                          ranks
                                          path
                                          events)
                             ranks
                             path))
          "In the event tree to nil")


    (t/is (full= (with-meta (dsim/queue event
                                        event
                                        event)
                            mta-2)
                 (dsim/e-get (-> {}
                                 (dsim/e-assoc ranks
                                               path
                                               event)
                                 (dsim/e-into ranks
                                              path
                                              events))
                             ranks
                             path))
          "In the event tree to a function")


    (t/is (full= q-target
                 (dsim/e-get (-> {}
                                 (dsim/e-assoc ranks
                                               path
                                               q)
                                 (dsim/e-into ranks
                                              path
                                              events))
                             ranks
                             path))
          "In the event tree to a queue")))




(t/deftest e-isolate


  (let [q-1 (dsim/queue (dsim/queue event))
        q-2 (dsim/queue q-1)]

    (t/is (= q-2
             (dsim/e-get (dsim/e-isolate (dsim/e-assoc {}
                                                       q-1))))
          "In the working queue")

    (t/is (= q-2
             (dsim/e-get (dsim/e-isolate (dsim/e-assoc {}
                                                       ranks
                                                       path
                                                       q-1)
                                         ranks
                                         path)
                         ranks
                         path))
          "In the event tree")))




(t/deftest e-push


  (let [mta-1    {:a 42}
        q        (with-meta (dsim/queue event)
                            mta-1)
        mta-2    {:b 42}
        events   (with-meta (dsim/queue event
                                        event)
                            mta-2)
        q-target (with-meta (dsim/queue event
                                        event
                                        event)
                            (merge mta-2
                                   mta-1))]


    (t/is (full= q-target
                 (dsim/e-get (-> {}
                                 (dsim/e-assoc q)
                                 (dsim/e-push events))))
          "In the working queue")


    (t/is (full= events
                 (dsim/e-get (dsim/e-into {}
                                          ranks
                                          path
                                          events)
                             ranks
                             path))
          "In the event tree to nil")


    (t/is (full= (conj events
                       event)
                 (dsim/e-get (-> {}
                                 (dsim/e-assoc ranks
                                               path
                                               event)
                                 (dsim/e-into ranks
                                              path
                                              events))
                             ranks
                             path))
          "In the event tree to a function")


    (t/is (full= q-target
                 (dsim/e-get (-> {}
                                 (dsim/e-assoc ranks
                                               path
                                               q)
                                 (dsim/e-into ranks
                                              path
                                              events))
                             ranks
                             path))
          "In the event tree to a queue")))




;;;;;;;;;; Basic engines


(def history

  (dsim/historic (dsim/engine)))




(t/deftest engine

  (t/is (= '()
           (history nil))
        "History of nothing yields an empty sequence")

  (let [h (history (-> {:n 0}
                       (dsim/e-conj [0 0]
                                    [:n]
                                    event-inc)
                       (dsim/e-conj [42 0 5]
                                    [:n]
                                    (dsim/queue event-inc
                                                event-inc))))]

    (t/is (= 2
             (count h))
          "3 events in 2 series")

    (t/is (= {:n 3}
             (last h))
          "Incremented thrice")

    (t/is (= [1 3]
             (map :n
                  h))
          "Respecting order of events")))



;;;;;;;;;; Discrete-Event Engines


(defn ctx-init

  [n]

  {:after  n
   :before n
   :n      n
   :writer []})




(defn ctx-jump

  [n]

  (assoc (ctx-init n)
         ::dsim/ptime
         n))




(defn periodic

  ;; Adds the same event n times with a time interval of 1.

  [ctx path n ranks event]

  (reduce (fn add-event [ctx-2 n]
            (dsim/e-conj ctx-2
                         (into [n]
                               ranks)
                         path
                         event))
          ctx
          (range 1
                 (inc n))))




(def ptime-options

  {::dsim/before (fn before [ctx]
                   (update ctx
                           :before
                           inc))
   ::dsim/after  (fn after [ctx]
                   (update ctx
                           :after
                           inc))})


(def history-DE

  (dsim/historic (dsim/engine-ptime ptime-options)))




(defn infinite-flow

  ;;

  ([n]

   (fn flow [ctx]
     (infinite-flow ctx
                    n)))


  ([ctx n]

   (let [path  (dsim/path ctx)
         ctx-2 (update-in ctx
                          path
                          inc)]
     (if (< (get-in ctx-2
                    path)
            n)
       (dsim/f-sample ctx-2
                      (dsim/wq-ptime+ ctx-2
                                      1))
       (dsim/f-end ctx-2)))))




(defn flow-write-b

  [ctx]

  (dsim/f-end ((event-writer :b) ctx)))




(def history-DE-op

  (dsim/historic (dsim/engine-ptime (assoc ptime-options
                                           ::dsim/handler
                                           (dsim/op-applier {::inc           event-inc
                                                             ::infinite-flow infinite-flow
                                                             ::pred?         (fn pred? [ctx n]
                                                                               (< (get-in ctx
                                                                                          (dsim/path ctx))
                                                                                  n))
                                                             ::write-b       flow-write-b
                                                             ::writer        event-writer})))))




(t/deftest engine-ptime

  (t/is (= '()
           (history-DE nil))
        "History of nothing yields an empty seq")

  (let [n 100
        h (history-DE (periodic (ctx-init 0)
                                [:n]
                                n
                                nil
                                event-inc))]

    (t/is (= n
             (count h))
          "Respecting the number of planned events")

    (t/is (= (map ctx-jump
                  (range 1
                         (inc n)))
             (map #(dissoc %
                           ::dsim/events)
                  h))
          "State evolve exactly following each planned ptime")

    (t/is (not (dsim/scheduled? (last h)))
          "All events have been executed at the end of a history"))


  (let [h (history-DE
            (-> (ctx-init 0)
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
                                                  source))))))))]
    (t/is (= 1
             (count h))
          "All events happens at the same ptime")

    (t/is (= (assoc (ctx-jump 1)
                    :m
                    2)
             (last h))
          "Respecting ranks"))

  (t/is (= (ctx-jump 1)
           (last (history-DE
                   (dsim/e-conj (ctx-init 0)
                                [1]
                                [:n]
                                (dsim/queue event-inc
                                            (dsim/queue (dsim/queue dsim/stop))
                                            event-inc
                                            event-inc)))))
        "Stopping everything")


  (let [q-failing   (dsim/queue event-inc
                                (fn error [_ctx]
                                  (throw (ex-info "Shit happens"
                                                  {})))
                                event-inc)
        error-mta   {::dsim/on-error (fn catch-error [catched]
                                       (println :catched)
                                       (dsim/e-stop (assoc (or (::dsim/ctx-inner catched)
                                                               (::dsim/ctx catched))
                                                           :handled?
                                                           true)))}
        after-error (assoc (ctx-jump 1)
                           :handled?
                           true)]
    (t/is (= after-error
             (last (history-DE
                     (dsim/e-conj (ctx-init 0)
                                  [1]
                                  [:n]
                                  (with-meta q-failing
                                             error-mta)))))
          "Error handling")

    (t/is (= after-error
             (last (history-DE
                     (dsim/e-conj (ctx-init 0)
                                  [1]
                                  [:n]
                                  (dsim/queue (dsim/queue (with-meta (dsim/queue q-failing)
                                                                     error-mta))
                                              event-inc)))))
          "Error handling of nested queues + removing the stack")))




;;;;;;;;;; Working queues
;;
;;
;; These functions are not tested explicitly because they are heavily used by other ones we do test
;; or because their implementation is really straightforward :
;;
;;   `wq-capture`
;;   `wq-do!`
;;   `wq-meta`
;;   `wq-mirror`
;;   `wq-vary-meta`
;;


(defn test-op-h

  [h op-h]

  (let [without-events (fn remove-events [h]
                         (map #(dissoc %
                                       ::dsim/events)
                              h))]
    (t/is (= (without-events h)
             (without-events op-h))
          "Operational and functional histories are equal without their events")))




(t/deftest wq-delay

  ;; Tests `wq-ptime+` as well.

  (let [delay-1u    (dsim/wq-delay (dsim/wq-ptime+ 1))
        h           (history-DE (dsim/e-assoc (ctx-init 0)
                                              [1]
                                              [:n]
                                              (dsim/queue event-inc
                                                          delay-1u
                                                          event-inc
                                                          delay-1u
                                                          event-inc)))
        op-delay-1u [::dsim/wq-delay [::dsim/wq-ptime+ 1]]]

    (t/is (= 3
             (count h))
          "A delay splits the queue into two ptimes everytime")

    (t/is (= (ctx-jump 3)
             (last h))
          "Inducing a delay has the same end result as scheduling everything in advance")

    (test-op-h h
               (history-DE-op (dsim/e-assoc (ctx-init 0)
                                            [1]
                                            [:n]
                                            (dsim/queue [::inc]
                                                        op-delay-1u
                                                        [::inc]
                                                        op-delay-1u
                                                        [::inc]))))))




(t/deftest wq-exec


  (let [q-inner (dsim/queue event-inc
                            event-inc)
        h       (history-DE (dsim/e-conj (ctx-init 0)
                                          [0]
                                          [:n]
                                          (dsim/queue (dsim/wq-exec q-inner))))]

    (t/is (= (history-DE (dsim/e-conj (ctx-init 0)
                                      [0]
                                      [:n]
                                      (dsim/queue q-inner)))
             h)
          "Executing dynamicaly an inner queue has the same end result as nesting it in advance")

    (test-op-h h
               (history-DE-op (dsim/e-conj (ctx-init 0)
                                           [0]
                                           [:n]
                                           (dsim/queue [::dsim/wq-exec (dsim/queue [::inc]
                                                                                   [::inc])]))))))




(t/deftest wq-replay


  (let [n     10
        pred? (fn pred? [ctx]
                (< (get-in ctx
                           (dsim/path ctx))
                   n))]

    (let [h (history-DE (dsim/e-conj (ctx-init 0)
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
            "N should be incremented to 10")

      (test-op-h h
                 (history-DE-op (dsim/e-conj (ctx-init 0)
                                             [1]
                                             [:n]
                                             (dsim/queue [::dsim/wq-capture]
                                                         [::inc]
                                                         [::dsim/wq-replay [::pred? n]])))))


    (let [h (history-DE (dsim/e-conj (ctx-init 0)
                                     [1]
                                     [:n]
                                     (dsim/queue dsim/wq-capture
                                                 event-inc
                                                 (dsim/wq-delay (dsim/wq-ptime+ 1))
                                                 (dsim/wq-replay pred?))))]
      (t/is (= 11
               (count h))
            "Every replay happens at a future timepoint + an additional ptime for deciding to stop")

      (t/is (= 10
               (:n (last h)))
            "Adding delays does not impact computation")

      (test-op-h h
                 (history-DE-op (dsim/e-conj (ctx-init 0)
                                             [1]
                                             [:n]
                                             (dsim/queue [::dsim/wq-capture]
                                                         [::inc]
                                                         [::dsim/wq-delay [::dsim/wq-ptime+ 1]]
                                                         [::dsim/wq-replay [::pred? n]])))))))




(t/deftest wq-sreplay

  (t/is (= [:out :in :in :out :out :in :in :out]
           (:writer (last (history-DE (dsim/e-conj (ctx-init 0)
                                                   [1]
                                                   [:writer]
                                                   (dsim/queue dsim/wq-capture
                                                               (event-writer :out)
                                                               dsim/wq-capture
                                                               (event-writer :in)
                                                               (dsim/wq-sreplay dsim/wq-pred-repeat
                                                                                1)
                                                               (event-writer :out)
                                                               (dsim/wq-sreplay dsim/wq-pred-repeat
                                                                                1))))))
           (:writer (last (history-DE-op (dsim/e-conj (ctx-init 0) 
                                                      [1]
                                                      [:writer]
                                                      (dsim/queue [::dsim/wq-capture]
                                                                  [::writer :out]
                                                                  [::dsim/wq-capture]
                                                                  [::writer :in]
                                                                  [::dsim/wq-sreplay [::dsim/wq-pred-repeat]
                                                                                     1]
                                                                  [::writer :out]
                                                                  [::dsim/wq-sreplay [::dsim/wq-pred-repeat]
                                                                                     1]))))))
        "An inner loop within an outer one"))




;;;;;;;;;; flows


(defn test-stability

  [end]

  (t/is (and (not (dsim/scheduled? end))
             (not (dsim/flowing? end)))
        "Context should be stable at the end (no events + no flows)"))




(t/deftest f-infinite

  (let [n   100
        h   (history-DE (dsim/e-conj (ctx-init 0)
                                     [1]
                                     [:n]
                                     (dsim/f-infinite (infinite-flow n))))
        end (last h)]

    (t/is (= n
             (:n end))
          "Flow should stop when agreed")

    (t/is (= n
             (count h))
          "Flow is moving through discrete time")

    (test-stability end)

    (t/is (= (map :n
                  h)
             (map :n
                  (history-DE-op (dsim/e-conj (ctx-init 0)
                                              [1]
                                              [:n]
                                              [::dsim/f-infinite [::infinite-flow n]]))))
          "Operation behaves like function.")))




(def ptime+1

  (dsim/wq-ptime+ 1))


(def op-ptime+1

  [::dsim/wq-ptime+ 1])




(t/deftest f-sampled

  ;; Tests `f-finite` as well.


  (let [n   100
        h   (history-DE (dsim/e-conj (ctx-init 0)
                                     [0]
                                     [:n]
                                     (dsim/f-sampled ptime+1
                                                     (dec n)
                                                     event-inc)))
        end (last h)]

    (t/is (= n
             (count h))
          "Is sampled as many time requested, incrementing ptime discretely from 0 to 100")

    (t/is (= n
             (:n end))
          "Flow should end on expected result")

    (test-stability end)
    
    (t/is (= (map :n
                  h)
             (map :n
                  (history-DE-op (dsim/e-conj (ctx-init 0)
                                              [0]
                                              [:n]
                                              [::dsim/f-sampled op-ptime+1
                                                                (dec n)
                                                                [::inc]]))))
          "Operation behave like function."))


  (let [h (history-DE (dsim/e-conj (ctx-init 0)
                                   [0]
                                   [:writer]
                                   (dsim/queue dsim/wq-capture
                                               (dsim/f-sampled ptime+1
                                                               2
                                                               (event-writer :a))
                                               (dsim/f-infinite flow-write-b)
                                               (dsim/wq-delay ptime+1)
                                               (dsim/f-sampled ptime+1
                                                               1
                                                               (event-writer :c))
                                               (dsim/wq-sreplay dsim/wq-pred-repeat
                                                                2))))
        op-h (history-DE-op (dsim/e-conj (ctx-init 0)
                                         [0]
                                         [:writer]
                                         (dsim/queue [::dsim/wq-capture]
                                                     [::dsim/f-sampled op-ptime+1
                                                                       2
                                                                       [::writer :a]]
                                                     [::dsim/f-infinite [::write-b]]
                                                     [::dsim/wq-delay op-ptime+1]
                                                     [::dsim/f-sampled op-ptime+1
                                                                       1
                                                                       [::writer :c]]
                                                     [::dsim/wq-sreplay [::dsim/wq-pred-repeat]
                                                                        2])))]
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
                  h)
             (map :writer
                  op-h))
          "Respecting the timing of transitions between flows")))
