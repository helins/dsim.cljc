;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns dvlopt.dsim.test

  {:author "Adam Helinski"}

  (:require [clojure.test                :as t]
            [cognitect.transit           :as transit]
            [dvlopt.dsim                 :as dsim]
            [dvlopt.dsim.transit         :as dsim.transit]
            [dvlopt.fdat                 :as fdat #?(:clj  :refer
                                                     :cljs :refer-macros) [?]]
            [dvlopt.fdat.plugins.transit :as fdat.plugins.transit]
            [dvlopt.rktree.transit       :as rktree.transit])
  #?(:clj (:import (java.io ByteArrayInputStream
                            ByteArrayOutputStream))))


;;;;;;;;;; Transit ser/de for testing that ctxs are indeed serializable


(defn serialize

  "Serializes using Transit."

  [x]

  #?(:clj  (let [out (ByteArrayOutputStream. 512)]
             (transit/write (transit/writer out
                                            :json
                                            (dsim.transit/writer-options))
                            x)
             out)
     :cljs (transit/write (transit/writer :json
                                          (dsim.transit/writer-options))
                          x)))



(defn deserialize

  "Deserializes using Transit."

  [x]

  (transit/read
    (transit/reader #?(:clj (ByteArrayInputStream. (.toByteArray x)))
                    :json
                    {:handlers (dsim.transit/reader-handlers)})
    #?(:cljs x)))



(fdat/register dsim/serializable)


;;;;;;;;;; Utilities for tests


(defn full=

  "= with meta as well."

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


;;;;;;;;;; Events, flows, and utilities needed for testing engines


(def ptime+1

  (dsim/rank+ 1))




(?
 (defn catch-error
 
   ""
 
   [catched]
   
   (dsim/e-dissoc (assoc (or (::dsim/ctx-inner catched)
                             (::dsim/ctx catched))
                         :handled?
                         true))))


(?
 (defn event
 
   ;; Mock event.
 
   [ctx]
 
   ctx))


(?
 (defn event-error

   [ctx]

   (throw (ex-info "Shit happens"
                   {}))))


(?
 (defn -event-inc
 
   [_ctx n _ptime]
 
   (inc n)))


(def event-inc
     (dsim/mirror -event-inc))


(?
 (defn event-inc-rel
 
   ""
    
   [ctx]
 
   (let [[target
          source] (dsim/path ctx)]
     (assoc ctx
            target
            (inc (get ctx
                      source))))))


(?
 (defn event-writer
 
   ([x]

    (? (partial event-writer
                x)))
 
 
   ([x ctx]
  
    (update-in ctx
               (dsim/path ctx)
               conj
               x))))


(?
 (defn flow-infinite
 
   ;; Will be used to test idempotency of sampling as well.
 
   ([n]

    (? (partial flow-infinite
                n)))
 
   ([n ctx]
 
    (let [path  (dsim/path ctx)
          ctx-2 (update-in ctx
                           path
                           inc)]
      (if (< (get-in ctx-2
                     path)
             n)
        (-> ctx-2
            (dsim/sample ptime+1)
            (dsim/sample ptime+1)
            (dsim/sample ptime+1))
        (dsim/end-flow ctx-2))))))


(?
 (defn flow-writer
 
   [ctx]
 
   (dsim/end-flow ((event-writer :b) ctx))))


(?
 (defn stateless-pred?

   ([n]

    (? (partial stateless-pred?
                n)))

   ([n ctx]

    (< (get-in ctx
               (dsim/path ctx))
       n))))


;; <!> Not forgetting to register our functions

(fdat/register [-event-inc
                catch-error
                event
                event-error
                event-inc-rel
                event-writer
                flow-infinite
                flow-writer
                stateless-pred?])


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


(def e-ranks

  [1000])


(def e-path

  [:a :b :c])



(t/deftest e-assoc


  (t/is (= (dsim/queue event)
           (dsim/e-get (dsim/e-assoc {}
                                     event)))
        "In the working queue")


  (t/is (= event
           (dsim/e-get (dsim/e-assoc {}
                                     e-ranks
                                     e-path
                                     event)
                       e-ranks
                       e-path))
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
                             (dsim/e-assoc e-ranks
                                           e-path
                                           event)
                             (dsim/e-conj e-ranks
                                          e-path
                                          event))
                         e-ranks
                         e-path))
          "In the event tree to a function")


    (t/is (= q
             (dsim/e-get (-> {}
                             (dsim/e-assoc e-ranks
                                           e-path
                                           (dsim/queue event))
                             (dsim/e-conj e-ranks
                                          e-path
                                          event))
                         e-ranks
                         e-path))
          "In the event tree to a queue"))


  (t/is (= (dsim/queue event)
           (dsim/e-get (dsim/e-conj {}
                                    e-ranks
                                    e-path
                                    event)
                       e-ranks
                       e-path))
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
                                          e-ranks
                                          e-path
                                          events)
                             e-ranks
                             e-path))
          "In the event tree to nil")


    (t/is (full= (with-meta (dsim/queue event
                                        event
                                        event)
                            mta-2)
                 (dsim/e-get (-> {}
                                 (dsim/e-assoc e-ranks
                                               e-path
                                               event)
                                 (dsim/e-into e-ranks
                                              e-path
                                              events))
                             e-ranks
                             e-path))
          "In the event tree to a function")


    (t/is (full= q-target
                 (dsim/e-get (-> {}
                                 (dsim/e-assoc e-ranks
                                               e-path
                                               q)
                                 (dsim/e-into e-ranks
                                              e-path
                                              events))
                             e-ranks
                             e-path))
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
                                                       e-ranks
                                                       e-path
                                                       q-1)
                                         e-ranks
                                         e-path)
                         e-ranks
                         e-path))
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
                                          e-ranks
                                          e-path
                                          events)
                             e-ranks
                             e-path))
          "In the event tree to nil")


    (t/is (full= (conj events
                       event)
                 (dsim/e-get (-> {}
                                 (dsim/e-assoc e-ranks
                                               e-path
                                               event)
                                 (dsim/e-into e-ranks
                                              e-path
                                              events))
                             e-ranks
                             e-path))
          "In the event tree to a function")


    (t/is (full= q-target
                 (dsim/e-get (-> {}
                                 (dsim/e-assoc e-ranks
                                               e-path
                                               q)
                                 (dsim/e-into e-ranks
                                              e-path
                                              events))
                             e-ranks
                             e-path))
          "In the event tree to a queue")))


;;;;;;;;;; Basic engines


(defn historic

  "Creates a historic engine but ser/de the ctx everytime so that this aspect is tested as well."

  [engine]

  (let [engine-2 (dsim/historic engine)]
    (fn history [ctx]
      (-> ctx
          serialize
          deserialize
          serialize
          deserialize
          engine-2))))



(def history
     (historic (dsim/basic-engine)))



(t/deftest basic-engine

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



(defn eager-periodicity

  "Adds the same event n times with a time interval of 1."

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



(def history-DE
     (historic (dsim/ptime-engine {::dsim/before (fn before [ctx]
                                                   (update ctx
                                                           :before
                                                           inc))
                                   ::dsim/after  (fn after [ctx]
                                                   (update ctx
                                                           :after
                                                           inc))})))


(t/deftest ptime-engine

  (t/is (= '()
           (history-DE nil))
        "History of nothing yields an empty seq")

  (let [n 100
        h (history-DE (eager-periodicity (ctx-init 0)
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
                             event-inc-rel)))]
    (t/is (= 1
             (count h))
          "All events happens at the same ptime despite having different secondary ranks.")

    (t/is (= (assoc (ctx-jump 1)
                    :m
                    2)
             (last h))
          "Respecting order of ranks")))



(t/deftest error-handling

  (let [q-failing   (dsim/queue event-inc
                                event-error
                                event-inc)
        error-mta   {::dsim/on-error catch-error}
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



(t/deftest stop

  (t/is (= (ctx-jump 1)
             (last (history-DE
                     (dsim/e-conj (ctx-init 0)
                                  [1]
                                  [:n]
                                  (dsim/queue event-inc
                                              (dsim/queue (dsim/queue dsim/stop))
                                              event-inc
                                              event-inc)))))
          "Stopping everything"))


;;;;;;;;;; Working queues
;;
;;
;; These functions are not tested explicitly because they are heavily used by other ones we do test
;; or because their implementation is really straightforward :
;;
;;   `wq-capture`
;;   `wq-do!`
;;   `wq-meta`
;;   `wq-vary-meta`
;;


(t/deftest wq-delay

  ;; Tests `rank+` as well.

  (let [delay-1u (dsim/wq-delay ptime+1)
        h        (history-DE (dsim/e-assoc (ctx-init 0)
                                           [1]
                                           [:n]
                                           (dsim/queue event-inc
                                                       delay-1u
                                                       event-inc
                                                       delay-1u
                                                       event-inc)))]

    (t/is (= 3
             (count h))
          "A delay splits the queue into two ptimes everytime")

    (t/is (= (ctx-jump 3)
             (last h))
          "Inducing a delay has the same end result as scheduling everything in advance")))



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
          "Executing dynamicaly an inner queue has the same end result as nesting it in advance")))



(t/deftest wq-replay


  (let [n     10
        pred? (stateless-pred? n)]

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
            "N should be incremented to 10"))


    (let [h (history-DE (dsim/e-conj (ctx-init 0)
                                     [1]
                                     [:n]
                                     (dsim/queue dsim/wq-capture
                                                 event-inc
                                                 (dsim/wq-delay (dsim/rank+ 1))
                                                 (dsim/wq-replay pred?))))]
      (t/is (= 11
               (count h))
            "Every replay happens at a future timepoint + an additional ptime for deciding to stop")

      (t/is (= 10
               (:n (last h)))
            "Adding delays does not impact computation"))))



(t/deftest wq-sreplay

  (t/is (= [:out :in :in :out :out :in :in :out]
           (:writer (last (history-DE (dsim/e-conj (ctx-init 0)
                                                   [1]
                                                   [:writer]
                                                   (dsim/queue dsim/wq-capture
                                                               (event-writer :out)
                                                               dsim/wq-capture
                                                               (event-writer :in)
                                                               (dsim/wq-sreplay dsim/pred-repeat
                                                                                1)
                                                               (event-writer :out)
                                                               (dsim/wq-sreplay dsim/pred-repeat
                                                                                1)))))))
        "An inner loop within an outer one"))


;;;;;;;;;; Flows


(defn test-stability

  [end]

  (t/is (and (not (dsim/scheduled? end))
             (not (dsim/flowing? end)))
        "Context should be stable at the end (no events + no flows)"))



(t/deftest infinite

  (let [n   100
        h   (history-DE (dsim/e-conj (ctx-init 0)
                                     [1]
                                     [:n]
                                     (dsim/infinite (flow-infinite n))))
        end (last h)]

    (t/is (= n
             (:n end))
          "Flow should stop when agreed")

    (t/is (= n
             (count h))
          "Flow is moving through discrete time")

    (test-stability end)))



(t/deftest sampled-finite

  ;; Tests `finite` as well.


  (let [n   100
        h   (history-DE (dsim/e-conj (ctx-init 0)
                                     [0]
                                     [:n]
                                     (dsim/sampled-finite ptime+1
                                                          (dec n)
                                                          event-inc)))
        end (last h)]

    (t/is (= n
             (count h))
          "Is sampled as many time requested, incrementing ptime discretely from 0 to 100")

    (t/is (= n
             (:n end))
          "Flow should end on expected result")

    (test-stability end))


  (let [h (history-DE (dsim/e-conj (ctx-init 0)
                                   [0]
                                   [:writer]
                                   (dsim/queue dsim/wq-capture
                                               (dsim/sampled-finite ptime+1
                                                                    2
                                                                    (event-writer :a))
                                               (dsim/infinite flow-writer)
                                               (dsim/wq-delay ptime+1)
                                               (dsim/sampled-finite ptime+1
                                                                    1
                                                                    (event-writer :c))
                                               (dsim/wq-sreplay dsim/pred-repeat
                                                                2))))]
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



(t/deftest sampler

  ;; Tests `finite` as well.

  (t/is (= (ctx-jump 11)
           (last (history-DE
                   (dsim/e-conj (ctx-init 0)
                                [1]
                                [:n]
                                (dsim/queue (dsim/sampler ptime+1)
                                            (dsim/finite 10
                                                         event-inc))))))
        "Sampler sample when expected"))
