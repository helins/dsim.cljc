(ns dvlopt.dsim.util-test

  {:author "Adam Helinski"}

  (:require [clojure.test     :as t]
            [dvlopt.dsim.util :as dsim.util]))




;;;;;;;;;; Handling trees


(t/deftest assoc-shortest


  (t/is (= {:a {:b :ok}}
           (dsim.util/assoc-shortest {:a {:b 42}}
                                     [:a :b]
                                     :ok))
        "Should behave like regular `assoc-in` when the path lead to a leaf")


  (t/is (= {:a {:b 42}}
           (dsim.util/assoc-shortest {:a {:b 42}}
                                     [:a :b :c]
                                     :fail))
        "Should not do anything when the path exceeds a leaf")


  (t/is (= {:a {:b :ok}}
           (dsim.util/assoc-shortest {:a {:b {:c 42}}}
                                     [:a :b]
                                     :ok))
        "Should assoc when the path is shorther than an existing leaf"))




; (t/deftest merge-shortest
; 
; 
;   (let [tree {:a {:b 42}
;               :c {:d 42}
;               :e {:f {:g 42}}}]
;     (t/is (= (-> tree
;                  (dsim.util/assoc-shortest [:a :b]
;                                           :ok)
;                  (dsim.util/assoc-shortest [:c :d :e-2]
;                                            :fail)
;                  (dsim.util/assoc-shortest [:e :f]
;                                            :ok))
;              (dsim.util/merge-shortest tree
;                                        {:a {:b :ok}
;                                         :c {:d {:e-2 :fail}}
;                                         :e {:f :ok}}))
;           "Should behave exactly like calling `assoc-shortest` repeatedly")))
