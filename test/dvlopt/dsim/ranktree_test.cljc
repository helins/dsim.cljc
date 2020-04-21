(ns dvlopt.dsim.ranktree-test

  {:author "Adam Helinski"}

  (:require [clojure.core         :as clj]
            [clojure.test         :as t]
            [dvlopt.dsim.ranktree :as dsim.ranktree])
  (:refer-clojure :exclude [assoc
                            dissoc
                            get
                            pop]))




;;;;;;;;;; Operations with ranks


(t/deftest r+

  (t/are [ranks-1 ranks-2 sum]
         (= sum
            (dsim.ranktree/r+ ranks-1
                              ranks-2))
    [5 5 5]
    [5 5 5]
    [10 10 10]

    [5 5 5]
    [5 5]
    [10 10 5]

    [5 5]
    [5 5 5]
    [10 10 5]

    [5 5]
    []
    [5 5]

    []
    [5 5]
    [5 5]))




;;;;;;;;;; Manipulating trees


(t/deftest assoc

  (t/is (= (sorted-map 1 :after)
           (dsim.ranktree/assoc (sorted-map 1 :before)
                                [1]
                                :after))
        "At an existing rank")

  (t/is (= (sorted-map 1 (sorted-map 2 42))
           (dsim.ranktree/assoc (sorted-map)
                                [1 2]
                                42))
        "Adding ranks")

  (t/is (= (sorted-map 1 (sorted-map 2 (sorted-map 0 24
                                                   3 42)))
           (dsim.ranktree/assoc (sorted-map 1 (sorted-map 2 (sorted-map 3 42)))
                                [1 2]
                                24))
        "Completing ranks")

  (t/is (= (sorted-map 1 (sorted-map 0 42
                                     2 (sorted-map 3 24)))
           (dsim.ranktree/assoc (sorted-map 1 42)
                                [1 2 3]
                                24))
        "Bubbling up unranked levels")
  
  (t/is (= (sorted-map 1 {:a {:b :after}
                          :c :before})
           (dsim.ranktree/assoc (sorted-map 1 {:c :before})
                                [1]
                                [:a :b]
                                :after))
        "With a path"))




(t/deftest dissoc

  ;; Tests `update` as well.

  (t/is (= (sorted-map 2 42)
           (dsim.ranktree/dissoc (sorted-map 0 (sorted-map 1 24)
                                             2 42)
                                 [0 1]))
        "Existing rank")

  (t/is (= (sorted-map 2 42)
           (dsim.ranktree/dissoc (sorted-map 0 (sorted-map 0 24)
                                             2 42)
                                 [0]))
        "Lower order rank, complete ranks using 0")

  (t/is (= (sorted-map 0 (sorted-map 1 24)
                       2 42)
           (dsim.ranktree/dissoc (sorted-map 0 (sorted-map 1 24)
                                             2 42)
                                 [0]))
        "Lower order rank but unable to complete using 0, hence nothing happens")

  (t/is (= (sorted-map 0 (sorted-map 1 42))
           (dsim.ranktree/dissoc (sorted-map 0 (sorted-map 1 42))
                                 [0 1 2 3 4]))
        "Higher order rank, nothing happens")
  
  (t/is (= (sorted-map 2 42)
           (dsim.ranktree/dissoc (sorted-map 0 (sorted-map 1 {:a {:b 42}})
                                             2 42)
                                 [0 1]
                                 [:a :b]))
        "Existing rank with a path"))





(t/deftest get

  (t/is (= 42
           (dsim.ranktree/get (sorted-map 0 (sorted-map 1 42))
                              [0 1]))
        "Existing rank")

  (t/is (= 42
           (dsim.ranktree/get (sorted-map 0 (sorted-map 0 (sorted-map 0 42)))
                              [0]))
        "Completing ranks with 0")

  (t/is (= ::not-found
           (dsim.ranktree/get (sorted-map 0 (sorted-map 1 42))
                              [0 1 2 3]
                              nil
                              ::not-found))
        "Higher order rank, not found")

  (t/is (= 42
           (dsim.ranktree/get (sorted-map 0 (sorted-map 1 {:a {:b 42}}))
                              [0 1]
                              [:a :b]))
        "Existing rank with a path")

  (t/is (= ::not-found
           (dsim.ranktree/get (sorted-map 0 (sorted-map 1 {:a {:b 42}}))
                              [0 1]
                              [:a :b :c]
                              ::not-found))
        "Existing rank with a path, not found"))




(t/deftest pop

  ;; Tests `pop*` as well.

  (t/is (= [(sorted-map 2 (sorted-map 3 24))
            [0 1]
            42]
           (dsim.ranktree/pop (sorted-map 0 (sorted-map 1 42)
                                          2 (sorted-map 3 24))))
        "Simple pop preserves the rest of the tree")

  (t/is (= [nil
            [0 1]
            42]
           (dsim.ranktree/pop (sorted-map 0 (sorted-map 1 42))))
        "Popping a unique entry returns nil as popped tree")

  (t/is (= [nil
            []
            nil]
           (dsim.ranktree/pop (sorted-map)))
        "Nothing to pop"))




(t/deftest pop-walk

  (t/is (= {:events (sorted-map 0 (sorted-map 1 -42))
            :n      42
            :path   [:a :b]
            :ranks  [0 0]}
           (dsim.ranktree/pop-walk {:n 0}
                                   (sorted-map 0 (sorted-map 0 {:a {:b 42}}
                                                             1 -42))
                                   (fn reattach-tree [ctx tree]
                                     (clj/assoc ctx
                                                :events
                                                tree))
                                   (fn f [ctx ranks path node]
                                     (merge ctx
                                            {:n     node
                                             :path  path
                                             :ranks ranks}))))
        "")
  )
