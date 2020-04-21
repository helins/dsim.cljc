(ns dvlopt.dsim.ranktree

  "A ranktree is composed of arbitrary subtrees (regualar nested maps) that are prioritized by arbitrarily nested sorted maps.
  
   In other words, each leaf is identified first by a vector of ranks (keys of the sorted maps) and then by a path in the
   unsorted maps. Yet in other words, those are sorted trees which have unsorted trees as leaves.
  
   For instance:

   ```clojure
   (def tree-2
        (sorted-map 0 (sorted-map 1 {:a {:b 42}})
                    5 {:c {:d {:e 24}}}))

   (= 42
      (ranktree/get tree
                    [0 1]
                    [:a :b]))
   ```

   It must start with at least one sorted map.

   The magic is that is it perfectly normal to mix rank vectors of various length. The functions in this namespace automatically
   treat missing ranks as 0 and know how to be smart about it. A lower rank means a higher priority.

   ```Clojure
   (ranktree/assoc tree-2
                   [0 1 0 0 0 5]
                   [:possible?]
                   true)

   (= 42
      ;; 
      (ranktree/get tree-2
                    [0 1 0 0 0 0]
                    [:a :b])
      ;;
      (ranktree/get tree-2
                    [0 1]
                    [:a :b]))

                
   ```
  "

  {:author "Adam Helinski"}

  (:require [clojure.core :as clj]
            [dvlopt.void  :as void])
  (:refer-clojure :exclude [assoc
                            dissoc
                            get
                            pop
                            update]))




;;;;;;;;;; Gathering all declarations


(declare update)




;;;;;;;;;; Creating new trees


(defn tree

  ""

  []

  (sorted-map))




;;;;;;;;;; Operations with ranks


(defn r+

  "Adds all dimension in `dtimevec` to `timevec`
  
   The first dimension in `dtimevec` denoting a ptime cannot be negative as one cannot travel
   back in time.
  
   ```clojure
   (timevec+ [0 5]
             [10 10 10])

   [10 15 10]
   ```"

  ;; TODO. Optimize for consecutive 0's ?

  [ranks-1 ranks-2]

  (let [n-1      (count ranks-1)
        n-2      (count ranks-2)
        [base
         [front
          rear]] (if (<= n-1
                         n-2)
                   [ranks-1
                    (split-at n-1
                              ranks-2)]
                   [ranks-2
                    (split-at n-2
                              ranks-1)])]
    (into (mapv +
                base
                front)
          rear)))




;;;;;;;;;; Manipulating trees


(defn- -assoc-in

  ;;

  ([path v]

   (-assoc-in {}
              path
              v))


  ([hmap path v]

   (if (empty? path)
     v
     (assoc-in hmap
               path
               v))))




(defn- -assoc-leaf

  ;;

  [[r & rs] path v]

  (sorted-map r (if rs
                  (-assoc-leaf rs
                               path
                               v)
                  (-assoc-in path
                             v))))




(defn- -bubbling-assoc-leaf

  ;;

  [[r & rs] path v bubbling-node]

  (if (empty? rs)
    (if (= r
           0)
      (-assoc-in bubbling-node
                 path
                 v)
      (sorted-map 0 bubbling-node
                  r
                  (-assoc-in path
                             v)))
    (sorted-map r (-bubbling-assoc-leaf rs
                                        path
                                        v
                                        bubbling-node))))




(defn- -assoc

  ""

  [tree [r & rs] path v]

  (clj/update tree
              r
              (fn at-rank [node]
                (cond
                  (nil? node)    (if rs
                                   (-assoc-leaf rs
                                                path
                                                v)
                                   (-assoc-in path
                                              v))
                  (sorted? node) (if rs
                                   (-assoc node
                                           rs
                                           path
                                           v)
                                   (-assoc node
                                           [0]
                                           path
                                           v))
                  :else          (if-some [r-2 (first rs)]
                                   (if-some [rs-2 (next rs)]
                                     (if (= r-2
                                            0)
                                       (-bubbling-assoc-leaf rs
                                                             path
                                                             v
                                                             node)
                                       (sorted-map 0   node
                                                   r-2 (-assoc-leaf rs-2
                                                                    path
                                                                    v)))
                                     (if (= r-2
                                            0)
                                       (sorted-map 0 (-assoc-in node
                                                                path
                                                                v))
                                       (sorted-map 0   node
                                                   r-2 (-assoc-in path
                                                                  v))))
                                   ;; TODO. Throw if not map? Clojure's `assoc-in` does not and is cryptic about it.
                                   (-assoc-in node
                                              path
                                              v))))))




(defn assoc

  ""

  ([tree ranks v]

   (assoc tree
          ranks
          nil
          v))


  ([tree ranks path v]

   (if (seq ranks)
     (-assoc tree
             ranks
             path
             v)
     tree)))




(defn dissoc

  ""

  ([tree ranks]

   (dissoc tree
           ranks
           nil))


  ([tree ranks path]

   (if (seq ranks)
     (update tree
             ranks
             nil
             (fn dissoc-in [node]
               (when (seq path)
                 (not-empty (void/dissoc-in node
                                            path)))))
     tree)))




(defn- -get

  ;;

  [tree [r & rs] path not-found]

  (let [node (clj/get tree
                      r)]
    (if (sorted? node)
      (if rs
        (recur node
               rs
               path
               not-found)
        (recur node
               [0]
               path
               not-found))
      (if rs
        not-found
        (get-in node
                path
                not-found)))))




(defn get

  ""

  ([tree ranks]

   (get tree
        ranks
        nil))


  ([tree ranks path]

   (get tree
        ranks
        path
        nil))


  ([tree ranks path not-found]

   (if (seq ranks)
     (-get tree
           ranks
           path
           not-found)
     tree)))




(defn pop*

  ""

  [node ranks-prefix]

  (if (and (map? node)
           (sorted? node))
    (if-some [[k
               node-next] (first node)]
      (clj/update (pop* node-next
                        (conj ranks-prefix
                              k))
                  0
                  (fn rebuild-tree [subtree]
                    (if subtree
                      (clj/assoc node
                                 k
                                 subtree)
                      (not-empty (clj/dissoc node
                                             k)))))
      [nil
       ranks-prefix
       nil])
    [nil
     ranks-prefix
     node]))




(defn pop

  ""

  [tree]

  (pop* tree
        []))




(defn- -pop-walk

  ;;

  [state ranks path node f]

  (if (map? node)
    (reduce-kv (fn deeper [state-2 k node-next]
                 (-pop-walk state-2
                            ranks
                            (conj path
                                  k)
                            node-next
                            f))
               state
               node)
    (f state
       ranks
       path
       node)))




(defn pop-walk*

  ""

  [ctx tree reattach-tree ranks-prefix f]

  (if (sorted? tree)
    (let [[tree-2
           ranks
           node]  (pop* tree
                        ranks-prefix)]
     (if (nil? node)
       ctx
       (-pop-walk (reattach-tree ctx
                                 tree-2)
                  ranks
                  []
                  node
                  f)))
    (-pop-walk (reattach-tree ctx
                              nil)
               ranks-prefix
               []
               tree
               f)))




(defn pop-walk

  ""

  [ctx tree reattach-tree f]

  (pop-walk* ctx
             tree
             reattach-tree
             []
             f))




(defn- -update

  ;; Cf. [[update]]
  ;;
  ;; A bit fugly because of the nested ifs, but works perfectly.

  [tree [r & rs :as ranks] path f]

  (void/update tree
               r
               (fn at-rank [node]
                 (if node
                   (if rs
                     (if (sorted? node)
                       (not-empty (-update node
                                           rs
                                           path
                                           f))
                       node)
                     (if (sorted? node)
                       (if (contains? node
                                      0)
                         (not-empty (-update node
                                             [0]
                                             path
                                             f))
                         node)
                       (if (seq path)
                         (not-empty (void/update-in node
                                                    path
                                                    f))
                         (f node))))
                   (when-some [v (f nil)]
                     (if rs
                       (-assoc-leaf rs
                                    path
                                    v)
                       (-assoc-in path
                                  v)))))))




(defn update

  ""

  ([tree ranks f]

   (update tree
           ranks
           nil
           f))


  ([tree ranks path f]

   (if (seq ranks)
     (-update tree
              ranks
              path
              f)
     tree)))
