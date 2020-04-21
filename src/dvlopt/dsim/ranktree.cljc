(ns dvlopt.dsim.ranktree

  ""

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

  ([ranks path v node]

   (-assoc-leaf (tree)
                ranks
                path
                v
                node))


  ([tree [r & rs] path v node]

   (clj/update tree
               r
               (fn at-rank [_]
                 (if rs
                   (-assoc-leaf rs
                                path
                                v
                                node)
                   (-assoc-in node
                              path
                              v))))))




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
                                                v
                                                nil)
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
                                       (-assoc-leaf rs
                                                    path
                                                    v
                                                    node)
                                       (sorted-map 0   node
                                                   r-2 (-assoc-leaf rs-2
                                                                    path
                                                                    v
                                                                    nil)))
                                     (if (= r-2
                                            0)
                                       (sorted-map 0 (assoc-in node
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
                                    v
                                    nil)
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
