(ns dvlopt.dsim.util

  "Utilities, the user should not be bothered by this."

  {:author "Adam Helinski"
   :no-doc true})




;;;;;;;;;; Handling trees


(defn- -assoc-shortest

  ;; Cf. `assoc-shortest`

  [node [k & ks :as path] leaf]

  (if (seq path)
    (if (associative? node)
      (if (contains? node
                     k)
        (assoc node
               k
               (-assoc-shortest (get node
                                     k)
                                ks
                                leaf))
        (assoc-in node
                  path
                  leaf))
      node)
    leaf))




(defn assoc-shortest

  ""

  [hmap [k & ks :as path] leaf]

  (if (seq path)
    (if (contains? hmap
                   k)
      (assoc hmap
             k
             (-assoc-shortest (get hmap
                                   k)
                              ks
                              leaf))
      (assoc-in hmap
                path
                leaf))
    hmap))




; (defn merge-shortest
; 
;   ""
; 
;   [hmap-1 hmap-2]
; 
;   ;; TODO. #1 When neither is a map, still pick v-2 over v-1?
;   ;;          Shouldn't matter as (= v-1 v-2), by convention, supposedly.
;   ;;
;   ;; TODO. Really useful now there is `assoc-shortest`?
; 
;   (reduce-kv (fn pick-shortest [hmap k v-2]
;                (if (contains? hmap
;                               k)
;                  (if (map? v-2)
;                    (let [v-1 (get hmap
;                                   k)]
;                      (if (map? v-1)
;                        (assoc hmap
;                               k
;                               (merge-shortest v-1
;                                               v-2))
;                        hmap))  ;; #1
;                    (assoc hmap
;                           k
;                           v-2))))
;              hmap-1
;              hmap-2))


