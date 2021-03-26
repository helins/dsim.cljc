;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns dvlopt.dsim.util

  "Utilities, the user should not be bothered by this."

  {:author "Adam Helinski"
   :no-doc true})


;;;;;;;;;; Collections


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

  ;; Associates `leaf` at `path` only if `path` is shorter than the tree. 
  ;;
  ;; Cf. Tests

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



(defn pop-stack

  ;; Given a stack in a map, pops and element and dissociates the stack if it is now empty.

  [hmap k]

  (if-some [stack (not-empty (pop (get hmap
                                       k)))]
    (assoc hmap
           k
           stack)
    (dissoc hmap
            k)))
