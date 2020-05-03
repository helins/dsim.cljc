(ns dvlopt.dsim.transit

  "Provides what is necessary to use Transit, which is pretty much mandatory for sharing between
   Clojure and Clojurescript.
  
   Out of the box, Transit does not provide handlers for sorted maps and queues, even less when it comes
   to functions."

  {:author "Adam Helinski"}

  (:require [cognitect.transit           :as transit]
            [dvlopt.dsim                 :as dsim]
            [dvlopt.fdat.plugins.transit :as fdat.plugins.transit]
            [dvlopt.rktree.transit       :as rktree.transit])
  #?(:clj (:import clojure.lang.PersistentQueue)))




;;;;;;;;;;


(defn reader-handlers

  "Map of handlers for readers."

  ([]

   (merge (fdat.plugins.transit/handler-in)
          rktree.transit/read-handler
          {"queue" (transit/read-handler (fn deserialize [x]
                                           (into (dsim/queue)
                                                 x)))})))




(defn writer-options

  "Options for writers. Contains `:handlers` but also the `:transform`.
  
   <!> Attention <!>

   Due to some weird dependency shenanigans, for Clojurescript, the user
   must also add Transit-JS, otherwise things will break. Indeed, for the time
   being, Transit-CLJS does not pull the latest version of Transit-JS as it
   should."

  ([]

   (-> (fdat.plugins.transit/writer-options)
       (update :handlers
                merge
                rktree.transit/write-handler
                {PersistentQueue (transit/write-handler (constantly "queue")
                                                        vec)})
       (update :transform
               (partial comp
                        transit/write-meta)))))
