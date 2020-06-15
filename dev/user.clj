(ns user

  "For daydreaming in the REPL." 

  (:require [clojure.core.async          :as a]
            [clojure.repl]
            [clojure.test                :as t]
            [cognitect.transit           :as transit]
            [criterium.core              :as C]
            [dvlopt.dsim                 :as dsim]
            [dvlopt.dsim.transit         :as dsim.transit]
            [dvlopt.dsim.util            :as dsim.util]
            [dvlopt.dsim-test            :as dsim-test]
            [dvlopt.dsim.util-test       :as dsim.util-test]
            [dvlopt.fdat                 :as fdat :refer [?]]
            [dvlopt.fdat.track           :as fdat.track]
            [dvlopt.fdat.plugins.nippy   :as fdat.plugins.nippy]
            [dvlopt.fdat.plugins.transit :as fdat.plugins.transit]
            [dvlopt.rktree               :as rktree]
            [dvlopt.rktree.transit       :as rktree.transit]
            [dvlopt.void                 :as void]
            [kixi.stats.core             :as K.S]
            [kixi.stats.distribution     :as K.D]
            [taoensso.nippy              :as nippy]))


;;;;;;;;;;


(require '[nrepl.server])  (defonce server (nrepl.server/start-server :port 4000))


(def run
     (dsim/historic (dsim/ptime-engine)))


(comment


  (fdat/register dsim/serializable)


  )
