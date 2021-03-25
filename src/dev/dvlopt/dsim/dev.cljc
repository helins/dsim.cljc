;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns dvlopt.dsim.dev

  "For daydreaming in the REPL." 

  (:require [cognitect.transit           :as transit]
            [dvlopt.dsim                 :as dsim]
            [dvlopt.dsim.transit         :as dsim.transit]
            [dvlopt.dsim.util            :as dsim.util]
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


(def run
     (dsim/historic (dsim/ptime-engine)))


(comment


  (fdat/register dsim/serializable)


  )
