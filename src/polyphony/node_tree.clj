;    Copyright (C) 2015-2016  Joseph Fosco. All Rights Reserved
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns polyphony.node-tree
  (:require
   [polyphony.node.condnode :refer [reset-cond-node set-cond-output
                                    set-cond-num-variables]]
   [polyphony.node.joinnode :refer [reset-join-node]]
   [polyphony.node.resultnode :refer [reset-result-node]]
   [polyphony.utils :refer [sym-to-key]]
   )
  )

(def all-conds (atom {}))
(def all-joins (atom {}))
(def all-results (atom {}))

(defn add-cond
  [new-cond-as-atom]
  (reset! all-conds (assoc @all-conds (keyword (:id @new-cond-as-atom)) new-cond-as-atom))
  )

(defn get-cond-node
  [cond-id]
  ((sym-to-key cond-id) @all-conds)
  )

(defn add-join
  [new-join-as-atom]
  (reset! all-joins (assoc @all-joins (keyword (:id @new-join-as-atom)) new-join-as-atom))
  )

(defn add-result
  [new-result]
  (reset! all-results (assoc @all-results (keyword (:id @new-result)) new-result))
  )

(defn find-id-for-clause
  [clause]
  (let [id-and-clause (first (for [cond-node (map deref (vals @all-conds))
                        :when (= clause (:cond-clause cond-node))]
                    (list (:id cond-node) clause)))]
    id-and-clause)
  )
