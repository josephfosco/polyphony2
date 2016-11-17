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

(ns polyphony.node.condnode
  (:require
   [polyphony.node.joinnode :refer [set-join-atom-input-val]]
   [polyphony.node.resultnode :refer [set-result-atom-input-val]]
   [polyphony.utils :refer [compile-clauses is-variable? log-to-console sym-to-key]]
   )
  )

(defrecord CondNode [id cond-clause compiled-clause num-variables variables
                     outputs reset-num])

(defn create-cond-node
  "Used to create a new cond-node

   id-and-clause - a list with the first element the id for the
                   clause, and the second element the clause"
  [id-and-clause]
  (log-to-console "create-cond-node: " id-and-clause)
  (CondNode. (first id-and-clause) (second id-and-clause)
             (compile-clauses (list (second id-and-clause))) nil {} () 0)
  )

(defn reset-cond-node
  [cond-node]
  (assoc cond-node :variables {})
  )

(defn- eval-cond-node
  " eval compiled cond clause
   Returns: true or false

  "
  [cond-node]
  (if ((first (:compiled-clause cond-node)))
    true
    false)
  )

(defn- send-output-val
  [cond-node val reset-num]
  (dorun (for [output-node (:outputs cond-node)]
           (cond (.startsWith (name (:id @output-node)) "J")
                 (set-join-atom-input-val output-node (:id cond-node) val reset-num)
                 (.startsWith (name (:id @output-node)) "R")
                 (set-result-atom-input-val output-node val reset-num)
                 :else
                 (throw (Throwable. "InvalidOutputNode"))
                 )
           ))
 )

(defn set-cond-output
  [cond-node output-node]
  (assoc cond-node :outputs (conj (:outputs cond-node) output-node ))
  )

(defn set-cond-num-variables
  [cond-node vars]
  (assoc cond-node :num-variables (count vars))
  )

(defn- set-cond-variable
  [cond-node var-name var-val reset-num]
  (if (= reset-num (:reset-num cond-node))
    (assoc cond-node
      :variables
      (assoc (:variables cond-node)
        (keyword var-name) var-val))
    (assoc cond-node
      :reset-num reset-num
      :variables (hash-map (keyword var-name) var-val))
    )
  )

(defn set-cond-atom-variable
  [cond-node-atom var-name var-val reset-num]
  (let [new-cond-node (reset! cond-node-atom
                              (set-cond-variable @cond-node-atom
                                                 var-name var-val
                                                 reset-num))]
    (when (= (count (:variables new-cond-node)) (:num-variables new-cond-node))
      (send-output-val new-cond-node (eval-cond-node new-cond-node) reset-num)
      )
    )
  )
