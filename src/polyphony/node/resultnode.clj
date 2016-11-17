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

(ns polyphony.node.resultnode
  (:require
   [polyphony.utils :refer [compile-clauses]]
   )
  )

(defrecord ResultNode [id input-id input-status result-clauses compiled-clauses
                       reset-num])

(defn create-result-node
  "Used to create a new result-node"
  [input-id rslt-clauses]
  (ResultNode. (gensym 'R_) input-id false rslt-clauses
               (compile-clauses rslt-clauses) 0)
  )

(defn reset-result-node
  [result-node]
  (assoc result-node :input-status false)
  )

(defn eval-result-clauses
  [result-node]
  (dorun (for [clause (:compiled-clauses result-node)]
           (clause)
           )
         )
  )

(defn- set-result-input-val
  [result-node val reset-num]
  (assoc result-node :input-status val :reset-num reset-num)
  )

(defn set-result-atom-input-val
  [result-node-atom val reset-num]
  ;; Only set input status if the node needs to be reset or
  ;; the input-status is not currently true
  ;; This will only execute result first time input-status
  ;; is set to true
  (when (or (not= reset-num (:reset-num @result-node-atom))
            (not (:input-status @result-node-atom)))
    (let [new-result-node (reset! result-node-atom
                                  (set-result-input-val @result-node-atom
                                                        val
                                                        reset-num))]
      (when (:input-status new-result-node)
        (eval-result-clauses new-result-node))
      )
    )
  )
