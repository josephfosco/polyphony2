;    Copyright (C) 2016  Joseph Fosco. All Rights Reserved
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

(ns polyphony.rule-graph
  [:require
   [polyphony.utils :refer [sym-to-key]]
   ]
  )

;; variables is a map with variables as keys and their values
;; variables-used is a map where keys = variable names as keywords and
;;   vals = a list of cond node ids that use the variable
(defrecord RuleGraph [all-conds all-joins all-results variables variables-used])

(defn create-rule-graph
  "creates an empty rule graph to store a blank version of the rules"
  []
  (RuleGraph. {} {} {} {} {})
  )

(defn new-variable
  [variables-used-dict variable-name-key node-id]
  (assoc variables-used-dict
         variable-name-key
         (conj (variable-name-key variables-used-dict) node-id)
         )
  )

(defn add-variable
  [rule-graph-atom variable-name node-id]
  (let [rule-graph @rule-graph-atom
        variable-name-key (sym-to-key variable-name)
        ]
    (swap! rule-graph-atom
           assoc
           :variables
           (assoc (:variables rule-graph) variable-name-key nil)
           :variables-used
           (if node-id
             (new-variable (:variables-used rule-graph)
                           variable-name-key
                           node-id)
             (:variables-used rule-graph)
             )
           )
    )
  variable-name
  )

(defn get-var-value
  "Returns the value of variable 'var' from rule-graph"
  [rule-graph var]
  ((sym-to-key var) (:variables rule-graph))
 )
