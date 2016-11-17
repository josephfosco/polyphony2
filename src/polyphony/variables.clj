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

(ns polyphony.variables
  [:require
   [polyphony.node.condnode :refer [set-cond-atom-variable]]
   [polyphony.utils :refer [sym-to-key]]
   ]
  )

;; all-variables is a map where keys = variable names as keywords and
;;   vals = a list of cond node ids that use the variable
(def all-variables (atom {}))

(defn- new-variable
  [cur-variables variable-name node-atom]
  (assoc cur-variables
    (keyword (name variable-name))
    (conj ((keyword (name variable-name)) cur-variables) node-atom))
  )

(defn add-variable
  [variable-name node-atom]
  (if node-atom
    (swap! all-variables new-variable variable-name node-atom))
  (intern (ns-name 'polyphony.variables) variable-name (atom nil))
  variable-name
  )

(defn set-variable
  [var-name val reset-num]
  (dorun (for [output-atom ((sym-to-key var-name) @all-variables)]
           (when (.startsWith (name (:id @output-atom)) "C")
             (set-cond-atom-variable output-atom var-name val reset-num)
             )
           ))
  val
  )

(defn get-variable
  "Returns the value af var-name

   var-name - the variable to return the value for. Returns nil if not set"
  [var-name]
  (let [var-node (deref (first ((sym-to-key var-name) @all-variables)))]
    ((sym-to-key var-name) (:variables var-node))
   )
  )
