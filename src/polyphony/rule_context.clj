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

(ns polyphony.rule-context
  (:require
   [polyphony.node.condnode :refer [reset-cond-node set-cond-output
                                    set-cond-atom-variable
                                    set-cond-num-variables]]
   [polyphony.node.joinnode :refer [reset-join-node]]
   [polyphony.node.resultnode :refer [reset-result-node]]
   [polyphony.utils :refer [sym-to-key]]
   )
  )

(defn rule-context
  []
  (let all-conds (atom {})
       all-joins (atom {})
       all-results (atom {})
       ;; all-variables is a map where keys = variable names as keywords and
       ;;   vals = a list of cond node ids that use the variable
       all-variables (atom {})

       new-variable (fn [cur-variables variable-name node-atom]
                     (assoc cur-variables
                            (keyword (name variable-name))
                            (conj ((keyword (name variable-name)) cur-variables)
                                  node-atom)))


       add-variable (fn [variable-name node-atom]
                      (if node-atom
                        (swap! all-variables
                               new-variable
                               variable-name
                               node-atom))
                      (intern (ns-name 'polyphony.variables)
                              variable-name
                              (atom nil))
                      variable-name
                      )

       set-variable (fn [var-name val reset-num]
                      (dorun (for [output-atom ((sym-to-key var-name)
                                                @all-variables)]
                               (when (.startsWith (name (:id @output-atom)) "C")
                                 (set-cond-atom-variable output-atom
                                                         var-name val
                                                         reset-num)
                                 )
                               ))
                      val
                      )

       get-variable (fn [var-name]
                      ;;  Returns the value af var-name
                      ;; var-name - the variable to return the value for.
                      ;; Returns nil if not set
                      (let [var-node (deref (first ((sym-to-key var-name)
                                                    @all-variables)))]
                        ((sym-to-key var-name) (:variables var-node))
                        )
                      )

       add-cond (fn [new-cond-as-atom]
                  (reset! all-conds (assoc @all-conds
                                           (keyword (:id @new-cond-as-atom))
                                           new-cond-as-atom))
                  )

       get-cond-node (fn [cond-id]
        ((sym-to-key cond-id) @all-conds)
                       )

       add-join (fn [new-join-as-atom]
                  (reset! all-joins (assoc @all-joins
                                           (keyword (:id @new-join-as-atom))
                                           new-join-as-atom))
                  )

       add-result (fn [new-result]
                    (reset! all-results (assoc @all-results
                                               (keyword (:id @new-result))
                                               new-result))
                    )

       find-id-for-clause (fn [clause]
        (let [id-and-clause (first (for [cond-node (map deref (vals @all-conds))
                                         :when (= clause
                                                  (:cond-clause cond-node))]
                                     (list (:id cond-node) clause)))]
          id-and-clause)
                            )

       (fn [m]
         (cond
           (= m :new-variable) new-variable
           (= m :add-variable) add-variable
           (= m :set-variable) set-variable
           (= m :get-variable) get-variable
           (= m :add-cond) add-cond
           (= m :get-cond-node) get-cond-node
           (= m :add-join) add-join
           (= m :add-result) add-result
           (= m :find-id-for-clause) find-id-for-clause
           )
        )

    )
  )
