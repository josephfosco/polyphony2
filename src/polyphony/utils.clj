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

(ns polyphony.utils)

(def ^:private log-to-console? (atom false))

(defn log-to-console
  [& msgs]
  (when @log-to-console?
    (binding [*print-readably* nil]
      (apply prn msgs)
      ))
  )

(defn set-log-to-console?
  [val]
  (reset! log-to-console? val)
  )

(defn is-variable?
  [var-name]
  (and (= (type var-name) clojure.lang.Symbol)
       (= \? (first (name var-name))))
  )

(defn sym-to-key
  [sym]
  (keyword (name sym))
  )

(defn check-atom
  [elem]
  (log-to-console "check-atom: " elem)
  (if (is-variable? elem)
    `(deref ~(symbol (str "polyphony.variables/" (name elem))))
    elem
    ))

(declare clause-to-fn)
(defn subst-atoms-for-vars
  [clause]
  (log-to-console "subst-atoms-for-vars clause: " clause)
  (doall (for [elem clause]
           (cond (= (type elem) java.lang.String) (str "\"" elem "\"")
                 (seq? elem) (clause-to-fn elem)
                 :else (check-atom elem))
           ))
  )

(defn clause-to-fn
  [clause]
  (log-to-console "clause-to-fn: " clause)
  (let [clause-with-atoms (subst-atoms-for-vars clause)
        new-clause (if (= (first clause) 'set-var)
                     (list 'fn [] (conj (rest clause) 'polyphony.core/set-var))
                     (list 'fn [] clause-with-atoms)
                     )
        ]
    (log-to-console "clause-to-fn with-atoms: " clause-with-atoms (= (first clause)
                                                                     'set-var))
    (log-to-console "clause-to-fn new-clause: " new-clause)
    new-clause
    )
  )

(defn compile-clauses
  [clauses]
  (doall (for [clause clauses]
           (let [new-clause (clause-to-fn clause)]
             (eval new-clause))))
  )
