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

(ns polyphony.core-test
  (:use clojure.test
        polyphony.core
        )
  (:require [polyphony.utils :refer [sym-to-key]])
  )

(defn clear-polyphony
  []
  (polyphony.reader/reset-rule-info)
  )

(clear-polyphony)

(defn rule-fixture
  "For testing ?varx should never be set"
  [f]
  (defrule ((= ?var1 10)) (()))
  (defrule ((= ?var2 10) (= ?var3 10)) (()))
  (defrule ((= ?var4 10) (= ?var5 10) (= ?var6 10)) (()))
  (defrule ((= ?var7 10)) (()))
  (defrule ((= ?var7 10)) (()))
  (defrule ((= ?var9 10) (= ?var10 10)) (()))
  (defrule ((= ?var8 10)) ((set-var ?var9 20) (set-var ?var10 25)))
  (defrule ((= ?var70 10) (= ?varx 100)) (()))
  (defrule ((= ?varx 100)) ((+ ?var71 10)))
  (defrule ((= ?var72 10)) ((set-var ?var72 11)))
  (defrule ((= ?var73 10) (= ?var74 10)) ((set-var ?var73 (+ ?var74 1))))
  (defrule ((> ?var75 100)) (()))
  (defrule ((> ?var76 10)) ((set-var ?var75 (- ?var76 10))))
  (defrule ((> ?var77 100)) (()))
  (defrule ((> ?var78 10) (= ?var79 10)) ((set-var ?var77 (- ?var78 10))))
  (defrule ((= ?var84 0)) (()))
  (defrule ((> ?var80 100) (= ?var83 ?var80)) ((set-var ?var84 10)))
  (defrule ((> ?var81 10) (= ?var82 10)) ((set-var ?var80 (- ?var81 10))))
  (defrule ((= ?var85 10)) ((set-var ?var85 11)))
  (defrule ((= ?var86 10) (= ?var87 10)) ((set-var ?var86 11)))
  (defrule ((= ?var88 10)) (()))
  (defrule ((= ?var89 10)) ((set-var ?var88 11)))
  (defrule ((= ?var90 10)) (()))
  (defrule ((= ?var91 10) (= ?var92 10)) ((set-var ?var90 11)))
  (defrule ((= ?var93 10)) (()))
  (defrule ((= ?var94 10) (= ?var95 10)) ((set-var ?var93 11)))
  (f)
  )

(use-fixtures :once rule-fixture)

(defn get-cond-id-for-var
  "Returns the first cond-id that is assigned to var-id
   most useful when you know the var-id is used in only 1 cond"
  [var-id]
  (:id (deref (first ((keyword (name var-id))
                      @polyphony.variables/all-variables))))
  )

(defn get-output-ids-for-cond-id
  "Returns a list of output-ids for cond-id"
  [cond-id]
  (doall (for [output (:outputs
                       (deref ((sym-to-key cond-id)
                               @polyphony.node-tree/all-conds)))]
           (:id @output)
           ))
  )

(defn get-join-node
  [join-id]
  (deref ((sym-to-key join-id) @polyphony.node-tree/all-joins))
  )

(defn get-result-node
  [result-id]
  (deref ((sym-to-key result-id) @polyphony.node-tree/all-results))
  )

(defn get-result-node-from-var
  [var-name]
  (deref
   (first
    (take 1
          (for [node ((sym-to-key var-name) @polyphony.variables/all-variables)
                :when (.startsWith (name (:id @node)) "R")
                ]
            node
            ))))
  )

(defn get-cond-node-from-var
  [var-name]
  (deref
   (first
    (take 1
          (for [node ((sym-to-key var-name) @polyphony.variables/all-variables)
                :when (.startsWith (name (:id @node)) "C")
                ]
            node
            ))))
  )

(defn get-var-val-from-node
  [node var-name]
  ((sym-to-key var-name) (:variables node))
  )

(deftest test-single-var-rule-to-result
  (let [cond-id (get-cond-id-for-var '?var1)
        result-id (first (get-output-ids-for-cond-id cond-id))
        ]
    (testing "single variable rule connects to result"
        (is (= (:input-id (get-result-node result-id)) cond-id))
      )
    ))

(deftest test-two-rules-to-join-and-result
  (let [cond-id-1 (get-cond-id-for-var '?var2)
        cond-id-2 (get-cond-id-for-var '?var3)
        output-id-1 (first (get-output-ids-for-cond-id cond-id-1))
        output-id-2 (first (get-output-ids-for-cond-id cond-id-2))
        left-input-id (:left-input-id (get-join-node output-id-1))
        right-input-id (:right-input-id (get-join-node output-id-2))

        ]
    (testing "two rules connect to join and result"
      ;; both conds should connect to same join
      (is (= output-id-1 output-id-2))
      ;; input of join should be the 2 conds
      (is (or (= cond-id-1 left-input-id) (= cond-id-1 right-input-id)))
      (is (or (= cond-id-2 left-input-id) (= cond-id-2 right-input-id)))
      ;; join should output to result node and result node should connect to join
      (is (= (:input-id (deref (:output-node (get-join-node output-id-1))))
             output-id-1))
      )
    )
  )

(deftest test-cond-node-has-multiple-outputs
  (let [cond-id (get-cond-id-for-var '?var7)
        result-ids (get-output-ids-for-cond-id cond-id)
        ]
    (testing "cond node has multiple outputs"
      (is (= (count result-ids) 2))
      (is (= (:input-id (get-result-node (first result-ids)))
             (:input-id (get-result-node (second result-ids)))
             cond-id
             ))
      )
    ))

(deftest test-rule-with-multiple-results
  (testing "both results fire with multiple results"
    (set-var ?var8 10)
    (is (= @polyphony.variables/?var9 20))
    (is (= @polyphony.variables/?var10 25))
    )
  )

(deftest test-get-variable-val
  (testing "get-variable-val returns correct value in cond node"
    (set-var ?var70 20)
    (is (= (get-variable-val ?var70) 20))
    )
  (testing "get-variable-val returns correct value in result node"
    (set-var ?var71 20)
    (is (= @polyphony.variables/?var71 20))
    )
  )

(deftest test-set-var-in-resultnode
  (testing "set-var in result node when variable already set"
    (set-var ?var72 10)
    (is (= @polyphony.variables/?var72 11))
    )
  (testing "nested set-var in resultnode"
    (set-var ?var73 10)
    (set-var ?var74 10)
    (is (= (get-variable-val ?var73) 11))
    )
  )

(deftest test-reset-check
  (testing "reset node-tree and make certain result nodes update when necessary"
    (set-var ?var76 12)
    (is (= 2 (get-var-val-from-node (get-cond-node-from-var '?var75) '?var75)))
    (reset-variable-vals)
    (set-var ?var76 20)
    (is (= 10 (get-var-val-from-node (get-cond-node-from-var '?var75) '?var75)))
    )
  (testing "reset node-tree and make certain join nodes update when necessary"
    (set-var ?var78 12)
    (set-var ?var79 10)
    (is (= 2 (get-var-val-from-node (get-cond-node-from-var '?var77) '?var77)))
    (reset-variable-vals)
    (set-var ?var78 20)
    (is (= 2 (get-var-val-from-node (get-cond-node-from-var '?var77) '?var77)))
    (set-var ?var79 10)
    (is (= 10 (get-var-val-from-node (get-cond-node-from-var '?var77) '?var77)))
    )
  (testing "reset node-tree and make certain cond nodes update when necessary"
    (set-var ?var81 12)
    (set-var ?var82 10)
    (set-var ?var83 10)
    (is (= 2 (get-var-val-from-node (get-cond-node-from-var '?var80) '?var80)))
    (reset-variable-vals)
    (is (= 2 (get-var-val-from-node (get-cond-node-from-var '?var80) '?var80)))
    (is (= 10 (get-var-val-from-node (get-cond-node-from-var '?var83) '?var83)))
    (set-var ?var81 20)
    (set-var ?var82 10)
    (is (= 10 (get-var-val-from-node (get-cond-node-from-var '?var80) '?var80)))
    (is (= nil (get-var-val-from-node (get-cond-node-from-var '?var83) '?var83)))
    )
  )

(deftest test-reset-num-set
  (testing "reset-num set on cond nodes"
    (reset-variable-vals)
    (set-var ?var85 10)
    (is (= (:reset-num (get-cond-node-from-var '?var85))
           @polyphony.core/reset-num))
    )
  (testing "reset-num set on join nodes"
    (reset-variable-vals)
    (set-var ?var86 10)
    (set-var ?var87 10)
    (is (= (:reset-num (deref
                        (first (:outputs (get-cond-node-from-var '?var86)))))
           @polyphony.core/reset-num))
    )
  (testing "reset-num set on result nodes"
    (reset-variable-vals)
    (set-var ?var89 10)
    (is (= (:reset-num (deref
                        (first (:outputs (get-cond-node-from-var '?var88)))))
           @polyphony.core/reset-num))
    )
  )

(deftest test-fire-rule-after-reset
  (testing "check rules fire correctly after a reset"
    (set-var ?var90 15)
    (is (=  @polyphony.variables/?var90 15))
    (set-var ?var91 10)
    (set-var ?var92 10)
    (is (=  @polyphony.variables/?var90 11))
    (reset-variable-vals)
    (set-var ?var90 20)
    (set-var ?var91 10)
    (is (=  @polyphony.variables/?var90 20))
    (set-var ?var90 21)
    (is (=  @polyphony.variables/?var90 21))
    (set-var ?var92 10)
    (is (=  @polyphony.variables/?var90 11))
   )
  (testing "check rules do not fire after a reset if one cond is false"
    (set-var ?var93 15)
    (is (=  @polyphony.variables/?var93 15))
    (set-var ?var94 10)
    (set-var ?var95 10)
    (is (=  @polyphony.variables/?var93 11))
    (reset-variable-vals)
    (set-var ?var93 20)
    (set-var ?var94 10)
    (is (=  @polyphony.variables/?var93 20))
    (set-var ?var93 21)
    (is (=  @polyphony.variables/?var93 21))
    (set-var ?var95 20)
    (is (=  @polyphony.variables/?var93 21))
   )
  )
