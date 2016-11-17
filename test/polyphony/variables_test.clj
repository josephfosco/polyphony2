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

(ns polyphony.variables-test
  (:use clojure.test
        polyphony.variables))

(defn variables-fixture
  [f]
  (add-variable '?v-var1 (atom {:id 'node1}))
  (add-variable '?v-var2 (atom {:id 'node2}))
  (add-variable '?v-var3 (atom {:id 'node3}))
  (f)
  (reset! all-variables {})
  )

(use-fixtures :once variables-fixture)

(deftest test-num-of-variables
  (testing "num of variables added"
    (is (= (count @all-variables) 3))
    ))

(deftest test-contents-of-variables
  (testing "contents of variables"
    (is (= 'node1
           (:id (deref (first ((keyword (name '?v-var1)) @all-variables))))))
    (is (= 'node2
           (:id (deref (first ((keyword (name '?v-var2)) @all-variables))))))
    (is (= 'node3
           (:id (deref (first ((keyword (name '?v-var3)) @all-variables))))))
    ))
