(ns polyphony.junk)

(def rules {:condn {} :join {} :result {}})

(defn rule-context
  [r]
  (let [all-rules (atom r)

        set-condn (fn [key val]
                    (->> val
                         (assoc (:condn all-rules) key)
                         (assoc @all-rules :condn)
                         (reset! all-rules)
                         )

                    )

        set-join (fn [val]
                   (assoc (:join all-rules) key val))

        set-result (fn [val]
                     (assoc (:result all-rules) key val))

        print (fn [] (println (:condn @all-rules)
                             (:join @all-rules)
                             (:result @all-rules)))
        ]


    (fn [m]
      (cond
        (= m :set-condn) set-condn
        (= m :set-join) set-join
        (= m :set-result) set-result
        (= m :print) print
        :else (fn [] (println "NO MATCH$$$$$$$$$"))
        )
      )
    ))
