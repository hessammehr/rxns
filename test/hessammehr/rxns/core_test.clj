(ns rxns.core-test
  (:require [clojure.test :refer :all]
            [hessammehr.rxns.core :refer :all]))

(->> (parse-reaction "2H2 + O2 => 2H2O")
     dump-reaction)
(-> (parse-fragment "H2")
    dump-fragment)


(-> (parse-chemical-with-coefficient "2H2O")
    dump-chemical-with-coefficient)
