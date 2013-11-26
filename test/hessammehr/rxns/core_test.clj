(ns rxns.core-test
  (:require [clojure.test :refer :all]
            [hessammehr.rxns.core :refer :all]))

(parse-reaction "2H2 + O2 => H2O")
(parse-fragment "H2")
