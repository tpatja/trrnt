(ns trrnt.pwp-test
  (:require [trrnt.pwp :refer :all]
            [clojure.test :refer :all]))

(deftest peer-client-name-t
  (is (= "unknown client"
         (peer-client-name "XX-124214-xxxxxx")))
  (is (= "Transmission 1.1.1.1"
         (peer-client-name "-TR1111-xxxxxxxxxx"))))
