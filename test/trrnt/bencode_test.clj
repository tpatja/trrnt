(ns trrnt.bencode-test
  (:require [trrnt.bencode :refer :all]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(deftest encode-trivial
  (is (= (slurp (encode {"a" 42}))
         "d1:ai42ee")))

(deftest decode-trivial
  (is (= (decode (io/input-stream (.getBytes "d1:ai42ee")))
         {"a" 42})))

(deftest encode-and-decode
  (let [m {"a" 42}
        encoded (encode m)]
    (is (= m
           (decode (io/input-stream encoded))))))
