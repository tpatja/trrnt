(ns trrnt.bencode
  (:import (java.io ByteArrayInputStream))
  (:require [trrnt.bencode :refer :all]
            [clojure.test :refer :all]))


(deftest encode-trivial []
  (is (= (String.  (encode {"a" 42}) "ISO-8859-1")
         "d1:ai42ee")))


(deftest decode-trivial []
  (is (= {"a" 42}
         (decode (ByteArrayInputStream. (.getBytes "d1:ai42ee"))))))


(deftest encode-and-decode []
  (let [m {"a" 42}] 
    (is (= m (decode (ByteArrayInputStream. (encode m)))))))
