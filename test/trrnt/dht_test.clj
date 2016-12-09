(ns trrnt.dht-test
  (:require  [clojure.test :refer :all]
             [clojure.java.io :as io]
             [trrnt.dht :as sut]))

(deftest decode-error-test
  (let [encoded (.getBytes "d1:eli201e24:A Generic Error Occurrede1:t2:aa1:y1:ee")
        decoded (sut/decode-error (io/input-stream encoded))]
    (is (= (:error-code decoded) 201))
    (is (= (:error-message decoded) "A Generic Error Occurred"))))

(deftest ping-query-test
  (let [transaction-id "asdgadsgdghh"
        querying-node-id "asdgasdg12345"
        encoded (sut/encode-ping-q transaction-id querying-node-id)
        decoded  (sut/decode-ping (io/input-stream encoded))]
    (is (= (:node-id decoded) querying-node-id))))

(deftest find-node-test
  (let [transaction-id "asdgadsgdghh"
        querying-node-id "asdgasdg12345"
        target-node-id "asdgasdg54321"
        encoded (sut/encode-find-node-q transaction-id
                                        querying-node-id
                                        target-node-id)
        decoded  (sut/decode-find-node (io/input-stream encoded))]
    (is (= (:node-id decoded) querying-node-id))
    (is (= (:target decoded) target-node-id))
    (is (nil? (:nodes decoded)))))
