(ns trrnt.bencode
  (:import (java.io ByteArrayOutputStream))
  (:require [gloss.core :as gloss]))

;; based on implementation by Nurullah Akkaya

(defn- decode-number [stream delimeter & ch]
  (loop [i (if (nil? ch) (.read stream) (first ch)), result ""]
    (let [c (char i)]
      (if (= c delimeter)
        (BigInteger. result)
        (recur (.read stream) (str result c))))))

(defn- decode-string [stream ch]
  (let [length (decode-number stream \: ch)
        buffer (make-array Byte/TYPE length)]
    (.read stream buffer)
    (String. buffer "ISO-8859-1")))

(declare decode)
(defn- decode-list [stream]
  (loop [result []]
    (let [c (char (.read stream))]
      (if (= c \e)
        result
        (recur (conj result (decode stream (int c))))))))

(defn- decode-map [stream]
  (let [l (decode-list stream)]
    (apply sorted-map l)))

(defn decode [stream & i]
  "decode clojure data structure from given InputStream of bencoded data"
  (let [indicator (if (nil? i) (.read stream) (first i))]
    (cond 
     (and (>= indicator 48) 
          (<= indicator 57)) (decode-string stream indicator)
          (= (char indicator) \i) (decode-number stream \e)
          (= (char indicator) \l) (decode-list stream)
          (= (char indicator) \d) (decode-map stream))))

(defn- encode-string [obj stream]
  (let [bytes (.getBytes obj "ISO-8859-1")
        bytes-length (.getBytes (str (count bytes) ":") "ISO-8859-1")]
    (.write stream bytes-length 0 (count bytes-length))
    (.write stream bytes 0 (count bytes))))

(defn- encode-number [n stream]
  (let [string (str "i" n "e")
        bytes (.getBytes string "ISO-8859-1")]
    (.write stream bytes 0 (count bytes))))

(declare encode-object)
(defn- encode-list [l stream]
  (.write stream (int \l))
  (doseq [item l]
    (encode-object item stream))
  (.write stream (int \e)))


(defn- encode-dictionary [d stream]
  (.write stream (int \d))
  (doseq [[k v] (seq d)]
    (encode-object k stream)
    (encode-object v stream))
  (.write stream (int \e)))

(defn- encode-object [obj stream]
  (cond (keyword? obj) (encode-string (name obj) stream)
        (string? obj) (encode-string obj stream)
        (number? obj) (encode-number obj stream)
        (vector? obj) (encode-list obj stream)
        (map? obj) (encode-dictionary obj stream)))

(defn encode 
  "bencode given clojure object, return byte[]"
  [obj]
  (let [stream (ByteArrayOutputStream.)] 
    (encode-object obj stream)
    (.toByteArray stream)))
