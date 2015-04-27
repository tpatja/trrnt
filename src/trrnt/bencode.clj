(ns trrnt.bencode
  (:import (java.io ByteArrayOutputStream))
  (:use [clojure.set]))

;; based on implementation by Nurullah Akkaya

(defn- decode-number [stream delimeter & ch]
  (loop [i (if (nil? ch) (.read stream) (first ch)), result ""]
    (let [c (char i)]
      (if (= c delimeter)
        (BigInteger. result)
        (recur (.read stream) (str result c))))))

(defn- decode-string [stream ch]
  (let [length (decode-number stream \: ch)
        buffer (byte-array length)]
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
  (let [list (decode-list stream)] 
    (with-meta 
      (apply hash-map list) 
      {:order (map first (partition 2 list))})))

(defn- decode-map-new [stream]
  (let [list (decode-list stream)
        m (apply hash-map list)
        to-str (fn [b] (String. b "ISO-8859-1"))
        mm (rename-keys m (zipmap (keys m) (map to-str (keys m))))]
    ;;(println mm)
    (with-meta 
      mm
      {:order (map first (partition 2 list))})))


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
        bytes-length (.getBytes (str (count bytes) ":"))]
    (.write stream bytes-length 0 (count bytes-length))
    (.write stream bytes 0 (count bytes))))

(defn- encode-number [n stream]
  (let [string (str "i" n "e")
        bytes (.getBytes string)]
    (.write stream bytes 0 (count bytes))))

(declare encode-object)
(defn- encode-list [l stream]
  (.write stream (int \l))
  (doseq [item l]
    (encode-object item stream))
  (.write stream (int \e)))


(defn bytes? [x]
  (= (Class/forName "[B")
     (.getClass x)))

(defn- encode-dictionary [d stream]
  (.write stream (int \d))
   (doseq [item (if (nil? (meta d)) 
                 (keys d)(:order (meta d)))]
    (encode-object item stream)
    (encode-object (d item) stream))
   (.write stream (int \e)))

(defn- encode-object [obj stream]
  (cond (keyword? obj) (encode-string (name obj) stream)
;;        (bytes? obj) (encode-object (String. obj "ISO-8859-1") stream)
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
