(ns trrnt.utils
  (:import (java.security MessageDigest))
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn split-ba
  "split ba on index i, return two seqs"
  [ba i]
  (map byte-array [(take i ba) (take-last (- (count ba) i) ba)]))

(defn rand-string
  "random n-length string consisting of given characters"
  [characters n]
  (->> (fn [] (rand-nth characters))
       repeatedly
       (take n)
       (apply str)))

(defn sha1
  "SHA1 digest of given bytes"
  [x]
  (let [d (MessageDigest/getInstance "SHA1")]
    (.digest d x)))

(defn hexify
  "Hex string from given byte-array"
  [ba]
 (string/join (map #(format "%02x" %) ba)))
