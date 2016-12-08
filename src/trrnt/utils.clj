(ns trrnt.utils
  (:import (java.security MessageDigest))
  (:require [clojure.java.io :as io]))

(defn split-ba
  "split ba on index i, return two seqs"
  [ba i]
  (map byte-array [(take i ba) (take-last (- (count ba) i) ba)]))

(defn rand-string [characters n]
  "random n-length string consisting of given characters"
  (->> (fn [] (rand-nth characters))
       repeatedly
       (take n)
       (apply str)))

(defn sha1
  [x]
  (let [d (MessageDigest/getInstance "SHA1")]
    (.digest d x)))
