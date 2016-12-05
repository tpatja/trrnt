(ns trrnt.utils
  (:import (java.security MessageDigest)))

(defn split-ba [ba i]
  "split ba on index i, return two seqs"
  (map byte-array [(take i ba) (take-last (- (count ba) i) ba)]))

(defn rand-string [characters n]
  (->> (fn [] (rand-nth characters))
       repeatedly
       (take n)
       (apply str)))

(defn sha1
  [x]
  (let [d (MessageDigest/getInstance "SHA1")]
    (.digest d x)))
