(ns trrnt.bencode
  (:require [clojure.string :as s])
  (:require [instaparse.core :as ip]))

(defn- encode-str
  [x]
  (let [valid-str (s/replace x #"(?i)[^a-z]" "")]
    (str (count valid-str) ":" valid-str)))

(defn- encode-int
  [n]
  (str "i" (str n) "e"))


(defn encode
  [x]
  (cond
    (some true? ((juxt  keyword? string?) x)) (encode-str x)
    (number? x) (encode-int (int x))
    (vector? x) (str "l" (s/join "" (map encode x)) "e")
    (map? x) (str "d" (s/join "" (map encode
                                      (flatten (seq x))))
                  "e")))

(def ^:private p
  (ip/parser
   (slurp "src/trrnt/bencode.abnf")
   :input-format :abnf))


(defn decode [x]
  (let [parse-tree (p x)]
    (ip/transform {:be (fn [& args] (first  args))
                   :dictionary (fn [_ & a]
                                 (let [items (apply hash-map (butlast a))
                                       key-vals (zipmap
                                                 (map #(keyword %) (keys items))
                                                 (vals items))
                                       ] key-vals)) 
                   :list (fn [ _ & a] (vec (butlast a)))
                   :anytype identity
                   :signumber (fn [n] n)
                   :integer (fn [_ n _] n)
                   :number (fn [& digits] (read-string (apply str (map second digits))))
                   :string (fn [_ _ & chars] (apply str (map second chars)))
                   }
                  parse-tree)))

