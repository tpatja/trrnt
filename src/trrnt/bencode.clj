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



  ;; decoding - functions return match and position

  ;; (defn decode-int [x]
  ;;   (let [num (re-find #"^\d+" (apply str (rest x)))]
  ;;     (Integer. num)))

  ;; (defn decode-str [x]
  ;;   (let [start (+ 1 (.indexOf x ":"))
  ;;         end (+ start (read-string (re-find #"^\d+" x)))
  ;;         s (subs x start end)]
  ;;     s))

  ;; (defn decode-list [x]
  ;;   (let [to-parse (apply str (rest x))]
  ;;     (println to-parse)
  ;;     (loop [res (decode to-parse)
  ;;            acc []]
  ;;       (if nil? res
  ;;           acc
  ;;           recur ()
  ;;           )
  
  ;;       )
  ;;     (decode to-parse)))

  ;; (defn decode-dict [x])

  ;; (defn decode
  ;;   [s]
  ;;   (let [ch0 (first s)
  ;;         starts-with #(= % ch0)
  ;;         numeric? #(comp not re-find #"[^\d]" %)]
  ;;     (println s)
  ;;     (cond
  ;;       (starts-with \i) (decode-int s)
  ;;       ;; (starts-with \l) (decode-list s)
  ;;       (starts-with \d) (decode-dict s)
  ;;       (numeric? ch0) (decode-str s))))
  
  
