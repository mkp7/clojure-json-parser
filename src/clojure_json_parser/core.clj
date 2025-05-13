(ns clojure-json-parser.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(declare parse-null parse-bool parse-number parse-string parse-array parse-object parse-value)

(defn parse-null [input]
  (when (str/starts-with? input "null")
    [nil (subs input 4)]))

(defn parse-bool [input]
  (cond
    (str/starts-with? input "true") [true (subs input 4)]
    (str/starts-with? input "false") [false (subs input 5)]))

(defn parse-number [input]
  (->>
    [(Double/parseDouble number-match)
     (subs input (.length number-match))]
    (let [number-match (get number-matcher 0)])
    (when number-matcher)
    (let [number-matcher (re-find #"^-?(0|([1-9]\d*))(\.\d+)?((e|E)(\+|-)?\d+)?" input)])))

(def str-esc-chars {"\"" "\"", "\\" "\\", "/" "/", "b" "\b", "f" "\f", "n" "\n", "r" "\r", "t" "\t"})
(defn parse-esc-char [input]
  (when (->>
          (subs input 0 1)
          (get str-esc-chars)
          (and
            (> (.length input) 0)))
    [(get str-esc-chars (subs input 0 1))
     (subs input 1)]))

(defn parse-unicode-char [input]
  (let [unicode-char-matcher (re-find #"^u([a-fA-F0-9]{4})" input)]
    (when unicode-char-matcher
      [(->
         (get unicode-char-matcher 1)
         (Integer/parseInt 16)
         (char))
       (subs input 5)])))

(def control-chars #{1 0 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 127})
(defn parse-string-char [input]
  (when (->>
          (get input 0)
          (int)
          (get control-chars)
          (not)
          (and (> (.length input) 0)))
    [(subs input 0 1)
     (subs input 1)]))

(defn parse-string-chars [input]
  (->>
    (cond
      (str/starts-with? input "\"") [data input]
      (str/starts-with? input "\\") (->>
                                      (recur (str data (get esc-char-matcher 0))
                                        (get esc-char-matcher 1))
                                      (when esc-char-matcher)
                                      (let [esc-char-matcher (or (parse-unicode-char (subs input 1))
                                                               (parse-esc-char (subs input 1)))]))
      :else (->>
              (recur (str data (get string-char-matcher 0))
                (get string-char-matcher 1))
              (when string-char-matcher)
              (let [string-char-matcher (parse-string-char input)])))
    (when (> (.length input) 0))
    (loop [data "" input input])))

(defn parse-string [input]
  (->>
    [(get string-chars-matcher 0)
     (subs (get string-chars-matcher 1) 1)]
    (when string-chars-matcher)
    (let [string-chars-matcher (parse-string-chars (subs input 1))])
    (when (str/starts-with? input "\""))))

(defn parse-array-values [input]
  (->>
    (recur (into data [(get value-matcher 0)])
      (get value-matcher 1))
    (when value-matcher)
    (let [value-matcher (parse-value (subs (str/triml input) 1))])
    (if (not (str/starts-with? (str/triml input) ","))
      [data input])
    (loop [data [] input input])))

(defn parse-array [input]
  (cond
    (not (str/starts-with? input "[")) nil
    (->
      (subs input 1)
      (str/triml)
      (str/starts-with? "]")) [[] (->
                                    (subs input 1)
                                    (str/triml)
                                    (subs 1))]
    :else (->>
            [(into [(get value-matcher 0)] (get array-values 0))
             (->
               (get array-values 1)
               (str/triml)
               (subs 1))]
            (when (and array-values (->
                                      (get array-values 1)
                                      (str/triml)
                                      (str/starts-with? "]"))))
            (let [array-values (->>
                                 (get value-matcher 1)
                                 (str/triml)
                                 (parse-array-values))])
            (when value-matcher)
            (let [value-matcher (->>
                                  (subs input 1)
                                  (str/triml)
                                  (parse-value))]))))

(defn parse-object-key-value [input]
  (->>
    [{object-key (get object-value-matcher 0)}
     (get object-value-matcher 1)]
    (when object-value-matcher)
    (let [object-value-matcher (parse-value (subs (str/triml input) 1))])
    (when (str/starts-with? (str/triml input) ":"))
    (let [object-key (get object-key-matcher 0) input (get object-key-matcher 1)])
    (when object-key-matcher)
    (let [object-key-matcher (parse-string (str/triml input))])))

(defn parse-object-key-values [input]
  (->>
    (recur (into data (get key-value-match 0))
      (get key-value-match 1))
    (when key-value-match)
    (let [key-value-match (parse-object-key-value (subs (str/triml input) 1))])
    (if (not (str/starts-with? (str/triml input) ","))
      [data input])
    (loop [data {} input input])))

(defn parse-object [input]
  (cond
    (not (str/starts-with? input "{")) nil
    (str/starts-with? (str/triml (subs input 1)) "}") [{} (subs (str/triml (subs input 1)) 1)]
    :else (->>
            [(into (get object-key-value 0) (get object-key-values 0))
             (subs (str/triml (get object-key-values 1)) 1)]
            (when (and object-key-values (str/starts-with? (str/triml (get object-key-values 1)) "}")))
            (let [object-key-values (parse-object-key-values (str/triml (get object-key-value 1)))])
            (when object-key-value)
            (let [object-key-value (parse-object-key-value (str/triml (subs input 1)))]))))

(def value-parsers [parse-null parse-bool parse-number parse-string parse-array parse-object])
(defn parse-value [input] (some #(% (str/triml input)) value-parsers))

(defn parse-json [input]
  (let [value-match (parse-value input)]
    (when (and value-match (= (str/triml (get value-match 1)) ""))
      (get value-match 0))))

(defn test-json-parser []
  (let [test-files
        (map #(.getAbsolutePath %)
          (filter #(.isFile %) (file-seq (io/file "./test/clojure_json_parser/cases"))))]
    (map #(list % (not (nil? (parse-json (slurp %)))))
      test-files)))

(parse-json (slurp "./test/clojure_json_parser/cases/pass1.json"))
(parse-json (slurp "./test/clojure_json_parser/cases/passReddit.json"))

(test-json-parser)