(ns clojure-json-parser.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(declare parse-null parse-bool parse-number parse-string parse-array parse-object parse-value)

(defn parse-null [input] (if (str/starts-with? input "null") [nil (subs input 4)] nil))

(defn parse-bool [input]
  (if (str/starts-with? input "true") [true (subs input 4)]
      (if (str/starts-with? input "false") [false (subs input 5)] nil)))

(defn parse-number [input]
  (let [number-matcher (re-find #"^-?(0|([1-9]\d*))(\.\d+)?((e|E)(\+|-)?\d+)?" input)]
    (if (nil? number-matcher) nil
        (let [number-match (get number-matcher 0)]
          [(Double/parseDouble number-match) (subs input (.length number-match))]))))

(def str-esc-chars {"\"" "\"", "\\" "\\", "/" "/", "b" "\b", "f" "\f", "n" "\n", "r" "\r", "t" "\t"})
(defn parse-esc-char [input]
  (if (and (> (.length input) 0) (get str-esc-chars (subs input 0 1)))
    [(get str-esc-chars (subs input 0 1)) (subs input 1)] nil))

(defn parse-unicode-char [input]
  (let [unicode-char-matcher (re-find #"^u([a-fA-F0-9]{4})" input)]
    (if (nil? unicode-char-matcher) nil
        [(char (Integer/parseInt (get unicode-char-matcher 1) 16)) (subs input 5)])))

(def control-chars #{1 0 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 127})
(defn parse-string-char [input]
  (if (= (.length input) 0) nil
      (if (get control-chars (int (get input 0))) nil
          [(subs input 0 1) (subs input 1)])))

(defn parse-string-chars [input]
  (loop [data "" input input]
    (if (= (.length input) 0) nil
        (if (str/starts-with? input "\"") [data input]
            (if (str/starts-with? input "\\")
              (let [char-matcher (or (parse-unicode-char (subs input 1)) (parse-esc-char (subs input 1)))]
                (if (nil? char-matcher) nil
                    (recur (str data (get char-matcher 0)) (get char-matcher 1))))
              (let [string-char-matcher (parse-string-char input)]
                (if (nil? string-char-matcher) nil
                    (recur (str data (get string-char-matcher 0)) (get string-char-matcher 1)))))))))

(defn parse-string [input]
  (if (not (str/starts-with? input "\"")) nil
      (let [string-chars-matcher (parse-string-chars (subs input 1))]
        (if (nil? string-chars-matcher) nil
            [(get string-chars-matcher 0) (subs (get string-chars-matcher 1) 1)]))))

(defn parse-array-values [input]
  (loop [data [] input input]
    (if (not (str/starts-with? (str/triml input) ",")) [data input]
        (let [value-matcher (parse-value (subs (str/triml input) 1))]
          (if (nil? value-matcher) nil
              (recur (into data [(get value-matcher 0)]) (get value-matcher 1)))))))

(defn parse-array [input]
  (if (not (str/starts-with? input "[")) nil
      (let [input (str/triml (subs input 1))]
        (if (str/starts-with? input "]") [[] (subs input 1)]
            (let [value-matcher (parse-value input)]
              (if (nil? value-matcher) nil
                  (let [array-values (parse-array-values (str/triml (get value-matcher 1)))]
                    (if (or (nil? array-values) (not (str/starts-with? (str/triml (get array-values 1)) "]"))) nil
                        [(into [(get value-matcher 0)] (get array-values 0)) (subs (str/triml (get array-values 1)) 1)]))))))))

(defn parse-object-key-value [input]
  (let [object-key-matcher (parse-string (str/triml input))]
    (if (nil? object-key-matcher) nil
        (let [object-key (get object-key-matcher 0) input (get object-key-matcher 1)]
          (if (not (str/starts-with? (str/triml input) ":")) nil
              (let [object-value-matcher (parse-value (subs (str/triml input) 1))]
                (if (nil? object-value-matcher) nil
                    [{object-key (get object-value-matcher 0)} (get object-value-matcher 1)])))))))

(defn parse-object-key-values [input]
  (loop [data {} input input]
    (if (not (str/starts-with? (str/triml input) ",")) [data input]
        (let [key-value-match (parse-object-key-value (subs (str/triml input) 1))]
          (if (nil? key-value-match) nil
              (recur (into data (get key-value-match 0)) (get key-value-match 1)))))))

(defn parse-object [input]
  (if (not (str/starts-with? input "{")) nil
      (let [input (str/triml (subs input 1))]
        (if (str/starts-with? input "}") [{} (subs input 1)]
            (let [object-key-value (parse-object-key-value input)]
              (if (nil? object-key-value) nil
                  (let [object-key-values (parse-object-key-values (str/triml (get object-key-value 1)))]
                    (if (or (nil? object-key-values) (not (str/starts-with? (str/triml (get object-key-values 1)) "}"))) nil
                        [(into (get object-key-value 0) (get object-key-values 0)) (subs (str/triml (get object-key-values 1)) 1)]))))))))

(defn parse-value [input] (some #(% (str/triml input)) [parse-null parse-bool parse-number parse-string parse-array parse-object]))

(defn parse-json [input]
  (let [value-match (parse-value input)]
    (if (or (nil? value-match) (> (.length (str/triml (get value-match 1))) 0)) nil
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