(ns clojure-json-parser.core
  (:require
   [clojure.string :as str]
   [clojure.data.json :as json]))

(defn parse-null [input]
  (if (str/starts-with? input "null") [nil (subs input 4)] nil))

(defn parse-bool [input]
  (if (str/starts-with? input "true") [true (subs input 4)]
      (if (str/starts-with? input "false") [false (subs input 5)] nil)))

(defn parse-number [input]
  (let [number-matcher (re-find #"^-?[1-9]\d*(\.\d+)?((e|E)(\+|-)?\d+)?" input)]
    (if (nil? number-matcher) nil
        (let [number-match (get number-matcher 0)]
          [(Double/parseDouble number-match) (subs input (.length number-match))]))))

(defn parse-string-chars [input]
  (if (= (.length input) 0) nil
      (if (str/starts-with? input "\"") ["" input]
          (let [char-matcher (parse-string-chars (subs input 1))]
            (if (nil? char-matcher) nil
                [(str (subs input 0 1) (get char-matcher 0)) (get char-matcher 1)])))))

(defn parse-string [input]
  (if (not (str/starts-with? input "\"")) nil
      (let [string-chars-matcher (parse-string-chars (subs input 1))]
        (if (nil? string-chars-matcher) nil
            [(get string-chars-matcher 0) (subs (get string-chars-matcher 1) 1)]))))

"tests"
(json/read-str "null")