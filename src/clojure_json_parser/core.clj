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

(def str-esc-chars {"\"" "\"", "\\" "\\", "/" "/", "b" "\b", "f" "\f", "n" "\n", "r" "\r", "t" "\t"})
(defn parse-esc-char [input]
  (if (and (> (.length input) 0) (get str-esc-chars (subs input 0 1)))
    [(get str-esc-chars (subs input 0 1)) (subs input 1)]
    nil))

(parse-esc-char "nn")

(defn parse-unicode-char [input]
  (let [unicode-char-matcher (re-find #"^u([a-fA-F0-9]{4})" input)]
    (if (nil? unicode-char-matcher) nil
        [(char (Integer/parseInt (get unicode-char-matcher 1) 16))
         (subs input 5)])))

(def str-control-chars #{1 0 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 127})
(defn parse-string-char [input]
  (if (= (.length input) 0) nil
      (if (get str-control-chars (int (get input 0))) nil
          [(subs input 0 1) (subs input 1)])))

(some #(str/starts-with? "\n" (str (char %))) str-control-chars)
(boolean (get str-control-chars 0))
(int (get "a\b" 0))
(parse-string-char "")

(defn parse-string-chars [input]
  (if (= (.length input) 0) nil
      (if (str/starts-with? input "\"") ["" input]
          (if (str/starts-with? input "\\")
            (let [char-matcher (or (parse-unicode-char (subs input 1))
                                   (parse-esc-char (subs input 1)))]
              (if (nil? char-matcher) nil
                  (let [string-matcher (parse-string-chars (get char-matcher 1))]
                    (if (nil? string-matcher) nil
                        [(str (get char-matcher 0) (get string-matcher 0))
                         (get string-matcher 1)]))))
            (let [string-char-matcher (parse-string-char input)]
              (if (nil? string-char-matcher) nil
                  (let [string-matcher (parse-string-chars (get string-char-matcher 1))]
                    (if (nil? string-matcher) nil
                        [(str (get string-char-matcher 0) (get string-matcher 0))
                         (get string-matcher 1)]))))))))

(defn parse-string [input]
  (if (not (str/starts-with? input "\"")) nil
      (let [string-chars-matcher (parse-string-chars (subs input 1))]
        (if (nil? string-chars-matcher) nil
            [(get string-chars-matcher 0) (subs (get string-chars-matcher 1) 1)]))))

(defn parse-array [input]
  (if (not (str/starts-with? input "[")) nil
      (let [input (str/triml (subs input 1))]
        (if (str/starts-with? input "]")
          [[] (subs input 1)]
          (let [value-matcher (parse-value input)]
            (if (nil? value-matcher) nil
                (let [input (str/triml (get value-matcher 1))]
                  (if (not (str/starts-with? input "]")) nil
                      [[(get value-matcher 0)] (subs input 1)]))))))))
(parse-array "[ 123.9 \t\n ]  s")
(json/read-str "   [ 123.9\t\n ]  ")

(defn parse-value [input] (some #(% input) [parse-null parse-bool parse-number parse-string parse-array]))
(parse-value "123abc")

;; Macros
(defmacro backwards [form] (reverse form))
(backwards (3 2 1 +))
(backwards (\c \b \a str))
(defmacro infix [[arg1 fnc arg2]] (list fnc arg1 arg2))
(infix (1 + 2))
(macroexpand '(infix (1 + 2)))
(defn sum-two [a b] (infix (a + b)))
(sum-two 1 2)

"check falsy or truthy values"
(boolean nil)

"parse hex char"
(parse-unicode-char "uabcde")

"parse string"
(def json-string "\"\\uabcde\\u8347\\n\"kadksjhda")
(parse-string json-string)
(json/read-str json-string)
