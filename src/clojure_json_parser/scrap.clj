(ns clojure-json-parser.scrap
  (:require
   [clojure-json-parser.core :refer [control-chars parse-array
                                     parse-array-values parse-esc-char
                                     parse-json parse-object
                                     parse-object-key-value
                                     parse-object-key-values parse-string
                                     parse-string-char parse-unicode-char
                                     parse-value]]
   [clojure.data.json :as json]
   [clojure.string :as str]
   [clojure.java.io :as io]))


(parse-esc-char "nn")
(some #(str/starts-with? "\n" (str (char %))) control-chars)
(boolean (get control-chars 0))
(int (get "a\b" 0))
(parse-string-char "")
(parse-string "\"JSON Test\"")
(parse-array-values "\r , \n 1 \t,null   , \"abc\"   blabla")
(parse-array-values ",null,1,\"\"  abc")
(parse-array "[ 1, [ 123.9 ] \t\n ]  s")
(parse-array "[]")
(json/read-str "   [ 1, [ 123.9 ]\t\n ]  ")
(parse-object-key-value " \r \"a\" \n : \t1bla")
(parse-object-key-value " \r  \n : \t1bla")
(parse-object-key-values "   ,  \"a\"  :   1   ,   \"b\"  :  2     \"c\"  :  3  ")
(parse-object-key-values "     \"a\"  :   1   ,   \"b\"  :  2     \"c\"  :  3  ")
(parse-object-key-values "   ,   :   1   ,   \"b\"  :  2     \"c\"  :  3  ")
(parse-object-key-values ", \"a\": 2}")
(parse-object "{}")
(parse-object "{\"b\": 1, \"a\": 2}")
(json/read-str "{}")
(json/read-str "{\"b\": 1, \"a\": 2}")
(parse-value "123abc")
(json/read-str "{\"b\": 1, \"a\": [2]}")
(parse-json "{\"b\": 1, \"a\": [2]}")
(let [input "   , \"a\"  :   1   ,   \"b\"  :  2     \"c\"  :  3  "]
  (if (not (str/starts-with? (str/triml input) ",")) [{} input] nil))


;; Macros
(defmacro backwards [form] (reverse form))
(backwards (3 2 1 +))
(backwards (\c \b \a str))
(defmacro infix [[arg1 fnc arg2]] (list fnc arg1 arg2))
(infix (1 + 2))
(macroexpand '(infix (1 + 2)))
(defn sum-two [a b] (infix (a + b)))
(sum-two 1 2)

(defmacro if-not-nil [nil-match suc-exp] (if (nil? nil-match) nil suc-exp))
(if (nil? 1) nil "success exp")
(if-not-nil 1 "success exp")

;; Loop Recur
(loop [iteration 0]
  (println (str "Iteration " iteration))
  (if (> iteration 3)
    (println "Goodbye!")
    (recur (inc iteration))))

(loop [iteration 0 data 10]
  (println (str "Iteration " iteration))
  (if (> iteration 3)
    [iteration data]
    (recur (inc iteration) (dec data))))

"check falsy or truthy values"
(boolean nil)

"parse hex char"
(parse-unicode-char "uabcde")

"parse string"
(def json-string "\"\\uabcde\\u8347\\n\"kadksjhda")
(parse-string json-string)
(json/read-str json-string)

(let [test-files
      (map #(.getAbsolutePath %)
           (filter #(.isFile %) (file-seq (io/file "./test/clojure_json_parser/cases"))))]
  (map #(list % (not (nil? (parse-json (slurp %)))))
       test-files))
;; (re-find #"^pass.+" (.getName %))
