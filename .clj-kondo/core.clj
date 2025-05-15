(ns core)

(defmacro --> [& form]
  (conj (reverse form) '->>))