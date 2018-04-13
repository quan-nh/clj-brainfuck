(ns brainfuck
  (:refer-clojure :exclude [eval])
  (:require [clojure.pprint :refer [pprint]]
            [instaparse.core :as insta]))

;; eval
(defmulti eval (fn [_ cmd] cmd))

(defmethod eval "+"
  [{:keys [pos] :as state} _]
  (update-in state [:stack pos] inc))

(defmethod eval "-"
  [{:keys [pos] :as state} _]
  (update-in state [:stack pos] dec))

(defmethod eval ">"
  [{:keys [pos stack]} _]
  {:pos (inc pos)
   :stack (if (= (inc pos) (count stack))
            (conj stack 0)
            stack)})

(defmethod eval "<"
  [{:keys [pos] :as state} _]
  (let [new-pos (dec pos)]
    (if (neg? new-pos)
      (update-in state [:stack] into [0])
      (assoc state :pos new-pos))))

(defmethod eval "."
  [{:keys [pos] :as state} _]
  (-> (get-in state [:stack pos])
      char
      print)
  state)

(defmethod eval ","
  [{:keys [pos] :as state} _]
  (->> (read-line)
       first
       (assoc-in state [:stack pos])))

(defmethod eval :default
  [state [_ & seq]]
  (loop [{:keys [pos] :as state} state]
    (if (zero? (get-in state [:stack pos]))
      state
      (recur (reduce eval state seq)))))

;; parse
(def p (insta/parser "brainfuck.bnf"))

(defn -main [s]
  (let [result (p s)]
    (if (insta/failure? result)
      (print (insta/get-failure result))
      (reduce eval {:stack [0]
                    :pos 0}
              result))))
