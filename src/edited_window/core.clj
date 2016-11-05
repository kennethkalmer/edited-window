(ns edited-window.core
  (:require [cheshire.core :refer :all]))

(def export-dir "./slackexport")

(def message-count (atom 0))

(defn get-files [dir]
  (->> (file-seq (clojure.java.io/file export-dir))
       (filter #(.isFile %))))

(defn file->messages [file]
  (try
    (with-open [rdr (clojure.java.io/reader file)]
      (parse-string (slurp rdr)))
    (catch com.fasterxml.jackson.core.JsonParseException e [])))

(defn edited-messages [message]
  (swap! message-count inc)
  (contains? message "edited"))

(defn filter-edited [files]
  (->> files
       (mapcat file->messages)
       (filter edited-messages)))

(defn ts->int [ts]
  (int (bigdec ts)))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))

(defn analyze-edits [messages]
  (let [windows (map #(- (ts->int ((% "edited") "ts")) (ts->int (% "ts"))) messages)
        edits   (count windows)]
    {:windows windows
     :total   edits
     :min     (apply min windows)
     :max     (apply max windows)
     :average (mean windows)
     :meadian (median windows)}))

(defn main []
  (reset! message-count 0)

  (-> (get-files export-dir)
      (filter-edited)
      (analyze-edits)))
