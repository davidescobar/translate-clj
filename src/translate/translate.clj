(in-ns 'translate.core)


(def TRANSLATE_BASE_API_URL "https://www.googleapis.com/language/translate/v2")
(def NUM_TRANSLATION_ATTEMPTS 20)
(def LANGUAGES { :en "English" :es "Spanish" :pt-br "Brazilian Portuguese"
                 :hi "Hindi" :zh-cn "Chinese (Simplified)" })


(defn escape-string-for-regex [string-to-escape]
  (string/replace string-to-escape #"([\{\}\(\)\[\]\?])" "\\\\$1"))


(defn extract-yaml-value [line]
  (let [ matches (re-find #"\s*[^#][\w\-\'\"]+:\s*(.+)" line) ]
    (match matches
      [_ value] (string/replace value #"\s*#[^\'\"]+$" "") ; Remove any comments from line.
      _ "")))


(defn get-translation [phrase from-lang to-lang api-key line-number max-tries]
  (let [ params (map #(URLEncoder/encode % "UTF-8") [ phrase (name from-lang) (name to-lang) api-key ])
         url (->> (into [ TRANSLATE_BASE_API_URL ] params)
                  (apply format "%s?q=%s&source=%s&target=%s&key=%s")
                  (URL.)) ]
    ; Inner recursive function that retries translations if necessary.
    (letfn [ (attempt-translation [ attempt ]
               (try
                 (with-open [ buffered-reader (io/reader url) ]
                   (let [ json-data (-> (line-seq buffered-reader)
                                        string/join
                                        (json/read-str :key-fn keyword)) ]
                     (if (nil? (json-data :error))
                       (let [ translation (-> json-data :data :translations first :translatedText
                                              (string/replace #"(&#39;|&quot;)" "")) ]
                         (if (= translation phrase)
                           (format "\"%s\" # Not translated - all attempts failed or timed out." translation)
                           (format "\"%s\"" translation)))
                       (->> json-data :error :errors
                            (map #(format "%s - %s" (% :reason) (% :message)))
                            (string/join ", ")
                            Exception.
                            throw))))
                 (catch Exception e
                   (if (<= attempt max-tries)
                     (attempt-translation (inc attempt))
                     (format "\"%s\" # Not translated - all attempts failed or timed out." phrase))))) ]
      (attempt-translation 1))))


(defn translate [yaml-file-path from-lang to-lang api-key]
  (if (.exists (io/file yaml-file-path))
    (with-open [ rdr (io/reader yaml-file-path) ]
      (let [ translation-cache (atom {})
             channels
               (doall
                 (for [[index line ] (map-indexed #(vector %1 %2) (line-seq rdr)) :when (not (nil? line))]
                   (let [ channel (chan) ]
                     (go
                       (cond
                         (and (zero? index) (re-find #"^\s*#" line))
                           (>! channel (format "# The %s file" (LANGUAGES (keyword to-lang))))
                         (= (-> from-lang name string/trim (str ":"))
                            (string/trim line))
                           (>! channel (format "%s:" (name to-lang)))
                         :else
                           (let [ value (extract-yaml-value line) ]
                             (if (string/blank? value)
                               (>! channel line)
                               (let [ yaml-var-regex #"%\{\S+\}"
                                      yaml-vars (re-seq yaml-var-regex value)
                                      value-regex (re-pattern (escape-string-for-regex (str value "\\s*$")))
                                      value-no-vars (string/replace value yaml-var-regex "**")
                                      translation-no-vars
                                        (if (nil? (@translation-cache value))
                                          (get-translation value-no-vars (name from-lang) (name to-lang)
                                                           api-key (inc index) NUM_TRANSLATION_ATTEMPTS)
                                          (@translation-cache value))
                                      translation (reduce #(string/replace-first %1 "**" %2)
                                                          translation-no-vars yaml-vars)
                                      translated-line (string/replace line value-regex translation) ]
                                 (do
                                   (when (nil? (@translation-cache value))
                                     (swap! translation-cache assoc value translation))
                                   (>! channel translated-line)
                                   (close! channel)))))))
                     channel)))
             translations (map #(<!! %) channels) ]
        (filter #(not (string/blank? %)) translations)))
    '()))
