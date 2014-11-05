(ns translate.core
  (:gen-class)
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.core.async :as async :refer [ >! <!! chan close! go ]]
            [clojure.core.match :as match :refer [ match ]])
  (:import [java.net URL URLEncoder]
           [java.util.regex Pattern]))


(load "translate")


(defn- path-join [ & path-fragments ]
  (.getPath (apply io/file path-fragments)))


(defn -main [& args]
  (let [ [ from-locale api-key yaml-folder-path ] args ]
    (if (= (count args) 3)
      (let [ original-yaml-path (path-join yaml-folder-path (str from-locale ".yml"))
             devise-original-yaml-path (path-join yaml-folder-path (str "devise." from-locale ".yml")) ]
        (if (every? #(.exists (io/file %)) [ original-yaml-path devise-original-yaml-path ])
          (let [ start (System/currentTimeMillis)
                 language-channels
                   (doall
                     (for [[locale language] LANGUAGES :when (not= (name locale) (name from-locale))]
                       (let [ channel (chan) ]
                         (go
                          (let [ translations
                                   (translate original-yaml-path (name from-locale) (name locale) api-key)
                                 devise-translations
                                   (translate devise-original-yaml-path (name from-locale) (name locale) api-key)
                                 translated-yaml-file-path
                                   (string/replace original-yaml-path
                                                   (-> original-yaml-path io/file .getName)
                                                   (format "%s.yml" (name locale)))
                                 devise-translated-yaml-file-path
                                   (string/replace devise-original-yaml-path
                                                   (-> devise-original-yaml-path io/file .getName)
                                                   (format "devise.%s.yml" (name locale))) ]
                            (do
                              (spit translated-yaml-file-path (string/join "\n" translations))
                              (spit devise-translated-yaml-file-path (string/join "\n" devise-translations))
                              (println (format "%d term(s) translated to %s."
                                               (+ (count translations)
                                                  (count devise-translations))
                                               language))
                              (>! channel true)
                              (close! channel))))
                         channel))) ]
            (if (every? #(true? (<!! %)) language-channels)
              (let [ end (System/currentTimeMillis) ]
                (do
                  (println "All languages finished translating.")
                  (println (format "\nTask completed in: %.2f seconds.\n" (-> (- end start) (/ 1000.0)) "\n"))))
              (println "Not all languages translated successfully! Please double-check the I18n YAML files.")))
          (println (format "Could not find '%s' or '%s'." original-yaml-path devise-original-yaml-path))))
      (println "Usage: ./translate [from-locale] [google-translate-api-key] [i18n-yaml-folder-path]"))))
