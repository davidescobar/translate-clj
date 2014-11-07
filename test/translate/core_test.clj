(ns translate.core-test
  (:require [clojure.test :refer :all]
            [translate.core :refer :all]
            [clojure.string :as string]))

(deftest extract-yaml-value-test
  (testing "Lines in the format 'key: value'"
    (let [ line "view: Vista" ]
      (is (= (extract-yaml-value line) "Vista"))))

  (testing "Lines in the format 'key: value # with a comment'"
    (let [ line "time: tiempo # This is spanish." ]
      (is (= (extract-yaml-value line) "tiempo"))))

  (testing "Lines in the format 'key:' and comments-only lines '# Just a comment on this line.'"
    (let [ lines [ "key:", "    sub-categories:      ", "# Just a comment on this line." ] ]
      (is (= true (every? #(string/blank? (extract-yaml-value %)) lines))))))
