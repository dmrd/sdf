(ns chapter-2-dsl.combinator-test
  (:require  [cljs.test :refer-macros [deftest is testing run-tests]]
             [chapter-2-dsl.exercises :as combinators]
             ))

(deftest test-import
  (is (= (combinators/square 2) 4)))

(deftest test-dummy
  (is (= 1 1)))

(deftest test-dummy2
  (is (= 2 2)))
