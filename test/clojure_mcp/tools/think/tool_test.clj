(ns clojure-mcp.tools.think.tool-test
  (:require [clojure-mcp.tool-system :as tool-system]
            ;; Required for multimethods
            [clojure-mcp.tools.think.tool]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import (clojure.lang ExceptionInfo)))

(deftest think-tool-multimethods
  (let [tool-config {:tool-type :think}]
    (testing "tool-name returns the correct name"
      (is (= "think" (tool-system/tool-name tool-config))))

    (testing "tool-description returns a non-empty string"
      (let [description (tool-system/tool-description tool-config)]
        (is (string? description))
        (is (not (str/blank? description)))))

    (testing "tool-schema returns a valid schema"
      (let [schema (tool-system/tool-schema tool-config)]
        (is (map? schema))
        (is (= "object" (:type schema)))
        (is (map? (:properties schema)))
        (is (contains? (:properties schema) "thought"))))

    (testing "validate-inputs validates correctly"
      (is (= {:thought "Some thought"}
             (tool-system/validate-inputs tool-config {:thought "Some thought"})))
      (is (thrown? ExceptionInfo
                   (tool-system/validate-inputs tool-config {}))))

    (testing "execute-tool returns expected result"
      (let [result (tool-system/execute-tool tool-config {:thought "Test execution"})]
        (is (map? result))
        (is (= false (:error result)))
        (is (= "Your thought has been logged." (:result result)))))

    (testing "format-results formats correctly"
      (let [success-result (tool-system/format-results tool-config {:result "Success" :error false})
            error-result (tool-system/format-results tool-config {:result "Error" :error true})]
        (is (= ["Success"] (:result success-result)))
        (is (= false (:error success-result)))
        (is (= ["Error"] (:result error-result)))
        (is (= true (:error error-result)))))))