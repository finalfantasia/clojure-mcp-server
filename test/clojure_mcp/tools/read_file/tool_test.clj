(ns clojure-mcp.tools.read-file.tool-test
  (:require
   [clojure-mcp.config :as config]
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.tools.read-file.tool :as read-file-tool]
   [clojure-mcp.tools.test-utils :as test-utils :refer [*nrepl-client-atom*]]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]))

;; Setup test fixtures
(test-utils/apply-fixtures *ns*)

;; Setup test files
(def ^:dynamic *test-dir* nil)
(def ^:dynamic *test-file* nil)

(defn setup-test-files-fixture [f]
  (let [test-dir (io/file (System/getProperty "java.io.tmpdir") "clojure-mcp-read-test")
        test-file (io/file test-dir "test-file.txt")]

    ;; Create test directory and file
    (.mkdirs test-dir)
    (spit test-file "Line 1\nLine 2\nLine 3\nLine 4\nLine 5")

    ;; Set allowed directories for path validation using config/set-config!
    (config/set-config! *nrepl-client-atom* :nrepl-user-dir (.getAbsolutePath test-dir))
    (config/set-config! *nrepl-client-atom* :allowed-directories [(.getAbsolutePath test-dir)])

    ;; Run test with fixtures bound
    (binding [*test-dir* test-dir
              *test-file* test-file]
      (try
        (f)
        (finally
          ;; Clean up
          (when (.exists test-file)
            (.delete test-file))
          (when (.exists test-dir)
            (.delete test-dir)))))))

(use-fixtures :each setup-test-files-fixture)

(deftest tool-registration-test
  (testing "Tool registration produces a proper map"
    (let [tool-instance (read-file-tool/create-read-file-tool *nrepl-client-atom*)
          reg-map (tool-system/registration-map tool-instance)]
      (is (map? reg-map))
      (is (= "fs_read_file" (:name reg-map)))
      (is (string? (:description reg-map)))
      (is (map? (:schema reg-map)))
      (is (fn? (:tool-fn reg-map))))))

(deftest validate-inputs-test
  (testing "Validate accepts valid input"
    (let [tool-instance (read-file-tool/create-read-file-tool *nrepl-client-atom*)
          result (tool-system/validate-inputs
                  tool-instance
                  {:path (.getAbsolutePath *test-file*)})]
      (is (map? result))
      (is (string? (:path result)))
      (is (not (nil? (:path result))))
      (is (.exists (io/file (:path result))))))

  (testing "Validate rejects missing path parameter"
    (let [tool-instance (read-file-tool/create-read-file-tool *nrepl-client-atom*)]
      (is (thrown? Exception (tool-system/validate-inputs tool-instance {})))))

  (testing "Validate rejects paths outside allowed directories"
    (let [tool-instance (read-file-tool/create-read-file-tool *nrepl-client-atom*)]
      (is (thrown? Exception
                   (tool-system/validate-inputs
                    tool-instance
                    {:path "/etc/passwd"}))))))

(deftest execute-tool-test
  (testing "Execute returns file contents"
    (let [tool-instance (read-file-tool/create-read-file-tool *nrepl-client-atom*)
          validated {:path (.getAbsolutePath *test-file*)}
          result (tool-system/execute-tool tool-instance validated)]
      (is (map? result))
      (is (contains? result :content))
      (is (contains? result :path))
      (is (str/includes? (:content result) "Line 1"))
      (is (not (:truncated? result)))))

  (testing "Execute with offset and limit"
    (let [tool-instance (read-file-tool/create-read-file-tool *nrepl-client-atom*)
          validated {:path (.getAbsolutePath *test-file*)
                     :line_offset 2
                     :limit 2}
          result (tool-system/execute-tool tool-instance validated)]
      (is (map? result))
      (is (= 2 (count (str/split (:content result) #"\n"))))
      (is (str/starts-with? (:content result) "Line 3"))
      (is (:truncated? result)))))

(deftest format-results-test
  (testing "Format results adds XML wrapper"
    (let [tool-instance (read-file-tool/create-read-file-tool *nrepl-client-atom*)
          content-result {:path (.getAbsolutePath *test-file*)
                          :content "Line 1\nLine 2\nLine 3"
                          :truncated? false
                          :line-count 3
                          :offset 0
                          :max-lines 1000}
          result (tool-system/format-results tool-instance content-result)]
      (is (map? result))
      (is (contains? result :result))
      (is (contains? result :error))
      (is (false? (:error result)))
      (is (vector? (:result result)))
      (is (= 1 (count (:result result))))
      (let [formatted-content (first (:result result))]
        (is (str/starts-with? formatted-content "<file-content"))
        (is (str/includes? formatted-content "Line 1"))
        (is (str/ends-with? formatted-content "</file-content>")))))

  (testing "Format results handles errors"
    (let [tool-instance (read-file-tool/create-read-file-tool *nrepl-client-atom*)
          error-result {:error "File not found" :path "/nonexistent/file.txt"}
          result (tool-system/format-results tool-instance error-result)]
      (is (map? result))
      (is (true? (:error result)))
      (is (vector? (:result result)))
      (is (= ["File not found"] (:result result))))))

(deftest tool-execution-test
  (testing "Basic file reading"
    (let [tool-instance (read-file-tool/create-read-file-tool *nrepl-client-atom*)
          tool-tester (test-utils/make-tool-tester tool-instance)
          result (tool-tester {"path" (.getAbsolutePath *test-file*)})]
      (is (false? (:error? result)))
      (is (vector? (:result result)))
      (is (= 1 (count (:result result))))
      (let [formatted-content (first (:result result))]
        (is (str/starts-with? formatted-content "<file-content"))
        (is (str/includes? formatted-content "Line 1"))
        (is (str/includes? formatted-content "Line 5"))
        (is (str/ends-with? formatted-content "</file-content>")))))

  (testing "Reading with offset and limit"
    (let [tool-instance (read-file-tool/create-read-file-tool *nrepl-client-atom*)
          tool-tester (test-utils/make-tool-tester tool-instance)
          result (tool-tester {"path" (.getAbsolutePath *test-file*)
                               "line_offset" 1
                               "limit" 2})]
      (is (false? (:error? result)))
      (is (vector? (:result result)))
      (let [formatted-content (first (:result result))]
        (is (str/includes? formatted-content "Line 2"))
        (is (str/includes? formatted-content "Line 3"))
        (is (str/includes? formatted-content "truncated=\"true\""))
        (is (not (str/includes? formatted-content "Line 4"))))))

  (testing "Reading non-existent file"
    (let [tool-instance (read-file-tool/create-read-file-tool *nrepl-client-atom*)
          tool-tester (test-utils/make-tool-tester tool-instance)
          result (tool-tester {"path" (.getAbsolutePath (io/file *test-dir* "nonexistent.txt"))})]
      (is (true? (:error? result)))
      (is (vector? (:result result)))
      (is (str/includes? (first (:result result)) "does not exist")))))
