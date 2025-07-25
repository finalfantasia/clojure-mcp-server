(ns clojure-mcp.file-content
  "File content utilities for MCP, including image content creation."
  (:import (io.modelcontextprotocol.spec
            McpSchema$BlobResourceContents
            McpSchema$EmbeddedResource
            McpSchema$ImageContent
            McpSchema$TextResourceContents)
           (java.nio.file Files Path)
           (java.util Base64)
           (org.apache.tika Tika)
           (org.apache.tika.mime MediaType MediaTypeRegistry MimeTypes)))

;; embedded resources aren't supported by claude desktop yet but who knows
;; which clients are supporting this and when

(def ^Tika mime-detector
  "Singleton Apache Tika detector (falls back to Files/probeContentType)."
  (delay (Tika.)))

(def ^MediaTypeRegistry registry
  (.getMediaTypeRegistry (MimeTypes/getDefaultMimeTypes)))

(defn text-media-type? [mime]
  (.isInstanceOf registry (MediaType/parse mime) MediaType/TEXT_PLAIN))

(defn image-media-type? [mime-or-media-type]
  (= "image" (.getType (MediaType/parse mime-or-media-type))))

(defn mime-type* [^Path p]
  (or (Files/probeContentType p)
      (try (.detect ^Tika @mime-detector (.toFile p))
           (catch Exception _ "application/octet-stream"))))

(defn str->nio-path [fp]
  (Path/of fp (make-array String 0)))

(defn mime-type [file-path]
  (mime-type* (str->nio-path file-path)))

(defn serialized-file [file-path]
  (let [path (str->nio-path file-path)
        bytes (Files/readAllBytes path)
        b64 (.encodeToString (Base64/getEncoder) bytes)
        mime (mime-type* path) ;; e.g. application/pdf
        uri (str "file://" (.toAbsolutePath path))]
    {:file-path file-path
     :nio-path path
     :uri uri
     :mime-type mime
     :b64 b64}))

(defn image-content [{:keys [b64 mime-type]}]
  (McpSchema$ImageContent. nil nil b64 mime-type))

(defn text-resource-content [{:keys [uri mime-type b64]}]
  (let [blob (McpSchema$TextResourceContents. uri mime-type b64)]
    (McpSchema$EmbeddedResource. nil nil blob)))

(defn binary-resource-content [{:keys [uri mime-type b64]}]
  (let [blob (McpSchema$BlobResourceContents. uri mime-type b64)]
    (McpSchema$EmbeddedResource. nil nil blob)))

(defn file-response->file-content [{:keys [::file-response]}]
  (let [{:keys [mime-type] :as ser-file} (serialized-file file-response)]
    (cond
      (text-media-type? mime-type) (text-resource-content ser-file)
      (image-media-type? mime-type) (image-content ser-file)
      :else (binary-resource-content ser-file))))

(defn should-be-file-response? [file-path]
  (not (text-media-type? (mime-type file-path))))

(defn text-file? [file-path]
  (text-media-type? (mime-type file-path)))

(defn image-file? [file-path]
  (image-media-type? (mime-type file-path)))

(defn ->file-response [file-path]
  {::file-response file-path})

(defn file-response? [map]
  (and (map? map)
       (::file-response map)))

(comment
  (-> "./dev/sponsors.pdf"
      ->file-response
      file-response->file-content)

  (should-be-file-response? "./dev/logback.xml")

  (text-media-type? (mime-type (str->nio-path "hello.md"))))
