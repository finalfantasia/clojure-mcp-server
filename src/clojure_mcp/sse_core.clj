(ns clojure-mcp.sse-core
  (:require
   [clojure.tools.logging :as log])
  (:import
   (com.fasterxml.jackson.databind ObjectMapper)
   (io.modelcontextprotocol.server McpServer)
   (io.modelcontextprotocol.server.transport
    HttpServletSseServerTransportProvider)
   (io.modelcontextprotocol.spec McpSchema$ServerCapabilities)
   (jakarta.servlet Servlet)
   (org.eclipse.jetty.ee10.servlet ServletContextHandler ServletHolder)
   (org.eclipse.jetty.server Server)))

;; helpers for setting up an sse mcp server

(defn mcp-sse-server []
  (log/info "Starting SSE MCP server")
  (try
    (let [transport-provider (HttpServletSseServerTransportProvider/new
                              (ObjectMapper/new) "/mcp/message")
          server (-> (McpServer/async transport-provider)
                     (.serverInfo "clojure-server" "0.1.0")
                     (.capabilities (-> (McpSchema$ServerCapabilities/builder)
                                        (.tools true)
                                        (.prompts true)
                                        (.resources true true)
                                        #_(.logging)
                                        (.build)))
                     (.build))]
      (log/info "SSE MCP server initialized successfully")
      {:provider-servlet transport-provider
       :mcp-server server})
    (catch Exception e
      (log/error e "Failed to initialize SSE MCP server")
      (throw e))))

(defn host-mcp-servlet
  "Main function to start the embedded Jetty server."
  [servlet port]
  (log/info "Starting SSE MCP server on port" port)
  (doto (^[int] Server/new port)
    (.setHandler
     (doto (ServletContextHandler/new ServletContextHandler/SESSIONS)
       (.setContextPath "/")
       (.addServlet (^[Servlet] ServletHolder/new servlet) "/")))
    (.start)
    (.join)))
