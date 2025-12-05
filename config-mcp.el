;;; ~/.doom.d/config-mcp.el -*- lexical-binding: t; -*-

;; MCP: Model Context Protocol for gptel
;; https://github.com/lizqwerscott/mcp.el
;;
;; Gives LLMs access to tools:
;; - Filesystem operations (read/write files)
;; - HTTP fetch (web requests)
;;
;; Requires: npx (Node.js), uvx (Python/uv)

(use-package! mcp
  :after gptel
  :config
  ;; Configure MCP servers
  ;; Each server provides tools the LLM can use
  (setq mcp-hub-servers
        '(;; Filesystem server - file operations
          ;; Restrict to specific directories for safety
          ("filesystem" . (:command "npx"
                           :args ("-y" "@modelcontextprotocol/server-filesystem"
                                  "/home/ahonnecke/src"
                                  "/home/ahonnecke/.doom.d")
                           :env nil))

          ;; Fetch server - HTTP requests
          ("fetch" . (:command "uvx"
                      :args ("mcp-server-fetch")
                      :env nil))))

  ;; Don't auto-start servers (start on demand)
  ;; Uncomment to start all servers on Emacs init:
  ;; (add-hook 'after-init-hook #'mcp-hub-start-all-server)
  )

;; gptel-mcp integration
;; See: https://github.com/lizqwerscott/mcp.el for manual setup
;; The integration is still evolving - check repo for latest

;; Keybindings
(after! mcp
  (define-key ashton-mode-map (kbd "C-c g m") #'mcp-hub)  ; MCP hub status
  (define-key ashton-mode-map (kbd "C-c g s") #'mcp-hub-start-all-server))

(provide 'config-mcp)
;;; config-mcp.el ends here
