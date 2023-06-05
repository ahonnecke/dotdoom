;;; ../src/home/.doom.d/config-ruff-lsp.el -*- lexical-binding: t; -*-

(defcustom lsp-ruff-executable "ruff-lsp"
  "Command to start the Ruff language server."
  :group 'lsp-python
  :risky t
  :type 'file)

;; Register ruff-lsp with the LSP client.
(lsp-register-client
    (make-lsp-client
        :new-connection (lsp-stdio-connection (lambda () (list lsp-ruff-executable)))
        :activation-fn (lsp-activate-on "python")
        :add-on? t
        :server-id 'ruff
        :initialization-options (lambda ()
                                    (list :settings
                                        (cl-list*
                                          (when
                                            poetry-project-venv
                                                (list
                                                :interpreter (vector (f-join (f-long poetry-project-venv) "bin" "python3"))
                                                :workspace (f-long poetry-project-venv)
                                                :path (vector (f-join (f-long poetry-project-venv) "bin" "ruff")))
                                                )
                                            ))
                                    )))
