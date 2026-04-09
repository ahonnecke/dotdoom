;;; ~/.doom.d/config-agent-shell-dispatch.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Multi-agent dispatch for agent-shell — spawns parallel background agents
;; with a live SVG task graph in the header line.
;; See: https://github.com/cassandracomar/agent-shell-dispatch

;;; Code:

(use-package! agent-shell-dispatch
  :after agent-shell
  :commands (agent-shell-dispatch-start
             agent-shell-dispatch-spawn-agent
             agent-shell-dispatch-stop
             agent-shell-dispatch-kill-agents
             agent-shell-dispatch-report
             agent-shell-dispatch-view-agent-output)
  :config
  (agent-shell-dispatch-render-mode 1))

(provide 'config-agent-shell-dispatch)
;;; config-agent-shell-dispatch.el ends here
