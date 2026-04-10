;;; config-llm.el -*- lexical-binding: t; -*-

;; Simon Willison's `llm` CLI helpers (region-based).
;; gptel is loaded by config-gptel.el (from ~/src/gptel, not MELPA).

(defun llm-region ()
  "Pipe region through `llm' CLI and insert result after the region."
  (interactive)
  (when (use-region-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (llm-region-output (get-buffer-create "*LLM Region Output*")))                                          
      (shell-command-on-region 
       start                                                                                         
       end 
       "llm --system 'implement instruction in comments, return the entire code block without backticks or fences, prepend implemented comments with DONE: '"              
       llm-region-output)                                                                                          
      (save-excursion                                                                                
        (goto-char end)                                                                              
        (insert-buffer llm-region-output)))))                                                               

;; This sometimes deletes too much                                                                   
(defun llm-region-replace ()
  "Pipe region through `llm' CLI and replace the region with the result."
  (interactive)
  (when (use-region-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (llm-replace-output (get-buffer-create "*LLM Replace Output*")))
      (shell-command-on-region 
       start                                                                                         
       end 
       "llm --system 'implement instruction in comments, return the entire code block sent in the response wrapped with **NEW** **END**, prepend implemented comments with DONE: '"
       llm-replace-output t)                                                                                          
      (delete-region start end)
      (insert-buffer llm-replace-output))))

;; this does not work                                      
(defun llm-region-interactive ()
  "Replace region with LLM output, prompting for an instruction.
Reads a system prompt from the minibuffer, pipes the region through
the `llm' CLI, and replaces the region with the result."
  (interactive)
  (when (use-region-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (instruction (read-string "Enter instruction: "))
           (llm-output (get-buffer-create "*LLM Output*")))
      (shell-command-on-region 
       start                                                                                         
       end 
       (concat "llm --system " (shell-quote-argument instruction))              
       llm-output t)                                                                                          
      (delete-region start end)
      (insert-buffer llm-output))))

;; gptel is loaded from ~/src/gptel by config-gptel.el (not straight/MELPA)
