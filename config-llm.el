;;; config-llm.el -*- lexical-binding: t; -*-

;; The error occurs because Emacs tries to define the variable
;; autoload-compute-prefixes as dynamic (via defvar) when it has already been
;; implicitly declared as lexical in the current lexical binding context.

;; This problem often arises in environments using Doom Emacs and straight.el,
;; particularly when the straight package manager attempts to build autoloads
;; for packages, as it does for your gptel package in this case.

(defvar autoload-compute-prefixes nil)
;; as such this is required so that it starts cleanly

(defun llm-region ()
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

(straight-use-package 'gptel)
