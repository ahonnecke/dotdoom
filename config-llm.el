;;; config-llm.el -*- lexical-binding: t; -*-


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
