;;; config-llm.el -*- lexical-binding: t; -*-


(defun llm-region ()
  (interactive)
  (when (use-region-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (llm-output (get-buffer-create "*LLM Output*")))                                          
      (shell-command-on-region 
       start                                                                                         
       end 
       "llm --system 'implement instruction in comments, return the entire code block without backticks or fences, prepend implemented comments with DONE: '"              
       llm-output)                                                                                          
      (save-excursion                                                                                
        (goto-char end)                                                                              
        (insert-buffer llm-output)))))                                                               

;; This sometimes deletes too much                                                                   
(defun llm-region-replace ()
  (interactive)
  (when (use-region-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (llm-output (get-buffer-create "*LLM Output*")))
      (shell-command-on-region 
       start                                                                                         
       end 
       "llm --system 'implement instruction in comments, return the entire code block sent in the response, prepend implemented comments with DONE: '"              
       llm-output t)                                                                                          
      (delete-region start end)
      (insert-buffer llm-output))))

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
                                                                                                     
