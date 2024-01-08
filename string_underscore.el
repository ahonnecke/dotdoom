;;; string_underscore.el -*- lexical-binding: t; -*-

(defun space-to-underscore ()
  "Cyclically replace {underscore, space, hypen} chars current line or text
selection.
When called repeatedly, this command cycles the {“ ”, “_”, “-”} characters."
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0 to length of charArray.
  (let (mainText charArray p1 p2 currentState nextState changeFrom
                 changeTo startedWithRegion-p )

    (if (region-active-p)
        (progn
          (setq startedWithRegion-p t )
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          )
      (progn (setq startedWithRegion-p nil )
             (setq p1 (line-beginning-position))
             (setq p2 (line-end-position)) ) )

    (setq charArray [" " "_" "-"])

    (setq currentState
          (if (get 's2u 'state)
              (get 's2u 'state)
            0))
    (setq nextState (% (+ currentState 1) (length charArray)))

    (setq changeFrom (elt charArray currentState ))
    (setq changeTo (elt charArray nextState ))

    (setq mainText (replace-regexp-in-string changeFrom changeTo
                                             (buffer-substring-no-properties p1 p2)) )
    (delete-region p1 p2)
    (insert mainText)

    (put 's2u 'state nextState)

    (when startedWithRegion-p
      (goto-char p2)
      (set-mark p1)
      (setq deactivate-mark nil) ) ) )


(global-set-key (kbd "C-c C-s _") 'space-to-underscore)
