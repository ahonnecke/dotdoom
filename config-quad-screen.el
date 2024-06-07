(fset 'quad-screen
      (lambda (&optional arg)
        "Split the screen into four buffers"
        (interactive "p")
        (delete-other-windows)
        (split-window-right)
        (split-window-right)
        (split-window-right)
        (balance-windows)
        ))

(fset 'penta-screen
      (lambda (&optional arg)
        "Split the screen into five buffers"
        (interactive "p")
        (delete-other-windows)
        (split-window-right)
        (split-window-right)
        (split-window-right)
        (split-window-right)
        (balance-windows)
        ))
