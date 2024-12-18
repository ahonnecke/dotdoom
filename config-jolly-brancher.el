;; Load the package
(add-to-list 'load-path "/home/ahonnecke/src/jolly-brancher")

;; Set up the prefix key and load the package
(after! transient
  (require 'jolly-brancher)
  (jolly-brancher-mode 1))
