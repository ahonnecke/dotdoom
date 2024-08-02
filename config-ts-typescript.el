;;; ../src/home/.doom.d/config-typescript.el -*- lexical-binding: t; -*-

(add-hook 'typescript-ts-mode
          (lambda ()
            (setq-local fill-column 138)
            (setq-local ffip-patterns '("*.ts"))))
