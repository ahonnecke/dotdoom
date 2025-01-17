;;; config-ripgrep.el -*- lexical-binding: t; -*-

(setq consult-ripgrep-args
      "rg --hidden --no-ignore-vcs --glob '!.cache/' --glob '!node_modules/' --glob '!uv.lock' -n --no-heading -M 200")
