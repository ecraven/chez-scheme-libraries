(library (scheme process-context)
  (export command-line ;; emergency-exit
          exit
          get-environment-variable
          ;; get-environment-variables
          )
  (import (chezscheme))
  (define get-environment-variable getenv))

