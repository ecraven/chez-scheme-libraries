(library (ecs base)
  (export aspect aspect? aspect-name aspect-type aspect-data
          set-aspect-data! make-aspect
          aspect-ref aspect-set! grow-aspect! create-aspect-data aspect-remove!
          make-entity! destroy-entity!
          aspect-fields grow-aspects! find-aspect register-aspect! for-all-entities
          register-event! execute-events! all-aspects
          construct-name entity-count)
  (import (chezscheme)
          (only (srfi :1) every))
  (include "base.scm"))
