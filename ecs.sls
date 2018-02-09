(library (ecs)
  (export define-aspect define-entity define-system define-event
          destroy-entity! entity-count execute-events!)
  (import (chezscheme)
          (only (srfi :1) list-index)
          (ecs base))
  (include "ecs/syntax.scm"))
