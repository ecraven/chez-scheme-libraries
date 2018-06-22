(library (scheme file)
  (export call-with-input-file call-with-output-file
          delete-file file-exists?
          open-binary-input-file open-binary-output-file
          open-input-file open-output-file
          with-input-from-file with-output-to-file)
  (import (chezscheme))
  (define open-binary-input-file open-file-input-port)
  (define open-binary-output-file open-file-output-port))
