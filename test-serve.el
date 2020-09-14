(require 'ert)
(require 'cl-lib)
(require 'serve)

(ert-deftest elnode-test-server ()
  "Test the server"
  (start-server))
