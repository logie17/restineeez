(require 'ert)
(require 'cl-lib)
(load-file "./serve.el")

(ert-deftest serve-test-server ()
  "Test the server"
  (message "This is a test")
  (let ((proc (async-start
               ;; What to do in the child process
               (lambda ()
                 (start-server)
                 222))))


    (with-current-buffer (url-retrieve-synchronously "http://localhost:8082/aaaa")
      (prog1
          (buffer-string)
        (kill-buffer)))
    (httpd-stop)

    (message "Test well go here")

    (async-get proc))))
