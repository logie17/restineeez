(load-file "./router.el")

(setq restineeze-port 8085)

(defun home(req res)
  (add-to-list 'res '(:status . 200))
  (add-to-list 'res '(:body . "Hello World"))
  res)

(GET
 :path "/"
 :fn 'home)

(GET
 :path "/aaaa"
 :fn (lambda (req res)
       (add-to-list 'res '(:status . 200))
       (add-to-list 'res '(:body . "Hello World"))
       res))

(start-server)
