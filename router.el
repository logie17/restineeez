(load-file "./serve.el")

(cl-defun addroutehandler (&key type path fn)
  (setq routes (cons (cons path fn) routes)))
  ;; (setq routes (cons (list (cons path fn)) routes)))

(cl-defun GET (&key path fn)
  (addroutehandler
   :type "GET"
   :fn fn
   :path path))

(cl-defun POST (&key path fn))
(cl-defun PUT (&key path fn))
