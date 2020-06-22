(require 'cl-lib)

(defun httpd--normalize-header (header)
  "Destructively capitalize the components of HEADER."
  (mapconcat #'capitalize (split-string header "-") "-"))

(defun httpd-parse-header (header)
  (print header)
  (setq start 0)
  (when (string-match "\\([^ ]+\\) +\\([^ ]+\\) +\\([^\r]+\\)\r\n" header)
    (let (
          (method (match-string 1 header))
          (path (decode-coding-string (match-string 2 header) 'iso-8859-1))
          (version (match-string 3 header))
          (headers ()))
      (setq start (match-end 0))
      (while (string-match "\\([-!#-'*+.0-9A-Z^_`a-z|~]+\\): *\\([^\r]+\\)\r\n" header start)
        (setq start (match-end 0))
        (let ((name (match-string 1 header))
              (value (match-string 2 header)))
          (push (list (httpd--normalize-header name)
                      (decode-coding-string value 'iso-8859-1)) headers)))
      (when (string-match "\r\n" header)
        (setq start (match-end 0))
        (cons (list method path version) (nreverse headers))))))

(defun httpd-parse-args (argstr)
  "Parse a string containing URL encoded arguments."
  (unless (zerop (length argstr))
    (mapcar (lambda (str)
              (mapcar 'httpd-unhex (split-string str "=")))
            (split-string argstr "&"))))

(defun httpd-unhex (str)
  (when str
    (let ((nonplussed (replace-regexp-in-string (regexp-quote "+") " " str)))
      (decode-coding-string (url-unhex-string nonplussed t) 'utf-8))))

(defun httpd-parse-uri (uri)
  (let ((p1 (string-match (regexp-quote "?") uri))
        (p2 (string-match (regexp-quote "#") uri))
        retval)
    (push (if p2 (httpd-unhex (substring uri (1+ p2)))) retval)
    (push (if p1 (httpd-parse-args (substring uri (1+ p1) p2))) retval)
    (push (substring uri 0 (or p1 p2)) retval)))
(defun server-filter (proc string)
  (let ((request (process-get proc :request)))
    (print string)
    (unless request
        (when (setf request (httpd-parse-header string))
          (process-put proc :request request)))
    (when request
        (let ((content-length (cadr (assoc "Content-Length" request))))
          (let* ((content (buffer-string))
                 (uri (cl-cadar request))
                 (parsed-uri (httpd-parse-uri (concat uri)))
                 (uri-path (httpd-unhex (nth 0 parsed-uri)))
                 (uri-query (append (nth 1 parsed-uri)
                                    (httpd-parse-args content))))
              ;(process-put proc :request nil)
            (setf request (nreverse (cons (list "Content" content)
                                          (nreverse request))))
            (print uri-path)
            ))))

  (process-send-string proc "Served\n"))

(setq routes (list))

(cl-defun addroutehandler (&key type path fn)
  (setq route (list "GET" path fn))
  (setq routes (cons route routes)))

(cl-defun GET (&key path fn)
  (addroutehandler
   :type "GET"
   :fn 'fn
   :path path))

(cl-defun POST (&key path fn))
(cl-defun PUT (&key path fn))

(defun home(req res))

(GET
 :path "/"
 :fn 'home)




(make-network-process :name "emacs-http-server"
                      :server t
                      :service 8081
                      :family 'ipv4
                      :filter 'server-filter)
(print "The webserver is up!")
(while t (sleep-for 60))
