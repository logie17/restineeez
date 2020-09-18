(require 'cl-lib)

(setq routes (list))

(defvar response-codes
  '((100 . "Continue")
    (101 . "Switching Protocols")
    (102 . "Processing")
    (200 . "OK")
    (201 . "Created")
    (202 . "Accepted")
    (203 . "Non-authoritative Information")
    (204 . "No Content")
    (205 . "Reset Content")
    (206 . "Partial Content")
    (207 . "Multi-Status")
    (208 . "Already Reported")
    (226 . "IM Used")
    (300 . "Multiple Choices")
    (301 . "Moved Permanently")
    (302 . "Found")
    (303 . "See Other")
    (304 . "Not Modified")
    (305 . "Use Proxy")
    (307 . "Temporary Redirect")
    (308 . "Permanent Redirect")
    (400 . "Bad Request")
    (401 . "Unauthorized")
    (402 . "Payment Required")
    (403 . "Forbidden")
    (404 . "Not Found")
    (405 . "Method Not Allowed")
    (406 . "Not Acceptable")
    (407 . "Proxy Authentication Required")
    (408 . "Request Timeout")
    (409 . "Conflict")
    (410 . "Gone")
    (411 . "Length Required")
    (412 . "Precondition Failed")
    (413 . "Payload Too Large")
    (414 . "Request-URI Too Long")
    (415 . "Unsupported Media Type")
    (416 . "Requested Range Not Satisfiable")
    (417 . "Expectation Failed")
    (418 . "I'm a teapot")
    (421 . "Misdirected Request")
    (422 . "Unprocessable Entity")
    (423 . "Locked")
    (424 . "Failed Dependency")
    (426 . "Upgrade Required")
    (428 . "Precondition Required")
    (429 . "Too Many Requests")
    (431 . "Request Header Fields Too Large")
    (444 . "Connection Closed Without Response")
    (451 . "Unavailable For Legal Reasons")
    (499 . "Client Closed Request")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")
    (504 . "Gateway Timeout")
    (505 . "HTTP Version Not Supported")
    (506 . "Variant Also Negotiates")
    (507 . "Insufficient Storage")
    (508 . "Loop Detected")
    (510 . "Not Extended")
    (511 . "Network Authentication Required")
    (599 . "Network Connect Timeout Error")))

(defun httpd--normalize-header (header)
  "Destructively capitalize the components of HEADER."
  (mapconcat #'capitalize (split-string header "-") "-"))

(defun find-route (list name)
  "Find routes in our alist"
  (assoc name list))

(defun http-response-header (code)
  (let ((status (cdr (assq code response-codes))))
    (let ((header (format "HTTP/1.1 %d %s\r\n\r\n" code status)))
      header)))

(defun httpd-parse-header (header)
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
            (when (setf routeinfo (find-route routes uri-path))
              (let ((response_header (http-response-header 200)))
                (process-send-string proc response_header)
                (process-send-string proc "Date: Mon, 27 Jul 2009 12:28:53 GMT\nServer: restineeze (OS/2 Warp)\nLast-Modified: Wed, 22 Jul 2009 19:15:56 GMT\nContent-Length: 88\nContent-Type: text/html\nConnection: Closed\n\n<html>\n<body>\n<h1>Hello, World!</h1></body></html>")
                ))
            (process-send-eof proc))))))



(cl-defun addroutehandler (&key type path fn)
  (setq routes (cons (cons path fn) routes)))
  ;; (setq routes (cons (list (cons path fn)) routes)))

(cl-defun GET (&key path fn)
  (addroutehandler
   :type "GET"
   :fn 'fn
   :path path))

(cl-defun POST (&key path fn))
(cl-defun PUT (&key path fn))

(defun home(req res))

(defun start-server ()
  "This starts eserver"
  (interactive)
  (make-network-process :name "emacs-http-server"
                        :server t
                        :service 8083
                        :family 'ipv4
                        :filter 'server-filter)
  (print "The webserver is up!")
  (while t (sleep-for 60))
  (print "The webserver is up!")
  )

(defun httpd-stop ()
  "Stop the web server"
  (interactive)
  (when (process-status "emacs-http-server")
    (delete-process "emacs-http-server")))

(provide 'serve)

;(start-server)
