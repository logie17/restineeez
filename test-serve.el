(require 'test-simple)
(test-simple-start) ;; Zero counters and start the stop watch.

;; Use (load-file) below because we want to always to read the source.
;; Also, we don't want no stinking compiled source.
(assert-t (load-file "./serve.el")
	  "Can't load serve.el - are you in the right directory?" )


(end-tests) ;; Stop the clock and print a summary
