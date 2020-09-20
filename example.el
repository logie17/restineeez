(load-file "./router.el")

(setq restineeze-port 8085)

(GET
 :path "/"
 :fn 'home)

(GET
 :path "/aaaa"
 :fn 'home)

(start-server)
