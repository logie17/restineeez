(load-file "./router.el")

(GET
 :path "/"
 :fn 'home)

(GET
 :path "/aaaa"
 :fn 'home)

(start-server)
