(asdf:defsystem #:rsa-app
 :serial t
 :depends-on (#:ltk)
 :components ((:file "package")
              (:file "src/list-utils")
              (:file "src/file-read-write-bits")
              (:file "src/generate-primes")
              (:file "src/generate-rsa-keys")
              (:file "src/main")))
