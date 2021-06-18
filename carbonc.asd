(load "~/quicklisp/setup.lisp")
(asdf:defsystem "carbonc"
    :author "Guilherme Nemeth"
    :description "Carbon Programming Language Compiler"
    :version "0.0.1"
    :license "MIT"
    :depends-on (:parse-float)
    :components ((:module "src"
                 :components
                 ((:file "parser")
                  (:file "main")))))