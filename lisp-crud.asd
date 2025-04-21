;;; lisp-crud.asd
(asdf:defsystem #:lisp-crud
  :description "A CRUD application with hexagonal architecture and dependency inversion"
  :author "Your Name"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-dbi
               #:cl-json
               #:postmodern 
               #:cl-postgres
               #:woo
               #:ningle
               #:log4cl
               #:alexandria
               #:bordeaux-threads
               #:cl-fad
               #:cl-who
               #:quri
               #:cl-dotenv
               #:clack
               #:uuid)
  :serial t
  :components (
    ;; Main package
    (:file "package")
    
    ;; Utilities
    (:module "src/utilities"
     :serial t
     :components (
       (:file "package")
       (:file "env")))
    
    ;; Domain layer - entities first
    (:module "src/domain/entities"
     :serial t
     :components (
       (:file "package")
       (:file "user.model")))
    
    ;; Domain layer - ports next
    (:module "src/domain/ports"
     :serial t
     :components (
       (:file "package")
       (:file "user.port")))
    
    ;; Domain layer - usecases last
    (:module "src/domain/usecases"
     :serial t
     :components (
       (:file "package")
       (:file "create-user.usecase")
       (:file "delete-user.usecase")
       (:file "update-user.usecase")))
    
    ;; Adapters - implementation of ports
    (:module "src/adapters/postgres"
     :serial t
     :components (
       (:file "package")
       (:file "client")
       (:file "user.adapter")
       (:file "presenters/user.model")))
    
    ;; DI after adapters since it uses them
    (:module "src/di"
     :serial t
     :components (
       (:file "package")
       (:file "domain.container")))
    
    ;; HTTP endpoints last
    (:module "src/adapters/entrypoints/http"
     :serial t
     :components (
       (:file "package")
       (:file "user.controller")))
    
    ;; Main application file
    (:file "lisp-crud")))