(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot
  :depends-on (#:sicl-new-boot-phase-0
               #:sicl-new-boot-phase-1
               #:sicl-new-boot-phase-2
               #:sicl-new-boot-phase-3)
  :serial t
  :components
  ((:file "boot")))
