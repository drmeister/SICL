(cl:in-package #:asdf-user)

(defsystem #:cleavir-ir-visualizer
  :depends-on (#:cleavir-ir
               #:cleavir-hir
               #:mcclim
               #:clouseau)
  :serial t
  :components
  ((:file "packages")
   (:file "longest-path")
   (:file "instruction-layout")
   (:file "gui")))