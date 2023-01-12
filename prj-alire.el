;; Set up building with Alire -*- no-byte-compile : t -*-

(wisi-prj-select-cache
 "prj-alire.el"
 (create-alire-prj
  :name "wisi stephe-3 Alire eglot"
  :gpr-file gpr-file
  :xref-label 'gpr_query)
 "Makefile")

;; end of file
