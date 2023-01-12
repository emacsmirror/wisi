;; Set up building with Alire -*- no-byte-compile : t -*-
;;
;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


(wisi-prj-select-cache
 "prj-alire.el"
 (create-alire-prj
  :name "wisi main Alire"
  :gpr-file gpr-file
  :xref-label 'gpr_query)
 "Makefile")

;; end of file
