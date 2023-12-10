;;; early-init.el
;;
;; Author: Danijel Camdzic
;; Maintainer: Danijel Camdzic <danijelcamdzic@tuta.com>
;;
;; Description:
;; This is a file used in the Android GUI Emacs. It is making it
;; possible to use Termux app and its packages in Emacs.
;;
;; Usage:
;; Loaded by Emacs at startup before init.el. Not for manual execution.
;;
;; License: NO LICENCE
;;
;;; Code:

(setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		       (getenv "PATH")))
(setenv "LD_LIBRARY_PATH" (format "%s:%s"
				  "/data/data/com.termux/files/usr/lib"
				  (getenv "LD_LIBRARY_PATH")))
(push "/data/data/com.termux/files/usr/bin" exec-path)
