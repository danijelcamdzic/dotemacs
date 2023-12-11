;; init.el
;;
;; Author: Danijel Camdzic
;; Maintainer: Danijel Camdzic <danijelcamdzic@tuta.com>
;;
;; License: NO LICENCE

;; --------- Melpa ---------

;; Add melpa package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Initialize packages
(package-initialize)

;; --------- Custom ---------

;; Custom .el files for additional configuration
(let ((my-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "elisp" my-dir)))

(require 'editor-config)
(require 'gui-config)
(require 'org-mode-config)
(require 'org-agenda-config)
(require 'auth-config)
(require 'ai-config)
