;;; init.el
;;
;; Author: Danijel Camdzic
;; Maintainer: Danijel Camdzic <danijelcamdzic@tuta.com>
;;
;; Description:
;; This is my personal Emacs initialization file. It sets up Emacs with a focus
;; on improved productivity and ease of use, incorporating various packages and
;; custom settings. Regularly updated to include new tools and optimizations.
;;
;; Usage:
;; Loaded by Emacs at startup. Not for manual execution. Modify with knowledge
;; of Emacs Lisp and the configuration's specifics.
;;
;;
;; License: NO LICENCE
;;
;;; Code:

;;; --------- Melpa ---------
;; Add melpa package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Initialize packages
(package-initialize)

;;; --------- elisp ---------
;; Custom .el files for additional configuration
(let ((my-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "elisp" my-dir)))

;; Basic editor configuration
(require 'editor-config)

;; GUI appearance and behavior
(require 'gui-config)

;; Org-mode for note taking
(require 'org-mode-config)

;; Org-agenda for task management
(require 'org-agenda-config)

;; GPG and authentication settings
(require 'auth-config)

;; AI tools and integrations
(require 'ai-config)

;;; init.el ends here
