;;; init.el
;;
;; Author: Danijel Camdzic
;; Maintainer: Danijel Camdzic <danijelcamdzic@tuta.com>
;;
;; Description:
;; This is the personal Emacs initialization file of Danijel Camdzic.
;; It sets up the Emacs environment to suit personal preferences and workflows.
;; This file is a living document, regularly updated to incorporate new tools,
;; tweaks, and improvements to the Emacs experience.
;;
;; Usage:
;; This file is loaded by Emacs at startup. It is not meant to be manually
;; executed. Instead, changes are to be made with an understanding of Emacs
;; lisp and the current Emacs configuration.
;;
;; License: NO LICENCE
;;
;;; Code:

;;; --------- Melpa ---------
;;
;; Add melpa package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Initialize packages
(package-initialize)

;;; --------- elisp ---------
;;
;; Provide custom configuration .el files for the init
(let ((my-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "elisp" my-dir)))

;; Editor configuration
(require 'editor-config)

;; GUI configuration
(require 'gui-config)

;; Org-mode configuration
(require `org-mode-config)

;; Org-agenda configuration
(require 'org-agenda-config)

;; GPG and  auth-sources configuration
(require 'auth-config)

;; AI tools
(require 'ai-config)
