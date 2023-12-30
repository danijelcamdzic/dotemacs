;;; init.el --- Personal configuration file for Emacs

;;; Code:

;; Custom elisp files for configuration
(let ((my-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "elisp" my-dir)))

;; Load configuration packages
(require 'user-config)                  ; User details and directory configuration
(require 'package-manager-config)       ; Package manager configuration (melpa and quelpa)
(require 'editor-config)                ; Text editing and buffer display configuration
(require 'viewer-config)                ; Viewer packages configuration
(require 'completion-config)            ; Completion packages configuration
(require 'gui-config)                   ; GUI configuration
(require 'dashboard-config)             ; Dashboard configuration
(require 'org-config)                   ; Org and supporting/extending packages configuration
(require 'bookmarks-config)             ; Bookmarks configuration
(require 'authorization-config)         ; GnuPg and auth-sources configuration
(require 'ai-config)                    ; AI tools configuration

;;; init.el ends here
