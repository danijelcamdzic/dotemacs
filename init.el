;;; init.el --- Personal configuration file

;;; Code:

;; Custom elisp files for configuration
(let ((my-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "elisp" my-dir)))

(require 'package-archive-config)       ; Melpa and use-package setup
(require 'user-config)                  ; User name and directories
(require 'editor-config)                ; Text editing and buffer display configuration
(require 'completion-config)            ; Autocomplete and ordering packages configuration
(require 'gui-config)                   ; GUI configuration
(require 'dashboard-config)             ; Dashboard package configuration
(require 'org-config)                   ; Org and supporting/extending packages configuration
(require 'bookmarks-config)             ; Eww and regular bookmarks configuration
(require 'auth-config)                  ; GnuPg and auth-sources configuration
(require 'ai-config)                    ; AI tools configuration

;;; init.el ends here
