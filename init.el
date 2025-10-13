;;; init.el - Personal configuration file for Emacs on Linux

;;; Code:

;; Add our `lisp` directory to Emacs's `load-path`
;; This tells Emacs where to find our custom .el files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Add packages from the lisp directory to the load path
;; Here will reside the packages manually downloaded from git
;; as well as standalone .el files
(let ((default-directory (expand-file-name (concat user-emacs-directory "lisp"))))
  (normal-top-level-add-subdirs-to-load-path))

;; Load the configuration modules in order
(require 'dc-packages)          ; Package management (MELPA, use-package)
(require 'dc-user)              ; User settings 
(require 'dc-files-and-buffers) ; Files and buffers configuration
(require 'dc-editing)           ; Editing configuration
(require 'dc-programming)       ; Programming configuration
(require 'dc-gui)               ; GUI configuration
(require 'dc-datetime)          ; Date-time configuration
(require 'dc-encryption)        ; Encryption configuration
(require 'dc-bookmarks)         ; Bookmarks configuration
(require 'dc-org-mode)          ; Org-mode configuration
(require 'dc-org-agenda)        ; Org-agenda configuration
(require 'dc-org-notifications) ; Org notifications configuration
(require 'dc-org-roam)          ; Org-roam configuration
(require 'dc-org-extra)         ; Extra org packages 



;;; init.el ends here
