;;; init.el -- Personal configuration file for Emacs

;;; Code:

;;; Custom Configuration Files
;; Add elisp folder to the load-path
(let ((my-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "elisp" my-dir)))

;; Load configuration packages
(require 'user-config)
(require 'package-manager-config)
(require 'editor-config)
(require 'viewer-config)
(require 'completion-config)
(require 'gui-config)
(require 'dashboard-config)
(require 'org-config)
(require 'bookmarks-config)
(require 'authorization-config)

;;; init.el ends here
