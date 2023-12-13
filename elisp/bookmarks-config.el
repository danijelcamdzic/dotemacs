;;; bookmarks-config.el -- Eww and regular bookmarks configuration

;;; Code:

;; Dependencies
(require 'package-archive-config)       ; Melpa and use-package setup
(require 'user-config)                  ; User name and directories

;; Bookmarks
(use-package bookmark
  :config
  (progn ;; Directories configuration
    (setq bookmark-default-file (concat my-documents-directory "bookmarks/bookmarks")))
)

;; Eww
(use-package eww
  :config
  (progn ;; Directories configurations
    (setq eww-bookmarks-directory (concat my-documents-directory "bookmarks/")))
)


(provide 'bookmarks-config)

;;; bookmarks-config.el ends here
