;;; bookmarks-config.el -- Eww and regular bookmarks configuration

;;; Code:

;; Dependencies
(require 'package-archive-config)       ; Melpa and use-package setup
(require 'user-config)                  ; User name and directories

;; Bookmarks configuration
(use-package bookmark
  :config
  (progn ;; Directories configuration
    (setq bookmark-default-file (concat my-documents-directory "Bookmarks/bookmarks")))
  )

;; Eww configuration
(use-package eww
  :config
  (progn ;; Directories configurations
    (setq eww-bookmarks-directory (concat my-documents-directory "Bookmarks/")))
  )


(provide 'bookmarks-config)

;;; bookmarks-config.el ends here
