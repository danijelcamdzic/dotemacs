;;; viewer-config.el -- Viewer packages configuration

;;; Code:

;; Dependencies
(require 'package-manager-config)       ; Package manager configuration (melpa and quelpa)

;; Doc-view configuration
(use-package doc-view
  :config
  (progn ;; Setup
    (setq doc-view-resolution 200))
  )

;; Mpv configuration
(use-package mpv
  :ensure t
  )

(provide 'viewer-config)

;;; viewer-config.el ends here
