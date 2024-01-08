;;; viewer-config.el -- Viewing and media packages configuration

;;; Code:
(provide 'viewer-config)

;;; Dependencies
(require 'package-manager-config)

;;; Doc-view
;;;; Configuration
(use-package doc-view
  :config
  ;; Increase document resolution from default 100
  (setq doc-view-resolution 200)
  )

;;; Mpv
;;;; Configuration
(use-package mpv
  :ensure t
  )

;;; viewer-config.el ends here
