;;; networking-config.el -- Networking configuration

;;; Code:
(provide 'networking-config)

;;; Dependencies
(require 'package-manager-config)

;;; Websocket
;;;; Configuration
(use-package websocket
  :after org-roam
  :ensure t
  )

;;; networking-config.el ends here
