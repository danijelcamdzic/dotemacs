;;; completion-config.el -- Autocomplete and ordering packages configuration

;;; Code:

;; Dependencies
(require 'package-archive-config)       ; Melpa and use-package setup
(require 'user-config)                  ; User name and directories

;; Company
(use-package company
  :ensure t
  :config
  (progn ;; Setup
    (company-mode 1)
    (add-hook 'after-init-hook 'global-company-mode))
  )

;; Orderless
(use-package orderless
  :ensure t
  )

;; Vertico
(use-package vertico
  :after orderless
  :ensure t
  :config
  (progn ;; Setup
    (vertico-mode 1)

    ;; Set completion style
    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles . (partial-completion))))))
  )

(provide 'completion-config)

;;; completion-config.el ends here
