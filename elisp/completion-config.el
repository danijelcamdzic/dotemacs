;;; completion-config.el -- Completion packages configuration

;;; Code:
(provide 'completion-config)

;;; Dependencies
(require 'package-manager-config)

;;; Company
;;;; Configuration
(use-package company
  :ensure t
  :config
  ;; Enable company mode
  (company-mode 1)

  ;; Add hook to enable company mode globally
  (add-hook 'after-init-hook 'global-company-mode)
  )

;;; Orderless
;;;; Configuration
(use-package orderless
  :ensure t
  )

;;; Vertico
;;;; Configuration
(use-package vertico
  :after orderless
  :ensure t
  :config
  ;; Enable vertico
  (vertico-mode 1)

  ;; Set completion style and categories
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  )

;;; completion-config.el ends here
