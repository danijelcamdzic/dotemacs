;;; completion-config.el -- Completion packages configuration

;;; Code:

;; Dependencies
(require 'package-manager-config)       ; Package manager configuration (melpa and quelpa)

;; Company configuration
(use-package company
  :ensure t
  :config
  (progn ;; Setup
    (company-mode 1)
    (add-hook 'after-init-hook 'global-company-mode))
  )

;; Orderless configuration
(use-package orderless
  :ensure t
  )

;; Vertico configuration
(use-package vertico
  :after orderless
  :ensure t
  :config
  (progn ;; Setup
    ;; Enable vertico
    (vertico-mode 1)

    ;; Set completion style
    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles . (partial-completion))))))
  )

(provide 'completion-config)

;;; completion-config.el ends here
