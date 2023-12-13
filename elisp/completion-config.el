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
    (global-company-mode 1))
)

;; Vertico
(use-package vertico
  :ensure t
  :config
  (vertico-mode 1)
  (progn ;; Setup
    (vertico-mode 1))
)

;; Orderless
(use-package orderless
  :ensure t
  :custom
  (progn ;; Setup
    (completion-styles '(orderless))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles . (partial-completion))))))
)


(provide 'completion-config)

;;; completion-config.el ends here
