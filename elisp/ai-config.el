;;; ai-config.el -- AI tools configuration

;;; Code:

;; Dependencies
(require 'user-config)                  ; User details and directory configuration
(require 'package-manager-config)       ; Package manager configuration (melpa and quelpa)
(require 'authorization-config)         ; GnuPg and auth-sources configuration

;; Gptel configuration
(use-package gptel
  :ensure t
  :config
  (progn ;; API key
    ;; Set API key to nil at the beginning
    (setq gptel-api-key nil))
  )

;; Gptel functions
(defun my/set-gptel-openai-api-key ()
  "Set the `gptel-api-key` variable from auth-source."
  (interactive)
  (setq gptel-api-key
        (auth-source-pick-first-password :host "API:openai.com")))


(provide 'ai-config)

;;; ai-config.el ends here
