;;; gpt-config.el -- GPT tools configuration

;;; Code:

;; Dependencies
(require 'package-manager-config)       ; Package manager configuration (melpa and quelpa)

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

(provide 'gpt-config)

;;; gpt-config.el ends here
