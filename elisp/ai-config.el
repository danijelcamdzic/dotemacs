;;; ai-config.el -- AI tools configuration

;;; Code:

;; Dependencies
(require 'package-archive-config)       ; Melpa and use-package setup
(require 'user-config)                  ; User name and directories
(require 'authorization-config)         ; GnuPg and auth-sources configuration

;; Chatgpt-shell
(use-package chatgpt-shell
  :ensure t
  :config
  (progn ;; API key configuration
    ;; Set API key to nil at the beginning
    (setq chatgpt-shell-openai-key nil))

  (progn ;; Chatgpt-shell functions
    (defun my/set-chatgpt-shell-openai-key ()
      "Set the `chatgpt-shell-openai-key` variable from auth-source."
      (interactive)
      (setq chatgpt-shell-openai-key
            (auth-source-pick-first-password :host "API:openai.com")))

    (defun my/open-chatgpt-shell ()
      "Set the OpenAI API key and then call the chatgpt-shell command."
      (interactive)
      (my/set-chatgpt-shell-openai-key)
      (chatgpt-shell)))
  )


(provide 'ai-config)

;;; ai-config.el ends here
