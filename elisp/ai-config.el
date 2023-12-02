;;; ai-config.el
;;
;; Author: Danijel Camdzic
;; Maintainer: Danijel Camdzic <danijelcamdzic@tuta.com>
;;
;;; Code:

;;; --------- Chatgpt-shell ---------
;;
;; Ensure that company package is installed and loaded
(unless (package-installed-p 'chatgpt-shell)
  (package-install 'chatgpt-shell))
(require 'chatgpt-shell)

;; Set API key to nil at the beginning
(setq chatgpt-shell-openai-key nil)

(defun my/set-chatgpt-shell-openai-key ()
  "Set the `chatgpt-shell-openai-key` variable from auth-source."
  (interactive)
  (setq chatgpt-shell-openai-key
        (auth-source-pick-first-password :host "api.openai.com")))

(defun my/open-chatgpt-shell ()
  "Set the OpenAI API key and then call the chatgpt-shell command."
  (interactive)
  (my/set-chatgpt-shell-openai-key)
  (chatgpt-shell))

;; Provide package for use
(provide 'ai-config)
