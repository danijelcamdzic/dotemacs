;;; ai-config.el -- AI tools configuration

;;; Code:

;; Dependencies
(require 'package-archive-config)       ; Melpa and use-package setup
(require 'user-config)                  ; User name and directories
(require 'authorization-config)         ; GnuPg and auth-sources configuration

;; Chatgpt-shell configuration
(use-package chatgpt-shell
  :ensure t
  :config
  (progn ;; API key configuration
    ;; Set API key to nil at the beginning
    (setq chatgpt-shell-openai-key nil))
  )

;; Chatgpt-shell functions
(defun my/set-chatgpt-shell-openai-key ()
  "Set the `chatgpt-shell-openai-key` variable from auth-source."
  (interactive)
  (setq chatgpt-shell-openai-key
        (auth-source-pick-first-password :host "API:openai.com")))

(defun my/open-chatgpt-shell ()
  "Set the OpenAI API key and then call the chatgpt-shell command."
  (interactive)
  (my/set-chatgpt-shell-openai-key)
  (chatgpt-shell))

;; Chatgpt-shell configuration
(use-package ob-chatgpt-shell
  :ensure t
  :after chatgpt-shell
  :config
  (progn ;; Setup
    (ob-chatgpt-shell-setup))
  )

(defun my/chatgpt-shell-prepend-variables (orig-fun body params)
  "Preprocess the ChatGPT shell block to prepend variables given before block is
executed."
  (let ((var-entry (alist-get :var params)))
    (when var-entry
      (when (consp var-entry)
        (let ((var-name (car var-entry))
              (var-value (cdr var-entry)))
          (setq body (concat (format "%s" var-value) "\n" body)))))
  (funcall orig-fun body params)))

(advice-add 'org-babel-execute:chatgpt-shell :around #'my/chatgpt-shell-prepend-variables)


(provide 'ai-config)

;;; ai-config.el ends here
