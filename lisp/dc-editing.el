;;; dc-editing.el --- Editing configuration

;;; Code:

;;                   -------------------------
;;                     General configuration   
;;                   -------------------------

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              indent-line-function 'insert-tab)

;; Text faces
(custom-set-faces
 '(bold ((t (:foreground "#4462B3" :weight bold))))
 '(italic ((t (:foreground "#B0A030" :slant italic))))
 '(strike-through ((t (:foreground "#8B0000" :strike-through t)))))

;; Disable line numbers
(global-display-line-numbers-mode 0)

;; Create shortcuts in Android with volume-up and volume-down keys
;; Volume up calls to execute the command
(global-set-key (kbd "<volume-up>") 'execute-extended-command)

;; Volume down is bound by default to org-ctrl-c-ctrl-c
(global-set-key (kbd "<volume-down>") 'org-ctrl-c-ctrl-c)

;; Make volume down programmable
(defun dc/bind-to-android-volume-down ()
  "Bind a command to the <volume-down> key on Android."
  (interactive)
  (let ((command (intern (completing-read "Command: " obarray 'commandp t))))
    (global-set-key (kbd "<volume-down>") command)))

;; Touchscreen keyboard spawn
(setq touch-screen-display-keyboard t))



;;                   -------------------------
;;                        Package: which-key  
;;                   -------------------------

(use-package which-key
  :defer t
  :ensure t
  :config
  ;; Setup which-key-mode
  (which-key-mode)
  )



;;                   -------------------------
;;                     Package: pretty-hydra  
;;                   -------------------------

(use-package pretty-hydra
  :defer t
  :ensure t
  )



;;                   -------------------------
;;                        Package: company  
;;                   -------------------------

(use-package company
  :ensure t
  :config
  ;; Enable company mode
  (company-mode 1)

  ;; Add hook to enable company mode globally
  (add-hook 'after-init-hook 'global-company-mode)
  )



;;                   -------------------------
;;                        Package: orderless  
;;                   -------------------------

(use-package orderless
  :ensure t
  )



;;                   -------------------------
;;                        Package: vertico  
;;                   -------------------------

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



;;                   -------------------------
;;                        Package: consult
;;                   -------------------------

(use-package consult
  :after orderless
  :ensure t
  :config
  )

(defun dc/org-roam-consult-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --multiline --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))


;; Provide package
(provide 'dc-editing)

;;; dc-editing.el ends here
