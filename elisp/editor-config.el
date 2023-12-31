;;; editor-config.el -- Text editing and buffer display configuration

;;; Code:

;; Dependencies
(require 'package-manager-config)       ; Package manager configuration (melpa and quelpa)

;; Theme
;; Set theme
(load-theme 'manoj-dark t)

;; Remove fringes
(set-fringe-mode 0)

;; Text editing
;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              indent-line-function 'insert-tab)

;; Custom faces for text
(custom-set-faces
 '(bold ((t (:foreground "#008000" :weight bold))))
 '(italic ((t (:foreground "#B0A030" :slant italic))))
 '(strike-through ((t (:foreground "#8B0000" :strike-through t)))))

;; Programming lanuage specific settings
;; C/C++
(defun my/c-cpp-mode-setup ()
  (setq c-basic-offset 4))

(add-hook 'c-mode-common-hook 'my/c-cpp-mode-setup)

;; Disable line numbers
(global-display-line-numbers-mode 0)

;; Default Emacs behaviour changes
;; Remove startup screen
(setq inhibit-startup-screen t)

;; Disable backup and lock files
(setq create-lockfiles nil
      auto-save-default nil
      make-backup-files nil)

;; Android specific settings
(when (eq system-type 'android)
  ;; Buffer display settings
  (setq display-buffer-alist
        '((".*" (display-buffer-same-window) (inhibit-same-window . nil))))
  ;; Touchscreen keyboard spawn
  (setq touch-screen-display-keyboard t))

;; iBuffer-sidebar configuration
(use-package ibuffer-sidebar
  :ensure t
  :config
  )

;; iBuffer-sidebar functions
(defun my/ibuffer-sidebar-toggle ()
  "Toggle `ibuffer-sidebar'"
  (interactive)
  (ibuffer-sidebar-toggle-sidebar)
  (ibuffer-sidebar-mode))

;; Dired-sidebar configuration
(use-package dired-sidebar
  :ensure t
  :config
  )

;; Dired-sidebar functions
(defun my/dired-sidebar-toggle ()
  "Toggle `dired-sidebar'"
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (dired-sidebar-mode))

;; Pretty-hydra configuration
(use-package pretty-hydra
  :ensure t
  )


(provide 'editor-config)

;;; editor-config.el ends here
