;;; editor-config.el -- Text editing and buffer display configuration

;;; Code:
(provide 'editor-config)

;;; Dependencies
(require 'package-manager-config)

;;; Files and Buffers
;; Remove startup screen
(setq inhibit-startup-screen t)

;; Disable backup and lock files
(setq create-lockfiles nil
      auto-save-default nil
      make-backup-files nil)

;; Change buffer behavior on Android
(when (eq system-type 'android)
  ;; Buffer display settings
  (setq display-buffer-alist
        '((".*" (display-buffer-same-window) (inhibit-same-window . nil))))
  ;; Touchscreen keyboard spawn
  (setq touch-screen-display-keyboard t))

;;; Theme
;; Install gruvbox-theme
(use-package gruvbox-theme
  :ensure t
  )

;; Set gruvbox-theme as the system theme
(load-theme 'gruvbox-dark-hard t)

;; Remove fringes
(set-fringe-mode 0)

;;; Text editing
;;;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              indent-line-function 'insert-tab)

;;;; Text Faces
(custom-set-faces
 '(bold ((t (:foreground "#008000" :weight bold))))
 '(italic ((t (:foreground "#B0A030" :slant italic))))
 '(strike-through ((t (:foreground "#8B0000" :strike-through t)))))

;;;; Programming
;;;;; C/Cpp
(defun my/c-cpp-mode-setup ()
  "Set basic c and cpp offset."
  (setq c-basic-offset 4))

;; Set hook to set indentation when in c/cpp file
(add-hook 'c-mode-common-hook 'my/c-cpp-mode-setup)

;; Disable line numbers
(global-display-line-numbers-mode 0)

;;; IBuffer
;;;; Configuration
(use-package ibuffer-sidebar
  :ensure t
  :config
  )

;;;; Functions - IBuffer-sidebar Toggler
(defun my/ibuffer-sidebar-toggle ()
  "Toggle `ibuffer-sidebar'"
  (interactive)
  (ibuffer-sidebar-toggle-sidebar)
  (ibuffer-sidebar-mode))

;;; Dired-sidebar
;;;; Configuration
(use-package dired-sidebar
  :ensure t
  :config
  )

;;;; Functions - Dired-sidebar Toggler
(defun my/dired-sidebar-toggle ()
  "Toggle `dired-sidebar'"
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (dired-sidebar-mode))

;;; Pretty-hydra
;;;; Configuration
(use-package pretty-hydra
  :ensure t
  )

;;; Which-key
;;;; Configuration
(use-package which-key
  :ensure t
  :config
  ;; Setup which-key-mode
  (which-key-mode)
  )

;;; editor-config.el ends here
