;;; editor-config.el -- Text editing and buffer display configuration

;;; Code:

;; Set theme
(load-theme 'manoj-dark t)

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              indent-line-function 'insert-tab)

;; Custom faces
(custom-set-faces
 '(bold ((t (:foreground "#008000" :weight bold))))
 '(italic ((t (:foreground "#B0A030" :slant italic))))
 '(strike-through ((t (:foreground "#8B0000" :strike-through t)))))

;; Disable line numbers
(global-display-line-numbers-mode 0)

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

;; Programming lanuage specific settings
;; C/C++
(defun my/c-cpp-mode-setup ()
  (setq c-basic-offset 4))

(add-hook 'c-mode-common-hook 'my/c-cpp-mode-setup)

;; Doc-view configuration
(use-package doc-view
  :config
  (progn ;; Setup
    (setq doc-view-resolution 200))
  )

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


(provide 'editor-config)

;;; editor-config.el ends here
