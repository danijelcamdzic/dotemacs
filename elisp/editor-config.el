;;; editor-config.el -- Text editing and buffer display configuration

;;; Code:

;; Set theme
(load-theme 'misterioso t)

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

;; Buffer display settings
(setq display-buffer-alist
      '((".*" (display-buffer-same-window) (inhibit-same-window . nil))))

;; Touch-screen keyboard settings for Android
(when (eq system-type 'android)
  (setq touch-screen-display-keyboard t))

(use-package doc-view
  :config
  (progn ;; Setup
    (setq doc-view-resolution 200))
  )

(use-package ibuffer-sidebar
  :ensure t
  :config
  (progn ;; Sidebar toggling function
    (defun my/ibuffer-sidebar-toggle ()
      "Toggle `ibuffer-sidebar'"
      (interactive)
      (ibuffer-sidebar-toggle-sidebar)
      (ibuffer-sidebar-mode)))
  )

(use-package dired-sidebar
  :ensure t
  :config
  (progn ;; Sidebar toggling function
    (defun my/dired-sidebar-toggle ()
      "Toggle `dired-sidebar'"
      (interactive)
      (dired-sidebar-toggle-sidebar)
      (dired-sidebar-mode)))
  )


(provide 'editor-config)

;;; editor-config.el ends here
