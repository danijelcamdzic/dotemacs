;;; editor-config.el -- Text editing and buffer display configuration

;;; Code:

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


(provide 'editor-config)

;;; editor-config.el ends here
