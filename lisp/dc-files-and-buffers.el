;;; dc-files-and-buffers.el --- Files and buffers configuration

;;; Code:

;;                   -------------------------
;;                             Files   
;;                   -------------------------

;; Disable backup and lock files
(setq create-lockfiles nil
      auto-save-default nil
      make-backup-files nil)

;; Set custom file
(setq custom-file  (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)



;;                   -------------------------
;;                            Buffers   
;;                   -------------------------

;; Set background and foreground color
(add-to-list 'default-frame-alist '(foreground-color . "white"))
(add-to-list 'default-frame-alist '(background-color . "#242424"))

;; Remove startup screen
(setq inhibit-startup-screen t)

;; Remove fringes from buffers
(set-fringe-mode 0)

(defun dc/kill-background-buffers ()
  "Kill all buffers that are not currently visible in any window."
  (interactive)
  (let ((visible-buffers (mapcar #'window-buffer (window-list))))
    (dolist (buffer (buffer-list))
      (unless (member buffer visible-buffers)
        (kill-buffer buffer)))))

;; Add keybinding menu for buffers
(define-prefix-command 'dc-buffer-map)
(global-set-key (kbd "C-c B") 'dc-buffer-map)

(define-key dc-buffer-map (kbd "k") 'dc/kill-background-buffers)



;;                   -------------------------
;;                     Package: dired-sidebar   
;;                   -------------------------

(use-package dired-sidebar
  :defer t
  :ensure t
  :config
  ;; Make the window size not fixed
  (setq dired-sidebar-window-fixed nil)
  )

(defun dc/dired-sidebar-toggle ()
  "Toggle `dired-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar))

;; Add keybindings
(define-key dc-dired-map (kbd "s") 'dc/dired-sidebar-toggle)



;;                   -------------------------
;;                       Package: doc-view   
;;                   -------------------------

(use-package doc-view
  :ensure nil
  :config
  ;; Set higher resolution for viewing documents
  (setq doc-view-resolution 400)
  )



;; Provide package
(provide 'dc-files-and-buffers)

;;; dc-files-and-buffers.el ends here
