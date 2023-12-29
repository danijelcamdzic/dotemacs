;;; bookmarks-config.el -- Eww and regular bookmarks configuration

;;; Code:

;; Dependencies
(require 'package-archive-config)       ; Melpa and use-package setup
(require 'user-config)                  ; User name and directories

;; Bookmarks configuration
(use-package bookmark
  :config
  (progn ;; Directories configuration
    (setq bookmark-default-file (concat my-documents-directory "Bookmarks/bookmarks")))
  )

;; Eww configuration
(use-package eww
  :config
  (progn ;; Directories configurations
    (setq eww-bookmarks-directory (concat my-documents-directory "Bookmarks/")))
  )

;; Bookmark+ configuration
(use-package bookmark+
  :quelpa (bookmark+ :fetcher github :repo "emacsmirror/bookmark-plus")
  :config
  (progn ;; Default files configuration for state and commands
    (setq bmkp-bmenu-state-file (expand-file-name ".emacs-bmk-bmenu-state.el" user-emacs-directory))
    (setq bmkp-bmenu-commands-file (expand-file-name ".emacs-bmk-bmenu-commands.el" user-emacs-directory))
    )
  )

;; Bookmarks+ functions
(defun my/modify-bookmark-path (orig-fun &rest args)
  "Modify the bookmark filename and directory based on system type before opening."
  (let* ((bookmark (car args))
         (bookmark-data (bookmark-get-bookmark bookmark))
         (filename (alist-get 'filename bookmark-data))
         (dired-directory (alist-get 'dired-directory bookmark-data)))
    ;; Modify filename for file bookmarks
    (when filename
      (if (eq system-type 'android)
          (progn
            (when (string-match-p (regexp-quote my-gnu-linux-home) filename)
              (setq filename (replace-regexp-in-string (regexp-quote my-gnu-linux-home) my-android-home filename)))
            (when (string-match-p (regexp-quote my-gnu-linux-home-extended) filename)
              (setq filename (replace-regexp-in-string (regexp-quote my-gnu-linux-home-extended) my-android-home filename))))
        (when (string-match-p (regexp-quote my-android-home) filename)
          (setq filename (replace-regexp-in-string (regexp-quote my-android-home) my-gnu-linux-home filename))))
      (setf (alist-get 'filename bookmark-data) filename))
    ;; Modify dired-directory for directory bookmarks
    (when dired-directory
      (if (eq system-type 'android)
          (progn
            (when (string-match-p (regexp-quote my-gnu-linux-home) dired-directory)
              (setq dired-directory (replace-regexp-in-string (regexp-quote my-gnu-linux-home) my-android-home dired-directory)))
            (when (string-match-p (regexp-quote my-gnu-linux-home-extended) dired-directory)
              (setq dired-directory (replace-regexp-in-string (regexp-quote my-gnu-linux-home-extended) my-android-home dired-directory))))
        (when (string-match-p (regexp-quote my-android-home) dired-directory)
          (setq dired-directory (replace-regexp-in-string (regexp-quote my-android-home) my-gnu-linux-home dired-directory))))
      (setf (alist-get 'dired-directory bookmark-data) dired-directory))

    (apply orig-fun args)))

(advice-add 'bookmark-jump :around #'my/modify-bookmark-path)


(provide 'bookmarks-config)

;;; bookmarks-config.el ends here
