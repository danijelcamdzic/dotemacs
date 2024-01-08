;;; bookmarks-config.el -- Bookmarks configuration

;;; Code:
(provide 'bookmarks-config)

;;; Dependencies
(require 'user-config)
(require 'package-manager-config)

;;; Eww
;;;; Configuration
(use-package eww
  :config
  ;; Set default eww-bookmarks directory
  (setq eww-bookmarks-directory (concat my-documents-directory "Bookmarks/"))
  )

;;; Bookmarks
;;;; Configuration
(use-package bookmark
  :config
  ;; Set default bookmark file
  (setq bookmark-default-file (concat my-documents-directory "Bookmarks/bookmarks"))
  )

;;; Bookmark+
;;;; Configuration
(use-package bookmark+
  :quelpa (bookmark+ :fetcher github :repo "emacsmirror/bookmark-plus")
  :config
  ;; Set default .emacs-bmk-bmenu-state.el file path
  (setq bmkp-bmenu-state-file (expand-file-name ".emacs-bmk-bmenu-state.el" user-emacs-directory))
  
  ;; Set default .emacs-bmk-bmenu-commands.el file path
  (setq bmkp-bmenu-commands-file (expand-file-name ".emacs-bmk-bmenu-commands.el" user-emacs-directory))
  )

;;;; Functions - Bookmark Paths on Different Platforms
(defun my/bookmark-jump--modify-bookmark-path-advice (orig-fun &rest args)
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

;; Add advice so bookmarks will be properly opened
(advice-add 'bookmark-jump :around #'my/bookmark-jump--modify-bookmark-path-advice)

;;; bookmarks-config.el ends here
