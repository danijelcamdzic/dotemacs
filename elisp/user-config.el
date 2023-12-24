;;; user-config.el -- User details and directory configuration

;;; Code:

;; User name and email
(setq user-full-name "Danijel Camdzic")
(setq user-mail-address "danijelcamdzic@tuta.com")

;; Define the home variables
(defvar my-android-home "/storage/emulated/0/")
(defvar my-gnu-linux-home "~/")

;; Set the home directory based on system type
(setq my-home-directory
      (cond
       ((eq system-type 'gnu/linux) my-gnu-linux-home)
       ((eq system-type 'android) my-android-home)
       (t my-gnu-linux-home)))
(setq my-books-directory (concat my-home-directory "Books/"))
(setq my-notes-directory (concat my-home-directory "Notes/"))
(setq my-documents-directory (concat my-home-directory "Documents/"))


(provide 'user-config)

;;; user-config.el ends here
