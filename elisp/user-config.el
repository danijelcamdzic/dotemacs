;;; user-config.el -- User credentials and directories configuration

;;; Code:
(provide 'user-config)

;;; User Credentials
;; User name and email
(setq user-full-name "Danijel Camdzic")
(setq user-mail-address "danijelcamdzic@tuta.com")

;;; User Directories
;; Define the home directories variables
(defvar my-android-home "/storage/emulated/0/")
(defvar my-gnu-linux-home "~/")
(defvar my-gnu-linux-home-extended "/home/danijelcamdzic/")

;; Set the home directory based on system type
(setq my-home-directory
      (cond
       ((eq system-type 'gnu/linux) my-gnu-linux-home)
       ((eq system-type 'android) my-android-home)
       (t my-gnu-linux-home)))

;; Set the user folders
(setq my-books-directory (concat my-home-directory "Books/"))
(setq my-notes-directory (concat my-home-directory "Notes/"))
(setq my-documents-directory (concat my-home-directory "Documents/"))

;;; user-config.el ends here
