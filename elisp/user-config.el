;;; user-config.el -- User details and directory configuration

;;; Code:

;; User name and email
(setq user-full-name "Danijel Camdzic")
(setq user-mail-address "danijelcamdzic@tuta.com")

;; Directories
(setq my-home-directory
      (cond
       ((eq system-type 'gnu/linux) "~/")
       ((eq system-type 'android) "/storage/emulated/0/")
       (t "~/")))
(setq my-notes-directory (concat my-home-directory "Notes/"))
(setq my-documents-directory (concat my-home-directory "Documents/"))


(provide 'user-config)

;;; user-config.el ends here
