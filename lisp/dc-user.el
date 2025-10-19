;;; dc-user.el --- User information

;;; Code:

;;                   -------------------------
;;                        User credentials   
;;                   -------------------------

;; User name and email
(setq user-full-name "Danijel Camdzic")
(setq user-mail-address "danijelcamdzic@proton.me")



;;                   -------------------------
;;                        User directories   
;;                   -------------------------

;; Define the home directories variables
(defvar dc-android-home "/storage/emulated/0/")

;; Set the home directory based on system type
(defvar dc-home-directory dc-android-home)

;; Define variables which represent my home directory folders
(defvar dc-journal-directory (concat dc-home-directory "Journal/"))

(defun dc/regex-open-folder-from-home-directory ()
  "Open a folder from home directory in dired using regex search."
  (interactive)
  (let* ((search-term (read-string "Enter search term: "))
         (search-regex (concat ".*" search-term ".*"))
         (all-paths (ignore-errors (directory-files-recursively dc-home-directory search-regex t)))
         (directories (seq-filter 'file-directory-p all-paths))
         (choice (completing-read "Choose a directory: " directories)))
    (when choice
      (dired choice))))

;; Add keybinding menu for directories
(define-prefix-command 'dc-dired-map)
(global-set-key (kbd "C-c D") 'dc-dired-map)

;; Add keybindings
(define-key dc-dired-map (kbd "r") 'dc/regex-open-folder-from-home-directory)



;; Provide package
(provide 'dc-user)

;;; dc-user.el ends here
