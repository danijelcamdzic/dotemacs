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
(defvar dc-gnu-linux-home "~/")
(defvar dc-gnu-linux-home-extended "/home/danijelcamdzic/")

;; Set the home directory based on system type
(defvar dc-home-directory dc-gnu-linux-home-extended)

;; Define variables which represent my home directory folders
(defvar dc-books-directory (concat dc-home-directory "Books/"))
(defvar dc-courses-directory (concat dc-home-directory "Courses/"))
(defvar dc-documents-directory (concat dc-home-directory "Documents/"))
(defvar dc-download-directory (concat dc-home-directory "Download/"))
(defvar dc-music-directory (concat dc-home-directory "Music/")) 
(defvar dc-notes-directory (concat dc-home-directory "Notes/"))
(defvar dc-pictures-directory (concat dc-home-directory "Pictures/"))   
(defvar dc-projects-directory (concat dc-home-directory "Projects/"))
(defvar dc-recordings-directory (concat dc-home-directory "Recordings/")) 
(defvar dc-videos-directory (concat dc-home-directory "Videos/"))

(defun dc/open-folder-from-home-directory ()
  "Open a folder from home directory in dired."
  (interactive)
  (let* ((directories '(("Books" . dc-books-directory)
                        ("Courses" . dc-courses-directory)
                        ("Documents" . dc-documents-directory)
                        ("Download" . dc-download-directory)
                        ("Music" . dc-music-directory)
                        ("Notes" . dc-notes-directory)
                        ("Pictures" . dc-pictures-directory)
                        ("Projects" . dc-projects-directory)
                        ("Recordings" . dc-recordings-directory)
                        ("Videos" . dc-videos-directory)))
         (choice (completing-read "Choose a directory: " directories))
         (directory-symbol (assoc-default choice directories))
         (directory (symbol-value directory-symbol)))
    (dired directory)))

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
(define-key dc-dired-map (kbd "h") 'dc/open-folder-from-home-directory)
(define-key dc-dired-map (kbd "r") 'dc/regex-open-folder-from-home-directory)



;; Provide package
(provide 'dc-user)

;;; dc-user.el ends here
