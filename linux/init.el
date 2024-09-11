;;; init.el - Personal configuration file for Emacs on Linux

;;; Code:

;; =================================================================
;;                       Package management
;; =================================================================

;;                   -------------------------
;;                            Package  
;;                   -------------------------

;; Use package
(require 'package)

;; Disable package signature checks
;; Some packages don't have signatures so
;; installing with Melpa creates problems
;; if package signature check is enabled
(setq package-check-signature nil)

;; Initialize packages
(package-initialize)

;; Add melpa
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)



;;                   -------------------------
;;                          Use-package  
;;                   -------------------------

;; Use use-package
(require 'use-package)



;;                   -------------------------
;;                        Custom lisp code   
;;                   -------------------------

;; Add packages from the lisp directory to the load path
;; Here will reside the packages manually downloaded from git
;; as well as standalone .el files
(let ((default-directory (expand-file-name (concat user-emacs-directory "lisp"))))
  (normal-top-level-add-subdirs-to-load-path))



;; =================================================================
;;                             User
;; =================================================================

;;                   -------------------------
;;                        User credentials   
;;                   -------------------------

;; User name and email
(setq user-full-name "Danijel Camdzic")
(setq user-mail-address "danijelcamdzic@tuta.com")



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



;; =================================================================
;;                      Files and buffers
;; =================================================================

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



;; =================================================================
;;                       Document viewing
;; =================================================================

;;                   -------------------------
;;                       Package: doc-view   
;;                   -------------------------

(use-package doc-view
  :ensure nil
  :config
  ;; Set higher resolution for viewing documents
  (setq doc-view-resolution 400)
  )



;; =================================================================
;;                         Text editing
;; =================================================================

;;                   -------------------------
;;                     General configuration   
;;                   -------------------------

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              indent-line-function 'insert-tab)

;; Text faces
(custom-set-faces
 '(bold ((t (:foreground "#008000" :weight bold))))
 '(italic ((t (:foreground "#B0A030" :slant italic))))
 '(strike-through ((t (:foreground "#8B0000" :strike-through t)))))

;; Disable line numbers
(global-display-line-numbers-mode 0)



;;                   -------------------------
;;                              C/C++  
;;                   -------------------------

(defun dc/setup-c-cpp-mode ()
  "Set basic c and cpp offset."
  (setq c-basic-offset 4))

;; Enable line numbers for  C modes
(add-hook 'c-mode-common-hook (lambda () (display-line-numbers-mode 1)))

;; Set hook to set indentation when in c/cpp file
(add-hook 'c-mode-common-hook 'dc/setup-c-cpp-mode)



;;                   -------------------------
;;                             Python 
;;                   -------------------------

;; Set the indentation level for Python code
(setq python-indent-offset 4)

;; Enable line numbers for Python mode
(add-hook 'python-mode-hook (lambda () (display-line-numbers-mode 1)))



;;                   -------------------------
;;                        Package: magit  
;;                   -------------------------

(use-package magit
  :defer t
  :ensure t
  )



;;                   -------------------------
;;                        Package: which-key  
;;                   -------------------------

(use-package which-key
  :defer t
  :ensure t
  :config
  ;; Setup which-key-mode
  (which-key-mode)
  )



;;                   -------------------------
;;                     Package: pretty-hydra  
;;                   -------------------------

(use-package pretty-hydra
  :defer t
  :ensure t
  )



;;                   -------------------------
;;                        Package: company  
;;                   -------------------------

(use-package company
  :ensure t
  :config
  ;; Enable company mode
  (company-mode 1)

  ;; Add hook to enable company mode globally
  (add-hook 'after-init-hook 'global-company-mode)
  )



;;                   -------------------------
;;                        Package: orderless  
;;                   -------------------------

(use-package orderless
  :ensure t
  )



;;                   -------------------------
;;                        Package: vertico  
;;                   -------------------------

(use-package vertico
  :after orderless
  :ensure t
  :config
  ;; Enable vertico
  (vertico-mode 1)

  ;; Set completion style and categories
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  )



;; =================================================================
;;                              GUI
;; =================================================================

(defun dc/gui-hide-all-bars ()
  "Disable scroll bar, menu bar, and tool bar."
  (interactive)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(defun dc/gui-show-all-bars ()
  "Enable scroll bar, menu bar, and tool bar."
  (interactive)
  (scroll-bar-mode 1)
  (menu-bar-mode 1)
  (tool-bar-mode 1))

(defun dc/gui-scrolless-mode ()
  "Disable scroll bar."
  (interactive)
  (scroll-bar-mode -1))

;; Start Emacs without scroll bar
(dc/gui-scrolless-mode)

;; Add keybinding menu for GUI
(define-prefix-command 'dc-gui-map)
(global-set-key (kbd "C-c G") 'dc-gui-map)

;; Add keybindings
(define-key dc-gui-map (kbd "a") 'dc/gui-show-all-bars)
(define-key dc-gui-map (kbd "h") 'dc/gui-hide-all-bars)
(define-key dc-gui-map (kbd "s") 'dc/gui-scrolless-mode)



;; =================================================================
;;                         Date and time
;; =================================================================

;;                   -------------------------
;;                        Package: calendar  
;;                   -------------------------

(use-package calendar
  :ensure nil
  :config
  ;; Set calendar to start on Monday
  (setq calendar-week-start-day 1)
  )



;;                   -------------------------
;;                      Package: time-stamp  
;;                   -------------------------

(use-package time-stamp
  :ensure t
  :config
  ;; Set up time-stamp format
  (setq time-stamp-format "%Y-%m-%d %H:%M"
        time-stamp-start "# Edited: "
        time-stamp-end "$")

  ;; Add hook to save time-stamp string on every file save
  (add-hook 'before-save-hook 'time-stamp)
  )



;; =================================================================
;;                 Encryption and authentication
;; =================================================================

;;                   -------------------------
;;                          Package: epa  
;;                   -------------------------

(use-package epa
  :ensure nil
  :config
  ;; Set the gpg default program
  (setq epg-gpg-program "gpg2")

  ;; Enable the EPA file encryption/decryption features
  (epa-file-enable)

  ;; Set to nil to disable the key selection dialog
  ;; Emacs will use the default GPG key automatically
  (setq epa-file-select-keys nil)

  ;; Set the pinentry mode to loopback, allowing Emacs to
  ;; prompt for passphrases in the minibuffer
  ;; This is useful when running Emacs in a terminal or
  ;; environment where GUI pinentry dialogs are not available
  (setq epa-pinentry-mode 'loopback)
  )



;; =================================================================
;;                    Browsing and bookmarks
;; =================================================================

;;                   -------------------------
;;                          Package: eww  
;;                   -------------------------

(use-package eww
  :ensure nil
  :config
  ;; Set default eww-bookmarks directory
  (setq eww-bookmarks-directory user-emacs-directory)
  )



;;                   -------------------------
;;                       Package: bookmark  
;;                   -------------------------

(use-package bookmark
  :ensure nil
  :config
  ;; Set default bookmark file
  (setq bookmark-default-file (concat user-emacs-directory "bookmarks"))
  )

;; Add keybinding menu for bookmarks
(define-prefix-command 'dc-bookmark-map)
(define-key dc-buffer-map (kbd "b") 'dc-bookmark-map)

;; Add keybindings
(define-key dc-bookmark-map (kbd "l") 'list-bookmarks)
(define-key dc-bookmark-map (kbd "s") 'bookmark-set)



;; =================================================================
;;                             Org
;; =================================================================

;;                   -------------------------
;;                          Package: org  
;;                   -------------------------

(use-package org
  :ensure nil
  :config
  ;; Directory and startup settings
  (setq org-directory dc-notes-directory)
  (setq org-startup-indented t)
  (setq org-startup-folded 't)
  (setq org-startup-with-inline-images t)

  ;; Visual and layout settings
  (setq org-hide-leading-stars t)
  (setq org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
  (setq org-cycle-separator-lines -1)
  (setq org-image-actual-width t)
  (setq org-emphasis-alist   
        (quote (("*" bold)
                ("/" italic)
                ("_" underline)
                ("=" (:foreground "yellow" :background "black"))
                ("~" org-verbatim verbatim)
                ("+" (:strike-through t)))))
  (custom-set-faces
   '(org-tag ((t (:foreground "#d92727" :weight bold)))))

  ;; Hooks for various modes and settings
  (add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
  (add-hook 'org-mode-hook #'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'org-hide-block-all)
  (add-hook 'org-mode-hook 'org-hide-drawer-all)

  ;; Todo keywords and states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i!)" "|" "DONE(d!)" "SKIP(s!)" "FAIL(f!)")))

  ;; Logging and tracking settings
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out t)
  (setq org-log-states-order-reversed t)
  
  ;; Link and path settings
  (setq org-link-file-path-type 'relative)

  ;; Babel and code execution settings
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)    ; Enable Python
     (C . t)))       ; Enable C
  (setq org-babel-min-lines-for-block-output 100)
  (setq org-babel-python-command "python3")
  (setq org-edit-src-content-indentation 0)
  (setq org-confirm-babel-evaluate nil)
  )

(defun dc/org-insert-current-date-time ()
  "Insert the current date and time along with the three-letter weekday name in
the format YYYY-MM-DD Day H:M."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a %H:%M")))

(defun dc/org-clock-in ()
  "Clock in the current org heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-clock-in)
    (org-clock-in)))

(defun dc/org-clock-out ()
  "Clock out of the current org heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-clock-out)
    (org-clock-out)))

(defun dc/org-add-schedule ()
  "Add a scheduling timestamp to the current item in the Org Agenda or in
an org file."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (let* ((marker (or (org-get-at-bol 'org-marker)
                         (org-agenda-error)))
             (buffer (marker-buffer marker))
             (pos (marker-position marker))
             (inhibit-read-only t))
        (org-with-remote-undo buffer
          (with-current-buffer buffer
            (widen)
            (goto-char pos)
            (call-interactively 'org-schedule))))
    (call-interactively 'org-schedule)))

(defun dc/org-remove-schedule ()
  "Remove the scheduling timestamp from the current item in the Org Agenda
or in an org file."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (let* ((marker (or (org-get-at-bol 'org-marker)
                         (org-agenda-error)))
             (buffer (marker-buffer marker))
             (pos (marker-position marker))
             (inhibit-read-only t))
        (org-with-remote-undo buffer
          (with-current-buffer buffer
            (widen)
            (goto-char pos)
            (org-schedule '(4)))))
    (let ((inhibit-read-only t))
      (save-excursion
        (org-back-to-heading t)
        (org-schedule '(4))))))

(defun dc/org-todo-change-state ()
  "Change state of a current heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo)
    (org-todo)))

(defun dc/org-add-note ()
  "Add a note to an org heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-add-note)
    (org-add-note)))

(defun dc/org-logbook--parse-logbook-states (logbook beg buffer)
  "Parse a logbook string and return a list of entries with states."
  (let ((lines (split-string logbook "\n" t))
        (line-start-pos beg)
        entries)
    (dolist (line lines entries)
      (when (string-match "- State \"\\(.*?\\)\".*?\\[\\(.*?\\)\\]" line)
        (let* ((state (match-string 1 line))
               (date-string (match-string 2 line))
               (time (org-parse-time-string date-string))
               (date (list (nth 4 time) (nth 3 time) (nth 5 time)))
               (entry-begin-pos line-start-pos))
          (push (list state date entry-begin-pos buffer) entries)))
      (setq line-start-pos (+ line-start-pos (length line) 1)))))

(defun dc/org-logbook--parse-logbook-notes (logbook beg buffer)
  "Parse a logbook string and return a list of entries with notes."
  (let ((lines (split-string logbook "\n" t))
        (line-start-pos beg)
        entries)
    (dolist (line lines entries)
      (when (string-match "- Note taken on \\[\\(.*?\\)\\]" line)
        (let* ((date-string (match-string 1 line))
               (time (org-parse-time-string date-string))
               (date (list (nth 4 time) (nth 3 time) (nth 5 time)))
               (entry-begin-pos line-start-pos))
          (push (list "NOTE" date entry-begin-pos buffer) entries)))
      (setq line-start-pos (+ line-start-pos (length line) 1)))))

(defvar dc-logbook-calendar-view-active nil
  "Flag to indicate if the custom TODO calendar view is active.")

(defun dc/org-logbook--reset-calendar-view-flag ()
  "Reset the  calendar view flag."
  (setq dc-logbook-calendar-view-active nil))

;; Define custom faces for different TODO states
(defface dc-mark-DONE '((t :background "#006400")) "")
(defface dc-mark-SKIP '((t :background "#999900")) "")
(defface dc-mark-FAIL '((t :background "#8B0000")) "")
(defface dc-mark-DOING '((t :background "#4B0082")) "")
(defface dc-mark-NOTE '((t :background "#006400")) "")

(defun dc/org-logbook--mark-calendar-dates (entries)
  "Mark days in the calendar for each entry in ENTRIES."
  (setq dc-logbook-marked-entries entries)
  (let ((last-date (current-time)))
    (dolist (entry entries)
      (let* ((state (car entry))
             (date (cadr entry))
             (next-entry (cadr (member entry entries)))
             (end-date (if next-entry (cadr next-entry) last-date)))
        (when (calendar-date-is-visible-p date)
          (calendar-mark-visible-date date (intern (concat "dc-mark-" state))))
        (when (string= state "DOING")
          (let ((current-date date))
            (while (and (not (equal current-date end-date))
                        (calendar-date-is-visible-p current-date))
              (calendar-mark-visible-date current-date 'dc-mark-DOING)
              (setq current-date (calendar-gregorian-from-absolute
                                  (+ 1 (calendar-absolute-from-gregorian current-date)))))))))))

(defun dc/org-logbook--mark-calendar-date-reapply ()
  "Reapply markings to the calendar."
  (when dc-logbook-calendar-view-active
    (dc/org-logbook--mark-calendar-dates dc-logbook-marked-entries)))

(defun dc/org-logbook-display-states-on-calendar ()
  "Show the state history of the heading at point in the org-agenda buffer or an
org file on the year calendar."
  (interactive)
  (setq dc-logbook-calendar-view-active t)
  (setq dc-logbook-marked-entries '())
  (let* ((marker (if (eq major-mode 'org-agenda-mode)
                     (or (org-get-at-bol 'org-marker)
                         (org-agenda-error))
                   (point-marker)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker))
         beg end logbook entries)
    (with-current-buffer buffer
      (widen)
      (goto-char pos)
      (let ((current-pos (point)))
        (outline-next-heading)
        (let ((section-end (point)))
          (goto-char current-pos)
          (if (and (search-forward ":LOGBOOK:" section-end t)
                   (setq beg (line-beginning-position 2))
                   (search-forward ":END:" section-end t)
                   (setq end (line-beginning-position)))
              (progn
                (setq logbook (buffer-substring-no-properties beg end))
                (setq entries (dc/org-logbook--parse-logbook-states logbook beg buffer))
                (calendar)
                (dc/org-logbook--mark-calendar-dates entries))
            (error "No LOGBOOK found for this TODO.")))))))

(defun dc/org-logbook-display-notes-on-calendar ()
  "Show the notes of the heading at point in the org-agenda buffer or an org file on the year calendar."
  (interactive)
  (setq dc-logbook-calendar-view-active t)
  (setq dc-logbook-marked-entries '())
  (let* ((marker (if (eq major-mode 'org-agenda-mode)
                     (or (org-get-at-bol 'org-marker)
                         (org-agenda-error))
                   (point-marker)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker))
         beg end logbook entries)
    (with-current-buffer buffer
      (widen)
      (goto-char pos)
      (let ((current-pos (point)))
        (outline-next-heading)
        (let ((section-end (point)))
          (goto-char current-pos)
          (if (and (search-forward ":LOGBOOK:" section-end t)
                   (setq beg (line-beginning-position 2))
                   (search-forward ":END:" section-end t)
                   (setq end (line-beginning-position)))
              (progn
                (setq logbook (buffer-substring-no-properties beg end))
                (setq entries (dc/org-logbook--parse-logbook-notes logbook beg buffer))
                (calendar)
                (dc/org-logbook--mark-calendar-dates entries))
            (error "No LOGBOOK found for this TODO.")))))))

(defun dc/org-logbook--goto-entry (date)
  "Navigate to the logbook entry corresponding to DATE."
  (interactive (list (calendar-cursor-to-date t)))
  (if dc-logbook-calendar-view-active
      (let ((entry (cl-find-if (lambda (entry) (equal date (cadr entry))) dc-logbook-marked-entries)))
        (if entry
            (progn
              (switch-to-buffer-other-window (nth 3 entry))
              (delete-other-windows)
              (goto-char (nth 2 entry))
              (recenter-top-bottom 0))
          (message "No logbook entry found for this date.")))
    (message "Not supported in this calendar.")))

(with-eval-after-load 'calendar
  ;; Add hook to reapply markings each time the calendar is moved
  (add-hook 'calendar-move-hook 'dc/org-logbook--mark-calendar-date-reapply)
  ;; Add hook to reset the custom calendar view flag when the calendar is closed
  (add-hook 'calendar-exit-hook 'dc/org-logbook--reset-calendar-view-flag)
  ;; Bind terminal emacs "Enter" clicks
  (define-key calendar-mode-map (kbd "RET") 'dc/org-logbook--goto-entry)
  ;; Bind GUI emacs "Enter" clicks
  (define-key calendar-mode-map (kbd "<return>") 'dc/org-logbook--goto-entry))

;; Add keybinding menu for org
(define-prefix-command 'dc-org-map)
(global-set-key (kbd "C-c O") 'dc-org-map)

;; Add keybindings
(define-key dc-org-map (kbd "l") 'org-insert-link)
(define-key dc-org-map (kbd "i") 'dc/org-clock-in)
(define-key dc-org-map (kbd "o") 'dc/org-clock-out)
(define-key dc-org-map (kbd "a") 'dc/org-add-schedule)
(define-key dc-org-map (kbd "r") 'dc/org-remove-schedule)
(define-key dc-org-map (kbd "t") 'dc/org-todo-change-state)
(define-key dc-org-map (kbd "n") 'dc/org-add-note)
(define-key dc-org-map (kbd "S") 'dc/org-logbook-display-states-on-calendar)
(define-key dc-org-map (kbd "N") 'dc/org-logbook-display-notes-on-calendar)

;; Add org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "i") 'dc/org-clock-in)
  (define-key org-agenda-mode-map (kbd "o") 'dc/org-clock-out)
  (define-key org-agenda-mode-map (kbd "a") 'dc/org-add-schedule)
  (define-key org-agenda-mode-map (kbd "r") 'dc/org-remove-schedule)
  (define-key org-agenda-mode-map (kbd "t") 'dc/org-todo-change-state)
  (define-key org-agenda-mode-map (kbd "n") 'dc/org-add-note)
  (define-key org-agenda-mode-map (kbd "S") 'dc/org-logbook-display-states-on-calendar)
  (define-key org-agenda-mode-map (kbd "N") 'dc/org-logbook-display-notes-on-calendar)
  )



;;                   -------------------------
;;                      Package: org-attach  
;;                   -------------------------

(use-package org-attach
  :ensure nil
  :after org
  :config
  ;; Set attach directory
  (setq org-attach-id-dir (concat org-directory "data/"))
  
  ;; Use relative directories
  (setq org-attach-dir-relative t)
  
  ;; Store links in place where file is attached
  (setq org-attach-store-link-p 'attached)
  
  ;; Use inheritance
  (setq org-attach-use-inheritance t)
  
  ;; Remove default tag for attachments
  (setq org-attach-auto-tag nil)
  
  ;; Set default attachment method to copy
  (setq org-attach-method 'cp)
  )

(defvar dc-org-attach-search-starting-directory ""
  "Preferred starting directory to search files to attach in Org mode.")

(defun dc/org-attach-set-starting-directory (dir)
  "Set the `dc-org-attach-search-starting-directory'."
  (interactive "DSet starting directory: ")
  (setq dc-org-attach-search-starting-directory dir))

(defun dc/org-attach-reset-starting-directory ()
  "Reset the `dc-org-attach-search-starting-directory'."
  (interactive)
  (setq dc-org-attach-search-starting-directory ""))

(defun dc/org-attach-file-and-insert-link ()
  "Attach a file to the current Org entry and insert a link to it.
The attached file is copied to the attachment directory and a link is inserted at point."
  (interactive)
  (let* ((search-directory (if (string-empty-p dc-org-attach-search-starting-directory)
                               default-directory
                             dc-org-attach-search-starting-directory))
         (file (read-file-name "Select file to attach: " search-directory)))
    (org-attach-attach file nil 'cp)
    (insert (format "[[attachment:%s]]" (file-name-nondirectory file)))))

(defvar dc-org-attach-source-node nil
  "Temporary variable to store the source node for attachment copying.")

(defun dc/org-attach-copy-attachments-from-node-to-node ()
  "Copy marked attachments from one org-roam node to another using dired.
Function presumes that the attachments directories are made according to
id[0:1]/id[2:] rule."
  (interactive)
  (let ((source-node (org-roam-node-read)))
    (when source-node
      (let* ((source-node-id (org-roam-node-id source-node))
             (first-two (substring source-node-id 0 2))
             (rest-id (substring source-node-id 2))
             (source-attach-dir (expand-file-name (concat first-two "/" rest-id) org-attach-id-dir)))
        (if (file-exists-p source-attach-dir)
            (progn
              (setq dc-org-attach-source-node source-node)
              (dired source-attach-dir)
              (message "Mark attachments to copy with 'm', then press 't' to proceed.")
              (let ((copy-fn (lambda ()
                               (interactive)
                               (let ((marked-files (dired-get-marked-files))
                                     (source-node dc-org-attach-source-node))
                                 (setq dc-org-attach-source-node nil)
                                 (if marked-files
                                     (let ((target-node (org-roam-node-read)))
                                       (when target-node
                                         (let* ((target-node-id (org-roam-node-id target-node))
                                                (target-first-two (substring target-node-id 0 2))
                                                (target-rest-id (substring target-node-id 2))
                                                (target-attach-dir (expand-file-name (concat target-first-two "/" target-rest-id) org-attach-id-dir)))
                                           (unless (file-exists-p target-attach-dir)
                                             (make-directory target-attach-dir t))
                                           (dolist (file marked-files)
                                             (let ((target-file (expand-file-name (file-name-nondirectory file) target-attach-dir)))
                                               (copy-file file target-file t)))
                                           (message "Copied %d attachments from '%s' to '%s'."
                                                    (length marked-files)
                                                    (org-roam-node-title source-node)
                                                    (org-roam-node-title target-node)))))
                                   (message "No attachments marked for copying."))
                                 (kill-buffer (current-buffer))))))
                (local-set-key (kbd "t") copy-fn))
              (other-window 1))
          (message "No attachment directory found for node '%s'." (org-roam-node-title source-node)))))))

(defun dc/org-attach-move-attachments-from-node-to-node ()
  "Move marked attachments from one org-roam node to another using dired.
Function presumes that the attachments directories are made according to
id[0:1]/id[2:] rule."
  (interactive)
  (let ((source-node (org-roam-node-read)))
    (when source-node
      (let* ((source-node-id (org-roam-node-id source-node))
             (first-two (substring source-node-id 0 2))
             (rest-id (substring source-node-id 2))
             (source-attach-dir (expand-file-name (concat first-two "/" rest-id) org-attach-id-dir)))
        (if (file-exists-p source-attach-dir)
            (progn
              (setq dc-org-attach-source-node source-node)
              (dired source-attach-dir)
              (message "Mark attachments to move with 'm', then press 't' to proceed.")
              (let ((move-fn (lambda ()
                               (interactive)
                               (let ((marked-files (dired-get-marked-files))
                                     (source-node dc-org-attach-source-node))
                                 (setq dc-org-attach-source-node nil)
                                 (if marked-files
                                     (let ((target-node (org-roam-node-read)))
                                       (when target-node
                                         (let* ((target-node-id (org-roam-node-id target-node))
                                                (target-first-two (substring target-node-id 0 2))
                                                (target-rest-id (substring target-node-id 2))
                                                (target-attach-dir (expand-file-name (concat target-first-two "/" target-rest-id) org-attach-id-dir)))
                                           (unless (file-exists-p target-attach-dir)
                                             (make-directory target-attach-dir t))
                                           (dolist (file marked-files)
                                             (let ((target-file (expand-file-name (file-name-nondirectory file) target-attach-dir)))
                                               (rename-file file target-file t)))
                                           (message "Moved %d attachments from '%s' to '%s'."
                                                    (length marked-files)
                                                    (org-roam-node-title source-node)
                                                    (org-roam-node-title target-node)))))
                                   (message "No attachments marked for moving."))
                                 (kill-buffer (current-buffer))))))
                (local-set-key (kbd "t") move-fn))
              (other-window 1))
          (message "No attachment directory found for node '%s'." (org-roam-node-title source-node)))))))

(defun dc/org-attach-delete-attachments-from-node ()
  "Delete marked attachments from an org-roam node using dired.
Function presumes that the attachments directories are made according to
id[0:1]/id[2:] rule."
  (interactive)
  (let ((source-node (org-roam-node-read)))
    (when source-node
      (let* ((source-node-id (org-roam-node-id source-node))
             (first-two (substring source-node-id 0 2))
             (rest-id (substring source-node-id 2))
             (source-attach-dir (expand-file-name (concat first-two "/" rest-id) org-attach-id-dir)))
        (if (file-exists-p source-attach-dir)
            (progn
              (setq dc-org-attach-source-node source-node)
              (dired source-attach-dir)
              (message "Mark attachments to delete with 'm', then press 'd' to proceed.")
              (let ((delete-fn (lambda ()
                                 (interactive)
                                 (let ((marked-files (dired-get-marked-files))
                                       (source-node dc-org-attach-source-node))
                                   (setq dc-org-attach-source-node nil)
                                   (if marked-files
                                       (progn
                                         (dolist (file marked-files)
                                           (delete-file file))
                                         (message "Deleted %d attachments from '%s'."
                                                  (length marked-files)
                                                  (org-roam-node-title source-node)))
                                     (message "No attachments marked for deletion."))
                                   (kill-buffer (current-buffer))))))
                (local-set-key (kbd "d") delete-fn))
              (other-window 1))
          (message "No attachment directory found for node '%s'." (org-roam-node-title source-node)))))))

(defun dc/org-attach-delete-empty-org-attach-folders ()
  "Delete all empty directories in the `org-attach-id-dir`."
  (interactive)
  (let ((dir org-attach-id-dir))
    (unless (file-directory-p dir)
      (error "The `org-attach-id-dir` does not point to a valid directory"))
    (dolist (file (directory-files dir t directory-files-no-dot-files-regexp))
      (when (and (file-directory-p file)
                 (= (length (directory-files file nil directory-files-no-dot-files-regexp)) 0))
        (delete-directory file)
        (message "Deleted empty directory: %s" file)))))

(defun dc/org-attach-delete-unlinked-folders ()
  "Find and display Org-attach directories that do not correspond to an existing Org-roam node and optionally delete them."
  (interactive)
  (let ((id-list (mapcar (lambda (row) (car row))
                         (org-roam-db-query "SELECT id FROM nodes")))
        (known-directories '())
        (actual-directories '()))
    (dolist (id id-list)
      (let ((first-two (substring id 0 2))
            (rest-id (substring id 2)))
        (push (expand-file-name (concat first-two "/" rest-id) org-attach-id-dir) known-directories)))
    (dolist (entry (directory-files org-attach-id-dir t))
      (when (and (file-directory-p entry) (not (member (file-name-nondirectory entry) '("." ".."))))
        (dolist (subdir (directory-files entry t))
          (when (and (file-directory-p subdir) (not (member (file-name-nondirectory subdir) '("." ".."))))
            (push subdir actual-directories)))))
    (let ((unlinked-directories (cl-set-difference actual-directories known-directories :test 'string=)))
      (if unlinked-directories
          (progn
            (with-current-buffer (get-buffer-create "*Unlinked Org-Attach Directories*")
              (erase-buffer)
              (dolist (dir unlinked-directories)
                (insert dir "\n"))
              (display-buffer (current-buffer)))
            (if (yes-or-no-p "Unlinked directories found. Delete them?")
                (dolist (dir unlinked-directories)
                  (delete-directory dir t)
                  (message "Deleted directory: %s" dir))
              (message "No directories were deleted.")))
        (message "All attachment directories are linked to nodes."))))
  (dc/org-attach-delete-empty-org-attach-folders))

;; Add keybindings
(define-key dc-org-map (kbd "k") 'org-attach-attach)
(define-key dc-org-map (kbd "j") 'dc/org-attach-file-and-insert-link)



;;                   -------------------------
;;                      Package: org-tempo  
;;                   -------------------------

(use-package org-tempo
  :after org
  )



;;                   -------------------------
;;                      Package: org-agenda  
;;                   -------------------------

(use-package org-agenda
  :ensure nil
  :after org
  :config  
  ;; Agenda view settings
  (setq org-agenda-span 7)
  (setq org-agenda-show-future-repeats 'next)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-time-grid
        '((daily weekly today require-timed)
          (0000 0200 0400 0600 0800 1000 1200 1400 1600 1800 2000 2200 2359)
          "......" "----------------"))

  ;; Agenda item formatting
  (setq org-agenda-prefix-format  '((agenda . "  %t %c: ")
                                    (todo . "%t ")
                                    (tags . "")
                                    (search . "%i")))
  (setq org-agenda-scheduled-leaders '("" ""))
  
  ;; Agenda sorting strategy
  (setq org-agenda-sorting-strategy '((agenda time-up priority-up category-keep)
                                      (todo priority-up time-up category-keep)
                                      (tags time-up priority-up category-keep)
                                      (search time-up priority-up category-keep)))
  
  ;; Agenda custom faces
  (custom-set-faces
   '(org-scheduled ((t (:foreground "grey"))))
   '(org-scheduled-today ((t (:foreground "white"))))
   '(org-agenda-done ((t (:foreground "grey" :slant italic))))
   '(org-agenda-calendar-event ((t (:foreground "grey" :slant italic)))))
  )

(defun dc/org-agenda--switch-to-view (view-fn)
  "Switch to the given Org Agenda view function VIEW-FN and insert timeline."
  (if (eq major-mode 'org-agenda-mode)
      (progn
        (unless (eq org-agenda-type 'agenda)
          (org-agenda-exit)
          (org-agenda-list))
        (run-with-idle-timer 0.1 nil view-fn))
    (org-agenda-list)
    (run-with-idle-timer 0.1 nil view-fn)))

(defun dc/org-agenda-day-view ()
  "Switch to the Org Agenda daily view from anywhere in Emacs."
  (interactive)
  (dc/org-agenda--switch-to-view 'org-agenda-day-view))

(defun dc/org-agenda-week-view ()
  "Switch to the Org Agenda weekly view from anywhere in Emacs."
  (interactive)
  (dc/org-agenda--switch-to-view 'org-agenda-week-view))

(defun dc/org-agenda-year-view ()
  "Switch to the Org Agenda yearly view from anywhere in Emacs."
  (interactive)
  (dc/org-agenda--switch-to-view 'org-agenda-year-view))

(defun dc/org-agenda-open-logbook-mode ()
  "Open logbook mode in Org Agenda. To see full logbook view manually press
'l + [' on day, week or year agenda views."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (progn
        (org-agenda-log-mode)
        (org-agenda-manipulate-query-add))))

;; Add keybinding menu for agenda
(define-prefix-command 'dc-agenda-map)
(global-set-key (kbd "C-c A") 'dc-agenda-map)

;; Add keybindings
(define-key dc-agenda-map (kbd "d") 'dc/org-agenda-day-view)
(define-key dc-agenda-map (kbd "w") 'dc/org-agenda-week-view)
(define-key dc-agenda-map (kbd "y") 'dc/org-agenda-year-view)

;; Add org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "d") 'dc/org-agenda-day-view)
  (define-key org-agenda-mode-map (kbd "w") 'dc/org-agenda-week-view)
  (define-key org-agenda-mode-map (kbd "y") 'dc/org-agenda-year-view))



;;                   -------------------------
;;                   Package: org-super-agenda  
;;                   -------------------------

(use-package org-super-agenda
  :after org-agenda
  :ensure t
  :config
  ;; Enable org-super-agenda mode
  (org-super-agenda-mode)
  )

;; Redefine the auto-category group of the org-super-agenda
(org-super-agenda--def-auto-group category "their org-category property"
  :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
              (org-get-category))
  :header-form key)

(defun dc/org-agenda-todo-view ()
  "Open Org Agenda in the todos view mode with super agenda. Use category as groups"
  (interactive)
  (let ((org-super-agenda-groups '((:auto-category t)))
        (org-agenda-sorting-strategy '((todo priority-down category-keep))))
    (org-agenda nil "t")
    (setq org-super-agenda-groups '())))

;; Add keybindings
(define-key dc-agenda-map (kbd "v") 'dc/org-agenda-todo-view)

;; Add org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "v") 'dc/org-agenda-todo-view))



;;                   -------------------------
;;                         Package: alert 
;;                   -------------------------

(use-package alert
  :ensure t
  :config
  )



;;                   -------------------------
;;                      Package: org-alert 
;;                   -------------------------

(use-package org-alert
  :ensure t
  :after org
  :config
  ;; Setup alert type
  (setq alert-default-style 'notifications)
  
  ;; Setup timing
  (setq org-alert-interval 600
        org-alert-notify-cutoff 10
        org-alert-notify-after-event-cutoff 10)
  
  ;; Setup notification title (if using 'custom)
  (setq org-alert-notification-title "Org Alert Reminder")
  
  ;; Use non-greedy regular expression
  (setq org-alert-time-match-string
        "\\(?:SCHEDULED\\|DEADLINE\\):.*?<.*?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\(?:-[0-9]\\{2\\}:[0-9]\\{2\\}\\)?\\).*")
  
  ;; Enable org-alert
  (org-alert-enable)
  )

(defvar dc-org-alert-title-type 'custom
  "Control the title type for `org-alert' notifications.
   Possible values are:
      - 'custom': Uses `org-alert-notification-title' as the title of notifications sent.
      - 'category': Uses the category property from the org file. If the category is not defined,
                    it defaults to the filename.")

(defun dc/org-alert--get-category ()
  "Retrieve the category from the current org entry, or use the filename if no category exists."
  (or (org-entry-get nil "CATEGORY" t)
      (file-name-nondirectory (buffer-file-name))))

(defun org-alert--parse-entry--use-category-as-title-advice (orig-fun &rest args)
  "Advice for `org-alert--parse-entry' function to use the category as the notification title."
  (let ((head (org-alert--strip-text-properties (org-get-heading t t t t)))
        (category-or-file (dc/org-alert--get-category)))
    (cl-destructuring-bind (body cutoff) (org-alert--grab-subtree)
      (if (string-match org-alert-time-match-string body)
          (list head category-or-file (match-string 1 body) cutoff)
        nil))))

(defun org-alert--dispatch--use-category-as-title-advice (orig-fun &rest args)
  "Advice for `org-alert--dispatch' function to dispatch notifications with category as title."
  (let ((entry (org-alert--parse-entry)))
    (when entry
      (cl-destructuring-bind (head category-or-file time cutoff) entry
        (if time
            (when (org-alert--check-time time cutoff)
              (alert (concat time ": " head) :title category-or-file))
          (alert head :title category-or-file))))))

(defun dc/org-alert-update-advices ()
  "Add or remove advice based on the value of `dc-org-alert-title-type'."
  (cond ((eq dc-org-alert-title-type 'category)
         (advice-add 'org-alert--parse-entry :around #'org-alert--parse-entry--use-category-as-title-advice)
         (advice-add 'org-alert--dispatch :around #'org-alert--dispatch--use-category-as-title-advice))
        ((eq dc-org-alert-title-type 'custom)
         (advice-remove 'org-alert--parse-entry #'org-alert--parse-entry--use-category-as-title-advice)
         (advice-remove 'org-alert--dispatch #'org-alert--dispatch--use-category-as-title-advice))))

;; Set up category mode
(setq dc-org-alert-title-type 'category) 

;; Update to set up or remove advices based on dc-org-alert-title-type
(dc/org-alert-update-advices)



;;                   -------------------------
;;                      Package: org-roam 
;;                   -------------------------

(use-package org-roam
  :after org
  :ensure t
  :config
  ;; Set directories
  (setq org-roam-directory org-directory)
  (setq org-roam-dailies-directory (concat org-directory "dailies/"))

  ;; Setup org-roam
  (org-roam-setup)
  )

(defun dc/org-roam--get-node-heirarchy (node)
  "Get the hierarchy of NODE as a list of titles, excluding non-node headings.
The hierarchy includes the NODE title and its ancestor node titles."
  (let ((titles '())
        (title (org-roam-node-title node))
        (file-path (org-roam-node-file node))
        (file-title)
        (olp (org-roam-node-olp node)))
    (setq file-title (caar (org-roam-db-query [:select title :from nodes :where (= file $s1)] file-path)))
    (when (and file-title (not (equal file-title title)))
      (push file-title titles))
    (dolist (heading olp)
      (let ((heading-title (car (last (split-string heading "/"))))) ;; Extract the heading title
        (when (caar (org-roam-db-query [:select id :from nodes :where (= title $s1)] heading-title))
          (push heading-title titles))))
    (push title titles)
    (nreverse titles)))

(defvar dc-org-roam-hierarchy-display-separator
  (propertize "->" 'face '(shadow))
  "Separator for org-roam hierarchy displaying.")

(defun dc/org-roam--create-node-hierarchy-chain (node)
  "Return the hierarchy of NODE as a string with a predefine separator."
  (let ((hierarchy (dc/org-roam--get-node-heirarchy node)))
    (if (cdr hierarchy)
        (let* ((last-element (car (last hierarchy)))
               (non-last-elements (butlast hierarchy))
               (shadow-italicized-elements (mapcar (lambda (element)
                                                     (propertize element 'face '(shadow italic)))
                                                   non-last-elements)))
          (concat (mapconcat 'identity shadow-italicized-elements dc-org-roam-hierarchy-display-separator) dc-org-roam-hierarchy-display-separator last-element))
      (mapconcat 'identity hierarchy dc-org-roam-hierarchy-display-separator))))

(cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
  "Method for obtaining hierarchy display for org-roam nodes."
  (dc/org-roam--create-node-hierarchy-chain node))

(cl-defmethod org-roam-node-colon-tags ((node org-roam-node))
  "Tag formatting for org-roam nodes."
  (let ((tags (org-roam-node-tags node)))
    (if tags
        (concat " :" (mapconcat 'identity tags ":") ":")
      "")))

(cl-defmethod org-roam-node-node-type ((node org-roam-node))
  "Return a string which indicates whether a node is a `@roam' or a `@daily'."
  (let ((file-path (org-roam-node-file node)))
    (if (string-prefix-p (file-name-as-directory org-roam-dailies-directory) (file-name-directory file-path))
        " @daily"
      " @roam")))

;; Set the hierarchy display formatting
(setq org-roam-node-display-template
      (concat "${hierarchy}" "${node-type}" (propertize "${colon-tags}" 'face 'org-tag)))

(defvar dc-org-roam-hierarchy-insert-separator
  (propertize "->" 'face '(shadow))
  "Separator for org-roam hierarchy insertion.")

(defvar dc-org-roam-link-prefix ""
  "Prefix to be added to org-roam links before insertion.")

(defun dc/org-roam-reset-link-prefix ()
  "Sets the dc-org-roam-link-prefix to an empty string."
  (interactive)
  (setq dc-org-roam-link-prefix ""))

(defun dc/org-roam-set-link-prefix ()
  "Sets the dc-org-roam-link-prefix to prefix-string."
  (interactive)
  (setq prefix-string (read-string "Prefix string: "))
  (setq dc-org-roam-link-prefix prefix-string))

(defun dc/org-roam-insert-nodes-by-name-tag-or-property ()
  "Interactive function to insert Org-roam nodes filtered by titles, tags, and properties into the current buffer.
This function allows the user to choose between filtering nodes based on titles, tags, or properties, individually or in combination.
For titles and tags, the user can specify keywords to include and exclude.
The user can continuously add different properties to filter by, specifying the property name and value for each.
Nodes that match all specified criteria are then inserted with their hierarchy and linked using Org-roam's ID scheme."
  (interactive)
  (let ((use-titles (y-or-n-p "Filter by title? "))
        title-keywords title-exclude-keywords
        (use-tags)
        tag-keywords tag-exclude-keywords
        property-list
        all-nodes filtered-nodes sorted-nodes)
    
    (when use-titles
      (setq title-keywords (read-string "Include title keywords: "))
      (setq title-exclude-keywords (read-string "Exclude title keywords: ")))
    
    (setq use-tags (y-or-n-p "Filter by tags? "))
    (when use-tags
      (setq tag-keywords (read-string "Include tags: "))
      (setq tag-exclude-keywords (read-string "Exclude tags: ")))
    
    (while (y-or-n-p "Filter by a property? ")
      (let ((property-name (read-string "Property name: "))
            (property-value (read-string "Property value: ")))
        (push (cons property-name property-value) property-list)))

    (unwind-protect
        (atomic-change-group
          (setq all-nodes (org-roam-node-list))
          (setq filtered-nodes
                (cl-remove-if-not
                 (lambda (node)
                   (and (if use-titles
                            (let ((title-include-list (if (string= title-keywords "") '() (split-string title-keywords " ")))
                                  (title-exclude-list (if (string= title-exclude-keywords "") '() (split-string title-exclude-keywords " "))))
                              (and (or (null title-include-list)
                                       (cl-some (lambda (keyword)
                                                  (string-match-p (regexp-quote keyword) (org-roam-node-title node)))
                                                title-include-list))
                                   (or (null title-exclude-list)
                                       (cl-notany (lambda (exclude-keyword)
                                                    (string-match-p (regexp-quote exclude-keyword) (org-roam-node-title node)))
                                                  title-exclude-list))))
                          t)
                        (if use-tags
                            (let ((tag-include-list (if (string= tag-keywords "") '() (split-string tag-keywords " ")))
                                  (tag-exclude-list (if (string= tag-exclude-keywords "") '() (split-string tag-exclude-keywords " "))))
                              (and (or (null tag-include-list)
                                       (cl-some (lambda (keyword)
                                                  (cl-some (lambda (tag)
                                                             (string-match-p (regexp-quote keyword) tag))
                                                           (org-roam-node-tags node)))
                                                tag-include-list))
                                   (or (null tag-exclude-list)
                                       (cl-notany (lambda (exclude-keyword)
                                                    (cl-some (lambda (tag)
                                                               (string-match-p (regexp-quote exclude-keyword) tag))
                                                             (org-roam-node-tags node)))
                                                  tag-exclude-list))))
                          t)
                        (cl-every (lambda (prop-pair)
                                    (let ((node-prop (assoc (car prop-pair) (org-roam-node-properties node))))
                                      (and node-prop
                                           (string= (cdr node-prop) (cdr prop-pair)))))
                                  property-list)))
                 all-nodes))
          (setq sorted-nodes
                (sort filtered-nodes
                      (lambda (a b)
                        (let ((hierarchy-a (mapconcat #'identity (dc/org-roam--get-node-heirarchy a) dc-org-roam-hierarchy-insert-separator))
                              (hierarchy-b (mapconcat #'identity (dc/org-roam--get-node-heirarchy b) dc-org-roam-hierarchy-insert-separator)))
                          (string< hierarchy-a hierarchy-b)))))
          (dolist (node sorted-nodes)
            (let* ((id (org-roam-node-id node))
                   (hierarchy (dc/org-roam--get-node-heirarchy node))
                   (arrow-chain (if (> (length hierarchy) 1)
                                    (mapconcat #'identity hierarchy dc-org-roam-hierarchy-insert-separator)
                                  (org-roam-node-title node)))
                   (link (org-link-make-string (concat "id:" id) arrow-chain)))
              (insert (concat dc-org-roam-link-prefix link "\n"))
              (run-hook-with-args 'org-roam-post-node-insert-hook id arrow-chain))))
      (deactivate-mark))))

(defun dc/org-roam-find-nodes-by-name-tag-or-property ()
  "Interactive function to find  Org-roam nodes filtered by titles, tags, and properties.
This function allows the user to choose between filtering nodes based on titles, tags, or properties, individually or in combination.
For titles and tags, the user can specify keywords to include and exclude.
The user can continuously add different properties to filter by, specifying the property name and value for each.
Nodes that match all specified criteria are then displayed with their hierarchy."
  (interactive)
  (let ((use-titles (y-or-n-p "Filter by title? "))
        title-keywords title-exclude-keywords
        (use-tags)
        tag-keywords tag-exclude-keywords
        property-list
        all-nodes filtered-nodes sorted-nodes)
    
    (when use-titles
      (setq title-keywords (read-string "Include title keywords: "))
      (setq title-exclude-keywords (read-string "Exclude title keywords: ")))
    
    (setq use-tags (y-or-n-p "Filter by tags? "))
    (when use-tags
      (setq tag-keywords (read-string "Include tags: "))
      (setq tag-exclude-keywords (read-string "Exclude tags: ")))
    
    (while (y-or-n-p "Filter by a property? ")
      (let ((property-name (read-string "Property name: "))
            (property-value (read-string "Property value: ")))
        (push (cons property-name property-value) property-list)))

    (let ((filter-fn (lambda (node)
                       (and (if use-titles
                                (let ((title-include-list (if (string= title-keywords "") '() (split-string title-keywords " ")))
                                      (title-exclude-list (if (string= title-exclude-keywords "") '() (split-string title-exclude-keywords " "))))
                                  (and (or (null title-include-list)
                                           (cl-some (lambda (keyword)
                                                      (string-match-p (regexp-quote keyword) (org-roam-node-title node)))
                                                    title-include-list))
                                       (or (null title-exclude-list)
                                           (cl-notany (lambda (exclude-keyword)
                                                        (string-match-p (regexp-quote exclude-keyword) (org-roam-node-title node)))
                                                      title-exclude-list))))
                              t)
                            (if use-tags
                                (let ((tag-include-list (if (string= tag-keywords "") '() (split-string tag-keywords " ")))
                                      (tag-exclude-list (if (string= tag-exclude-keywords "") '() (split-string tag-exclude-keywords " "))))
                                  (and (or (null tag-include-list)
                                           (cl-some (lambda (keyword)
                                                      (cl-some (lambda (tag)
                                                                 (string-match-p (regexp-quote keyword) tag))
                                                               (org-roam-node-tags node)))
                                                    tag-include-list))
                                       (or (null tag-exclude-list)
                                           (cl-notany (lambda (exclude-keyword)
                                                        (cl-some (lambda (tag)
                                                                   (string-match-p (regexp-quote exclude-keyword) tag))
                                                                 (org-roam-node-tags node)))
                                                      tag-exclude-list))))
                              t)
                            (cl-every (lambda (prop-pair)
                                        (let ((node-prop (assoc (car prop-pair) (org-roam-node-properties node))))
                                          (and node-prop
                                               (string= (cdr node-prop) (cdr prop-pair)))))
                                      property-list)))))
      (let ((node (org-roam-node-read nil filter-fn)))
        (when node
          (org-roam-node-visit node))))))

;; Add keybinding menu for org-roam
(define-prefix-command 'dc-roam-map)
(global-set-key (kbd "C-c R") 'dc-roam-map)

;; Add keybindings
(define-key dc-roam-map (kbd "d") 'org-roam-dailies-find-date)
(define-key dc-roam-map (kbd "t") 'org-roam-dailies-goto-today)
(define-key dc-roam-map (kbd "f") 'org-roam-node-find)
(define-key dc-roam-map (kbd "i") 'org-roam-node-insert)



;;                   -------------------------
;;                       Package: org-ql 
;;                   -------------------------

(use-package org-ql
  :after org
  :ensure t
  )



;;                   -------------------------
;;                       Package: websocket 
;;                   -------------------------

(use-package websocket
  :after org-roam
  :ensure t
  )



;;                   -------------------------
;;                     Package: org-roam-ui
;;                   -------------------------

(use-package org-roam-ui
  :after (org-roam websocket)
  :ensure t
  )



;;                   -------------------------
;;                   Package: org-transclusions
;;                   -------------------------

(use-package org-transclusion
  :after org
  :ensure t
  )

(defun dc/org-transclusion-set-link-prefix ()
  "Sets the dc-org-roam-link-prefix to #+transclude: .
Used to add a prefix to the function which inserts org-roam
nodes based on tags."
  (interactive)
  (setq dc-org-roam-link-prefix "#+transclude: "))

(defun dc/org-transclusion-insert-node ()
  "Insert a transcluded link to an org-roam node."
  (interactive)
  (let ((node (org-roam-node-read)))
    (when node
      (let ((link (format "#+transclude: [[id:%s][%s]]"
                          (org-roam-node-id node)
                          (org-roam-node-title node))))
        (insert link)))))

;; Add keybindings
(define-key dc-org-map (kbd "z") 'dc/org-transclusion-insert-node)



;;                   -------------------------
;;                     Package: org-download 
;;                   -------------------------

(use-package org-download
  :defer t
  :ensure t
  :after org
  :config
  ;; Use attachments and not file links
  (setq org-download-method 'attach)
  
  ;; Don't create folders based on heading levels
  (setq-default org-download-heading-lvl nil)
  )

(defun dc/org-download-clipboard--prompt-for-name-advice (orig-fun &optional basename)
  "Advice to prompt for a basename before calling `org-download-clipboard'."
  (message "Calling advice function")
  (let ((name (if (called-interactively-p 'any)
                  (read-string "Enter image name (without extension): ")
                basename)))
    (funcall orig-fun (if (string-empty-p name) basename (concat name ".png")))))

(advice-add 'org-download-clipboard :around #'dc/org-download-clipboard--prompt-for-name-advice)

;; Add keybindings
(define-key dc-org-map (kbd "p") 'org-download-clipboard)



;;                   -------------------------
;;                       Package: org-ref 
;;                   -------------------------

(use-package org-ref
  :defer t
  :ensure t
  :after org
  )



;;                   -------------------------
;;                       Package: org-noter 
;;                   -------------------------

(use-package org-noter
  :defer t
  :ensure t  
  :after org 
  :config
  ;; Set the location of the notes
  (setq org-noter-notes-search-path '(org-directory))
  )



;;                   -------------------------
;;                         Package: mpv 
;;                   -------------------------

(use-package mpv
  :defer t
  :ensure t
  )



;;                   -------------------------
;;                    Package: org-media-note 
;;                   -------------------------

(use-package org-media-note
  :defer t
  :hook (org-mode .  org-media-note-mode)
  :bind (("H-v" . org-media-note-hydra/body))
  :config
  ;; Set up save method
  (setq org-media-note-screenshot-save-method 'attach)
  (setq org-media-note-screenshot-link-type-when-save-in-attach-dir 'attach)
  )

(defun dc/org-media-note--format-picture-file-name--prepend-timestamp-advice (orig-func &rest args)
  "Advice to prepend the current timestamp to the filename created by `org-media-note--format-picture-file-name'."
  (let ((original-filename (apply orig-func args))
        (timestamp (format-time-string "%Y-%m-%d_%H-%M-%S")))
    (concat timestamp "_" original-filename)))

(advice-add 'org-media-note--format-picture-file-name :around #'dc/org-media-note--format-picture-file-name--prepend-timestamp-advice)

(defun dc/remove-invalid-characters-from-filename (filename)
  "Remove invalid characters from filename in order for it to sync to different systems using syncthing."
  (replace-regexp-in-string "[/*\":<>?|]" "" filename))

(defun dc/org-media-note--format-picture-file-name--remove-invalid-characters-from-filename-advice (orig-func &rest args)
  "Advice to remove invalid characters from filename in `org-media-note--format-picture-file-name'."
  (dc/remove-invalid-characters-from-filename (apply orig-func args)))

(advice-add 'org-media-note--format-picture-file-name :around #'dc/org-media-note--format-picture-file-name--remove-invalid-characters-from-filename-advice)

;;; init.el ends here
