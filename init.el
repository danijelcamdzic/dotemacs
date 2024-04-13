;;; init.el - Personal configuration file for Emacs

;;; Code:

;;; Package managers
;;;; Package - package
;;;;; Configuration
(require 'package)

;; Temporarily disable signature checks
(setq package-check-signature nil)

;; Initialize packages
(package-initialize)

;;;; Archive - melpa
;;;;; Configuration
;; Add melpa package archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;;;; Package - use-package
;;;;; Configuration
;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;;; Package - quelpa
;;;;; Configuration
(use-package quelpa
  :ensure t
  :init
  ;; Disable self upgrades to reduce startup time
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-update-melpa-p nil)
  )

;;;; Package - quelpa-use-package
;;;;; Configuration
(use-package quelpa-use-package
  :ensure t
  :after (quelpa)
  :config
  ;; Activate quelpa-use-package
  (quelpa-use-package-activate-advice)
  )

;;; User
;; User name and email
(setq user-full-name "Danijel Camdzic")
(setq user-mail-address "danijelcamdzic@tuta.com")

;;;; Keybinding menus
;; Add keybinding menu for dired
(define-prefix-command 'dc-dired-map)
(global-set-key (kbd "C-c d") 'dc-dired-map)

;; Add keybinding menu for buffers
(define-prefix-command 'dc-buffer-map)
(global-set-key (kbd "C-c b") 'dc-buffer-map)

;; Add keybinding menu for GUI
(define-prefix-command 'dc-gui-map)
(global-set-key (kbd "C-c g") 'dc-gui-map)

;; Add keybinding menu for bookmarks
(define-prefix-command 'dc-bookmark-map)
(define-key dc-buffer-map (kbd "b") 'dc-bookmark-map)

;; Add keybinding menu for org
(define-prefix-command 'dc-org-map)
(global-set-key (kbd "C-c o") 'dc-org-map)

;; Add keybinding menu for agenda
(define-prefix-command 'dc-agenda-map)
(global-set-key (kbd "C-c a") 'dc-agenda-map)

;; Add keybinding menu for org-roam
(define-prefix-command 'dc-roam-map)
(global-set-key (kbd "C-c r") 'dc-roam-map)

;;;; Directories
;; Define the home directories variables
(defvar dc-android-home "/storage/emulated/0/")
(defvar dc-gnu-linux-home "~/")
(defvar dc-gnu-linux-home-extended "/home/danijelcamdzic/")

;; Set the home directory based on system type
(defvar dc-home-directory
  (cond
   ((eq system-type 'gnu/linux) dc-gnu-linux-home-extended)
   ((eq system-type 'android) dc-android-home)
   (t dc-gnu-linux-home)))

;; Define variables which represent the home directory folders
(defvar dc-audio-directory (concat dc-home-directory "Audio/"))  
(defvar dc-books-directory (concat dc-home-directory "Books/"))
(defvar dc-documents-directory (concat dc-home-directory "Documents/"))
(defvar dc-download-directory (concat dc-home-directory "Download/"))        
(defvar dc-notes-directory (concat dc-home-directory "Notes/"))
(defvar dc-pictures-directory (concat dc-home-directory "Pictures/"))   
(defvar dc-projects-directory (concat dc-home-directory "Projects/"))    
(defvar dc-recordings-directory (concat dc-home-directory "Recordings/"))
(defvar dc-videos-directory (concat dc-home-directory "Videos/"))

;;;;; Function - Opening home directories in dired
(defun dc/open-folder-from-home-directory ()
  "Open a folder from home directory in dired."
  (interactive)
  (let* ((directories '(("Audio" . dc-audio-directory)
                        ("Books" . dc-books-directory)
                        ("Documents" . dc-documents-directory)
                        ("Download" . dc-download-directory)
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

;; Add keybindings
(define-key dc-dired-map (kbd "h") 'dc/open-folder-from-home-directory)
(define-key dc-dired-map (kbd "r") 'dc/regex-open-folder-from-home-directory)

;;;;; Package - dired-sidebar
;;;;;; Configuration
(use-package dired-sidebar
  :ensure t
  :config
  ;; Make the window size not fixed
  (setq dired-sidebar-window-fixed nil)
  )

;;;;;; Function - Toggling dired-sidebar
(defun dc/dired-sidebar-toggle ()
  "Toggle `dired-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar))

;; Add keybindings
(define-key dc-dired-map (kbd "s") 'dc/dired-sidebar-toggle)

;;; Theme
;; Install gruvbox-theme
(use-package gruvbox-theme
  :ensure t
  )

;; Set gruvbox-theme as the system theme
(load-theme 'gruvbox-dark-hard t)

;;; Buffers
;; Disable backup and lock files
(setq create-lockfiles nil
      auto-save-default nil
      make-backup-files nil)

;; Set custom file
(setq custom-file (concat dc-documents-directory "Emacs/custom.el"))
(load custom-file 'noerror)

;; Create shortcuts in Android with volume-up and volume-down keys
(when (eq system-type 'android)
  ;; Volume up calls to execute the command
  (global-set-key (kbd "<volume-up>") 'execute-extended-command)
  
  ;; Volume down is bound by default to org-ctrl-c-ctrl-c
  (global-set-key (kbd "<volume-down>") 'org-ctrl-c-ctrl-c)

  ;; Make volume down programmable
  (defun dc/bind-to-android-volume-down ()
    "Bind a command to the <volume-down> key on Android."
    (interactive)
    (let ((command (intern (completing-read "Command: " obarray 'commandp t))))
      (global-set-key (kbd "<volume-down>") command)))
  )

;; Change buffer behavior on Android
;; Since screen size on Android is not suitable (for me) to use
;; split screen, I choose to open each buffer in full screen mode
(when (eq system-type 'android)
  ;; Buffer display settings
  (setq display-buffer-alist
        '((".*" (display-buffer-same-window) (inhibit-same-window . nil))))
  
  ;; Touchscreen keyboard spawn
  (setq touch-screen-display-keyboard t))

;; Remove startup screen
(setq inhibit-startup-screen t)

;; Remove fringes from buffers
(set-fringe-mode 0)

;; Open org-agenda day view and org-roam daily note on startup
;; (unless called with a file argument)
(add-hook 'emacs-startup-hook
          (lambda ()
            (unless (> (length command-line-args) 1)
              (org-roam-dailies-goto-today)
              (dc/org-agenda-day-view)
              )))

;;;; Function - Killing all buffers
(defun dc/kill-background-buffers ()
  "Kill all buffers that are not currently visible in any window, except the *Messages*, *Org Agenda*,
*scratch* and today's Org Roam daily buffer."
  (interactive)
  (let ((visible-buffers (mapcar 'window-buffer (window-list)))
        (today-daily-file (format-time-string "%Y-%m-%d.org" (current-time))))
    (dolist (buffer (buffer-list))
      (unless (or (member buffer visible-buffers)
                  (string= (buffer-name buffer) "*Messages*")
                  (string= (buffer-name buffer) "*Org Agenda*")
                  (string= (buffer-name buffer) "*scratch*")
                  (string= (buffer-name buffer) today-daily-file))
        (kill-buffer buffer)))))

;; Set keybindings
(define-key dc-buffer-map (kbd "k") 'dc/kill-background-buffers)

;; Add org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "k") 'dc/kill-background-buffers))

;;;; Package - ibuffer
;;;;; Configuration
(use-package ibuffer-sidebar
  :ensure t
  :config
  )

;;;;; Function - Toggling ibuffer-sidebar
(defun dc/ibuffer-sidebar-toggle ()
  "Toggle `ibuffer-sidebar'."
  (interactive)
  (ibuffer-sidebar-toggle-sidebar))

;; Set keybindings
(define-key dc-buffer-map (kbd "s") 'dc/ibuffer-sidebar-toggle)

;;;; Package - imenu-list
;;;;; Configuration
(use-package imenu-list
  :ensure t
  :config
  )

;; Set keybindings
(define-key dc-buffer-map (kbd "l") 'imenu-list-smart-toggle)

;;; Editor
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

;;;; Visual
;; Enable outline-minor-mode as soon as .el file is opened
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

;;;;; Package - outline-minor-faces
;;;;;; Configuration
(use-package outline-minor-faces
  :ensure t
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    #'outline-minor-faces-mode)
  )

;;;;; Package - pretty-hydra
;;;;;; Configuration
(use-package pretty-hydra
  :ensure t
  )

;;;;; Package - which-key
;;;;;; Configuration
(use-package which-key
  :ensure t
  :config
  ;; Setup which-key-mode
  (which-key-mode)
  )

;;;;; Package - doc-view
;; Set higher resolution for viewing documents
(setq doc-view-resolution 400)

;;;; Completion
;;;;; Package - company
;;;;;; Configuration
(use-package company
  :ensure t
  :config
  ;; Enable company mode
  (company-mode 1)

  ;; Add hook to enable company mode globally
  (add-hook 'after-init-hook 'global-company-mode)
  )

;;;;; Package - orderless
;;;;;; Configuration
(use-package orderless
  :ensure t
  )

;;;;; Package - vertico
;;;;;; Configuration
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

;;;; Programming
;;;;; C/Cpp
(defun dc/setup-c-cpp-mode ()
  "Set basic c and cpp offset."
  (setq c-basic-offset 4))

;; Enable line numbers for  C modes
(add-hook 'c-mode-common-hook (lambda () (display-line-numbers-mode 1)))

;; Set hook to set indentation when in c/cpp file
(add-hook 'c-mode-common-hook 'dc/setup-c-cpp-mode)

;;;;; Python
;; Set the indentation level for Python code
(setq python-indent-offset 4)

;; Enable line numbers for Python mode
(add-hook 'python-mode-hook (lambda () (display-line-numbers-mode 1)))

;;;; Version control
;;;;; Package - magit
;;;;;; Configuration
(use-package magit
  :ensure t
  )

;;; GUI
;;;; Function - Manipulating GUI display modes
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

;; Add keybindings
(define-key dc-gui-map (kbd "a") 'dc/gui-show-all-bars)
(define-key dc-gui-map (kbd "h") 'dc/gui-hide-all-bars)
(define-key dc-gui-map (kbd "s") 'dc/gui-scrolless-mode)

;;; Org-mode
;;;; Package - org
;;;;; Configuration
(use-package org
  :ensure t
  :config
  ;; Set org directory
  (setq org-directory dc-notes-directory)

  ;; Set indentation for headings
  (setq org-startup-indented t)

  ;; Set coloumn limit for a paragraph to 80 characters
  (add-hook 'org-mode-hook (lambda () (setq fill-column 80)))

  ;; Turn on auto-fill mode
  (add-hook 'org-mode-hook #'turn-on-auto-fill)

  ;; Fold all blocks in a buffer
  (add-hook 'org-mode-hook 'org-hide-block-all)

  ;; Fold all drawers in a buffer
  (add-hook 'org-mode-hook 'org-hide-drawer-all)

  ;; Add new lines before headings but not in lists
  (setq org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))

  ;; Display inline images on startup
  (setq org-startup-with-inline-images t)

  ;; Set the width of the inline images to be the actual size
  (setq org-image-actual-width t)

  ;; Configure heading logs to be logged in "LOGBOOK" drawer
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out t)
  
  ;; Set path type to relative so it works on all platforms
  (setq org-link-file-path-type 'relative)

  ;; Languages support
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)    ; Enable Python
     (C . t)))       ; Enable C

  ;; Set the minimal number or lines in org-babel output before
  ;; they are stored in a code block
  (setq org-babel-min-lines-for-block-output 100)

  ;; Set the command for executing Python code in Org Babel
  (setq org-babel-python-command "python3")

  ;; Set the indentation level for org-babel source block content
  (setq org-edit-src-content-indentation 0)

  ;; Disable confirmation prompts when executing org-babel code blocks
  (setq org-confirm-babel-evaluate nil)

  ;; Set the org-todo-keywords and their states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i!)" "|" "DONE(d!)" "SKIP(s!)" "FAIL(f!)")))

  ;; Set custom faces for scheduled headings
  (custom-set-faces
   '(org-scheduled ((t (:foreground "#555555"))))
   '(org-scheduled-today ((t (:foreground "grey")))))
  )

;; Add keybindings
(define-key dc-org-map (kbd "l") 'org-insert-link)

;;;;; Function - Inserting datetime string
(defun dc/org-insert-current-date-time ()
  "Insert the current date and time along with the three-letter weekday name in
the format YYYY-MM-DD Day H:M."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a %H:%M")))

;;;;; Function - Clocking in and clocking out
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

;; Add keybindings
(define-key dc-org-map (kbd "i") 'dc/org-clock-in)
(define-key dc-org-map (kbd "o") 'dc/org-clock-out)

;; Add org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "i") 'dc/org-clock-in)
  (define-key org-agenda-mode-map (kbd "o") 'dc/org-clock-out))

;;;;; Function - Adding and removing a schedule
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

;; Add keybindings
(define-key dc-org-map (kbd "a") 'dc/org-add-schedule)
(define-key dc-org-map (kbd "r") 'dc/org-remove-schedule)

;; Add org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "a") 'dc/org-add-schedule)
  (define-key org-agenda-mode-map (kbd "r") 'dc/org-remove-schedule))

;;;;; Function - Changing a TODO state
(defun dc/org-todo-change-state ()
  "Change state of a current heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo)
    (org-todo)))

(defun dc/org-todo-change-state-on-date ()
  "Change state of the current heading and log with a chosen date."
  (interactive)
  (let ((selected-date (org-read-date nil t nil "Select Date:")))
    (if selected-date
        (progn
          (setq dc-time-override-lock t)
          (dc/time-adjust-time (format-time-string "<%Y-%m-%d %a>" selected-date))
          (advice-add 'current-time :override #'dc/time-override-current-time)
          (if (eq major-mode 'org-agenda-mode)
              (org-agenda-todo)
            (org-todo))
          (advice-remove 'current-time #'dc/time-override-current-time)
          (setq dc-adjusted-time nil)
          (setq dc-time-override-lock nil))
      (message "No date selected"))))

(defun dc/org-todo-skip-overdue-tasks ()
  "Mark tasks scheduled for yesterday or earlier as SKIP and
log them as changed on their scheduled date, but only if their
current state is TODO."
  (interactive)
  (dolist (file (org-agenda-files))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-scheduled-time-regexp nil t)
          (let ((scheduled-time (org-get-scheduled-time (point)))
                (todo-state (org-get-todo-state)))
            (when (and scheduled-time
                       (string= todo-state "TODO")
                       (< (time-to-days scheduled-time)
                          (time-to-days (current-time))))
              (unless dc-time-override-lock
                (setq dc-time-override-lock t)
                (dc/time-adjust-time (format-time-string "<%Y-%m-%d %a>" scheduled-time))
                (advice-add 'current-time :override #'dc/time-override-current-time)
                (org-todo "SKIP")
                (advice-remove 'current-time #'dc/time-override-current-time)
                (setq dc-adjusted-time nil)
                (setq dc-time-override-lock nil)))))))))

;; Add keybindings
(define-key dc-org-map (kbd "t") 'dc/org-todo-change-state)

;; Add org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "t") 'dc/org-todo-change-state))

;;;;; Function - Adding notes
(defun dc/org-add-note ()
  "Add a note to an org heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-add-note)
    (org-add-note)))

(defun dc/org-add-note-on-date ()
  "Change state of the current heading and log with a chosen date."
  (interactive)
  (let ((selected-date (org-read-date nil t nil "Select Date:")))
    (if selected-date
        (progn
          (setq dc-time-override-lock t)
          (dc/time-adjust-time (format-time-string "<%Y-%m-%d %a>" selected-date))
          (advice-add 'current-time :override #'dc/time-override-current-time)
          (if (eq major-mode 'org-agenda-mode)
              (org-agenda-add-note)
            (org-add-note))
          (advice-remove 'current-time #'dc/time-override-current-time)
          (setq dc-adjusted-time nil)
          (setq dc-time-override-lock nil))
      (message "No date selected"))))

;; Add keybindings
(define-key dc-org-map (kbd "n") 'dc/org-add-note)

;; Add org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "n") 'dc/org-add-note))

;;;;; Function - Visualizing logbook on calendar for notes and TODOs
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

;; Add keybindings
(define-key dc-org-map (kbd "S") 'dc/org-logbook-display-states-on-calendar)
(define-key dc-org-map (kbd "N") 'dc/org-logbook-display-notes-on-calendar)

;; Add org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "S") 'dc/org-logbook-display-states-on-calendar)
  (define-key org-agenda-mode-map (kbd "N") 'dc/org-logbook-display-notes-on-calendar))

;;;; Package - org-agenda
;;;;; Configuration
(use-package org-agenda
  :after org
  :config  
  ;; Set the prefix format for agenda items
  (setq org-agenda-prefix-format  '((agenda . "  %t ")
                                    (todo . "%t ")
                                    (tags . "")
                                    (search . "%i")))
  
  ;; Define sorting strategy for agenda items
  (setq org-agenda-sorting-strategy '((agenda time-up priority-up category-keep)
                                      (todo priority-up time-up category-keep)
                                      (tags time-up priority-up category-keep)
                                      (search time-up priority-up category-keep)))
  
  ;; Customize leaders for scheduled items in the agenda
  (setq org-agenda-scheduled-leaders '("" ""))
  
  ;; Set the number of days displayed in the agenda view
  (setq org-agenda-span 7)
  
  ;; Configure the display of future repeats in the agenda
  (setq org-agenda-show-future-repeats 'next)
  
  ;; Enable the use of a time grid in the agenda view
  (setq org-agenda-use-time-grid t)
  
  ;; Define the time grid for the agenda view
  (setq org-agenda-time-grid
        '((daily weekly today require-timed)
          (0000 0200 0400 0600 0800 1000 1200 1400 1600 1800 2000 2200 2359)
          "......" "----------------"))
  )

;;;;; Function - Changing org-agenda TODO views
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

;; Add keybindings
(define-key dc-agenda-map (kbd "d") 'dc/org-agenda-day-view)
(define-key dc-agenda-map (kbd "w") 'dc/org-agenda-week-view)
(define-key dc-agenda-map (kbd "y") 'dc/org-agenda-year-view)

;; Add org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "d") 'dc/org-agenda-day-view)
  (define-key org-agenda-mode-map (kbd "w") 'dc/org-agenda-week-view)
  (define-key org-agenda-mode-map (kbd "y") 'dc/org-agenda-year-view))

;;;;; Function - Integrating org-agenda-files between Android and Linux
(defun dc/org-agenda-adjust-org-agenda-files-paths ()
  "Adjust the paths in `org-agenda-files` based on the system type.
The function reads the org-agenda-files list and adjusts the paths
based on the system type."
  (setq org-agenda-files
        (mapcar (lambda (file)
                  (cond ((eq system-type 'android)
                         (replace-regexp-in-string (regexp-quote dc-gnu-linux-home) dc-android-home
                                                   (replace-regexp-in-string (regexp-quote dc-gnu-linux-home-extended) dc-android-home file)))
                        (t
                         (replace-regexp-in-string (regexp-quote dc-android-home) dc-gnu-linux-home file))))
                org-agenda-files)))

;; Call the function to adjust the paths
(dc/org-agenda-adjust-org-agenda-files-paths)

;;;; Package - org-super-agenda
;;;;; Configuration
(use-package org-super-agenda
  :after org-agenda
  :ensure t
  :config
  ;; Enable org-super-agenda mode
  (org-super-agenda-mode)
  )

;;;;; Function - Getting TODO parent name automatically
(defun dc/org-super-agenda-get-todo-parent (item)
  "Get the parent heading of ITEM, or if none, the file title or filename."
  (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
    (if (org-up-heading-safe)
        (org-entry-get nil "ITEM") 
      (let ((keywords (org-collect-keywords '("TITLE"))))
        (if keywords
            (car (cdr (assoc "TITLE" keywords))) 
          (file-name-nondirectory (buffer-file-name))))))) 

(org-super-agenda--def-auto-group parent "their parent heading or file title/filename"
  :key-form (dc/org-super-agenda-get-todo-parent item))

;;;;; Function - Changing to all TODOs view
(defun dc/org-agenda-todo-view ()
  "Open Org Agenda in the todos view mode with super agenda. Use file title as groups"
  (interactive)
  (let ((org-super-agenda-groups '((:auto-parent t)))
        (org-agenda-sorting-strategy '((todo priority-down category-keep))))
    (org-agenda nil "t")
    (setq org-super-agenda-groups '())))

;; Add keybindings
(define-key dc-agenda-map (kbd "v") 'dc/org-agenda-todo-view)

;; Add org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "v") 'dc/org-agenda-todo-view))

;;;; Package - org-roam
;;;;; Configuration
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

;; Add keybindings
(define-key dc-roam-map (kbd "d") 'org-roam-dailies-find-date)
(define-key dc-roam-map (kbd "t") 'org-roam-dailies-goto-today)
(define-key dc-roam-map (kbd "f") 'org-roam-node-find)
(define-key dc-roam-map (kbd "i") 'org-roam-node-insert)

;;;;; Function - Getting node hierarchy
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

;;;;; Function - Displaying nodes in org-roam search
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

;;;;; Function - Inserting nodes by tags
(defvar dc-org-roam-hierarchy-insert-separator
  (propertize "->" 'face '(shadow))
  "Separator for org-roam hierarchy insertion.")

(defvar dc-org-roam-link-prefix ""
  "Prefix to be added to org-roam links before insertion.")

(defun dc/org-roam-reset-link-prefix ()
  "Sets the dc-org-roam-link-prefix to an empty string."
  (interactive)
  (setq dc-org-roam-link-prefix ""))

(defun dc/org-roam-set-link-prefix (prefix-string)
  "Sets the dc-org-roam-link-prefix to prefix-string."
  (interactive)
  (setq dc-org-roam-link-prefix prefix-string))

(defun dc/org-roam-insert-nodes-by-tags(keywords exclude-keywords &optional filter-fn)
  "Inserts all Org-roam nodes connected to the provided keywords and not connected to the exclude keywords.
KEYWORDS is a space-separated list of keywords to find the connected nodes.
EXCLUDE-KEYWORDS is a space-separated list of keywords to exclude nodes.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
  (interactive "sKeywords: \nsExclude Keywords: ")
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* ((all-nodes (org-roam-node-list))
               (keywords-list (if (string= keywords "") '() (split-string keywords " ")))
               (exclude-keywords-list (if (string= exclude-keywords "") '() (split-string exclude-keywords " ")))
               (filtered-nodes (cl-remove-if-not
                                (lambda (node)
                                  (and (if keywords-list
                                           (cl-every (lambda (keyword)
                                                       (or (string-match-p (regexp-quote keyword) (org-roam-node-title node))
                                                           (cl-some (lambda (tag)
                                                                      (string-match-p (regexp-quote keyword) tag))
                                                                    (org-roam-node-tags node))))
                                                     keywords-list)
                                         t)
                                       (if exclude-keywords-list
                                           (cl-notany (lambda (exclude-keyword)
                                                        (or (string-match-p (regexp-quote exclude-keyword) (org-roam-node-title node))
                                                            (cl-some (lambda (tag)
                                                                       (string-match-p (regexp-quote exclude-keyword) tag))
                                                                     (org-roam-node-tags node))))
                                                      exclude-keywords-list)
                                         t)
                                       (or (not filter-fn) (funcall filter-fn node))))
                                all-nodes))
               (sorted-nodes (sort filtered-nodes
                                   (lambda (a b)
                                     (let ((hierarchy-a (mapconcat #'identity (dc/org-roam--get-node-heirarchy a) dc-org-roam-hierarchy-insert-separator))
                                           (hierarchy-b (mapconcat #'identity (dc/org-roam--get-node-heirarchy b) dc-org-roam-hierarchy-insert-separator)))
                                       (string< hierarchy-a hierarchy-b))))))
          (dolist (node sorted-nodes)
            (let* ((id (org-roam-node-id node))
                   (hierarchy (dc/org-roam--get-node-heirarchy node))
                   (arrow-chain (if (> (length hierarchy) 1)
                                    (mapconcat #'identity hierarchy dc-org-roam-hierarchy-insert-separator)
                                  (org-roam-node-title node)))
                   (link (org-link-make-string (concat "id:" id) arrow-chain)))
              (insert (concat dc-org-roam-link-prefix link))
              (insert "\n")
              (run-hook-with-args 'org-roam-post-node-insert-hook
                                  id
                                  arrow-chain))))))
  (deactivate-mark))

;; Add keybindings
(define-key dc-roam-map (kbd "a") 'dc/org-roam-insert-nodes-by-tags)

;;;; Package - org-attach
;;;;; Configuration
(use-package org-attach
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

;; Add keybindings
(define-key dc-org-map (kbd "k") 'org-attach-attach)

;;;;; Function - Attaching and inserting attachment as a link
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

;; Add keybindings
(define-key dc-org-map (kbd "j") 'dc/org-attach-file-and-insert-link)

;;;;; Function - Copying attachments between org-roam nodes
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

;;;;; Function - Moving attachments between org-roam nodes
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

;;;;; Function - Deleting attachments from node
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

;;;; Package - websocket
;;;;; Configuration
(use-package websocket
  :after org-roam
  :ensure t
  )

;;;; Package - org-roam-ui
;;;;; Configuration
(use-package org-roam-ui
  :after org-roam
  :ensure t
  )

;;;; Package - org-transclusion
;;;;; Configuration
(use-package org-transclusion
  :after org
  :ensure t
  )

;;;;; Function - Inserting of transcluded nodes
(defun dc/org-tranclusion-set-link-prefix ()
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

;;;; Package - alert
;;;;; Configuration
(use-package alert
  :ensure t
  :config
  ;; Setup default icon for Android notifications
  (when (eq system-type 'android)
    ;; android.R.drawable icons must be used
    (setq alert-default-icon "ic_popup_reminder"))
  )

;;;;; Function - Supporting Android notifications
(defun dc/alert-android-notifications-notify (info)
  "Send notifications using `android-notifications-notify'.
`android-notifications-notify' is a built-in function in the native Emacs
Android port."
  (let ((title (or (plist-get info :title) "Android Notifications Alert"))
        (body (or (plist-get info :message) ""))
        (urgency (cdr (assq (plist-get info :severity)
                            alert-notifications-priorities)))
        (icon (or (plist-get info :icon) alert-default-icon))
        (replaces-id (gethash (plist-get info :id) alert-notifications-ids)))
    (android-notifications-notify
     :title title
     :body body
     :urgency urgency
     :icon icon
     :replaces-id replaces-id)))

(alert-define-style 'android-notifications :title "Android Notifications"
                    :notifier #'dc/alert-android-notifications-notify)

;;;; Package - org-alert
;;;;; Configuration
(use-package org-alert
  :ensure t
  :after org
  :custom
  ;; Use different backends depending on the platform
  (alert-default-style (if (eq system-type 'android)
                           'android-notifications
                         'notifications))
  :config
  ;; Setup timing
  (setq org-alert-interval 300
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

;;;;; Function - Changing title of notifications
(defvar dc-org-alert-title-type 'custom
  "Control the title type for `org-alert' notifications.
  /home/danijelcamdzic/Projects/dotemacs/ Possible values are:
      - 'custom: The usual workings of org-alert package. Uses `org-alert-notification-title'
                 as the title of notifications sent.
      - 'parent: Uses the immediate parent heading of the TODO as the title of the notification.
                 If the TODO does not have a parent, it uses the file title instead. If the file
                 does not have a title, it uses the filename as the title for notifications.")

(defun dc/org-alert--get-todo-parent ()
  "Get the immediate parent heading of a TODO. If no parents, use file title. If no file title
use filename."
  (if (org-up-heading-safe)
      (org-get-heading t t t t)
    (let ((title (cdr (assoc "TITLE" (org-collect-keywords '("TITLE"))))))
      (if (and title (listp title))
          (car title)
        title))))

(defun org-alert--parse-entry--use-parent-as-title-advice (orig-fun &rest args)
  "Advice for `org-alert--parse-entry' function. It adapts it to accept parameters from the
`dc/org-alert--get-todo-parent' function which retrieves the parent heading or file title/name."
  (let ((head (org-alert--strip-text-properties (org-get-heading t t t t)))
        (parent-or-file-head (dc/org-alert--get-todo-parent)))
    (cl-destructuring-bind (body cutoff) (org-alert--grab-subtree)
      (if (string-match org-alert-time-match-string body)
          (list head parent-or-file-head (match-string 1 body) cutoff)
        nil))))

(defun org-alert--dispatch--use-parent-as-title-advice (orig-fun &rest args)
  "Advice for `org-alert--dispatch' function."
  (let ((entry (org-alert--parse-entry)))
    (when entry
      (cl-destructuring-bind (head parent-or-file-head time cutoff) entry
        (if time
            (when (org-alert--check-time time cutoff)
              (alert (concat time ": " head) :title parent-or-file-head))
          (alert head :title parent-or-file-head))))))

(defun dc/org-alert-update-advices ()
  "Add or remove advice based on the value of `org-alert-title-type'."
  (cond ((eq dc-org-alert-title-type 'parent)
         (advice-add 'org-alert--parse-entry :around #'org-alert--parse-entry--use-parent-as-title-advice)
         (advice-add 'org-alert--dispatch :around #'org-alert--dispatch--use-parent-as-title-advice))
        ((eq dc-org-alert-title-type 'custom)
         (advice-remove 'org-alert--parse-entry #'org-alert--parse-entry--use-parent-as-title-advice)
         (advice-remove 'org-alert--dispatch #'org-alert--dispatch--use-parent-as-title-advice))))

;; Set up 'parent mode
(setq dc-org-alert-title-type 'parent)

;; Update to set up or remove advices based on dc-org-alert-title-type
(dc/org-alert-update-advices)

;;;; Package - org-tempo
;;;;; Configuration
(use-package org-tempo
  :after org
  )

;;;; Package - org-analyzer
;;;;; Configuration
(use-package org-analyzer
  :after org
  :ensure t
  :config
  ;; Set up directory
  (setq org-analyzer-org-directory org-directory)
  )

;;;; Package - org-download
;;;;; Configuration
(use-package org-download
  :ensure t
  :after org
  :config
  ;; Use attachments and not file links
  (setq org-download-method 'attach)
  
  ;; Don't create folders based on heading levels
  (setq-default org-download-heading-lvl nil)
  )

;; Add keybindings
(define-key dc-org-map (kbd "p") 'org-download-clipboard)

;;;;; Function - Prompting for screenshot filename
(defun dc/org-download-clipboard--prompt-for-name-advice (orig-fun &optional basename)
  "Advice to prompt for a basename before calling `org-download-clipboard'."
  (message "Calling advice function")
  (let ((name (if (called-interactively-p 'any)
                  (read-string "Enter image name (without extension): ")
                basename)))
    (funcall orig-fun (if (string-empty-p name) basename (concat name ".png")))))

(advice-add 'org-download-clipboard :around #'dc/org-download-clipboard--prompt-for-name-advice)

;;;; Package - org-ref
;;;;; Configuration
(use-package org-ref
  :ensure t
  :after org
  )

;;;; Package - org-noter
;;;;; Configuration
(use-package org-noter
  :ensure t  
  :after org 
  :config
  ;; Set the location of the notes
  (setq org-noter-notes-search-path '(org-directory))
  )

;;;; Package - org-media-note
;;;;; Configuration
(use-package org-media-note
  :quelpa (org-media-note :fetcher github :repo "yuchen-lea/org-media-note")
  :hook (org-mode .  org-media-note-mode)
  :bind (("H-v" . org-media-note-hydra/body))
  :config
  ;; Set up save method
  (setq org-media-note-screenshot-save-method 'attach)
  (setq org-media-note-screenshot-link-type-when-save-in-attach-dir 'attach)
  )

;;;;; Function - Enabling mpv-android support on Android
;; This should only be done on Android
(when (eq system-type 'android)
  (defun dc/mpv-start--android-advice (orig-fun &rest args)
    "Advice to use a different mpv command on Android. Android uses
Termux package called mpv-android and Emacs should pass appropriate
commands to it.

This is an example of a full command passed down to mpv-android:

am start -a android.intent.action.VIEW -t video/* -d file:///storage/emulated/0/Download/why_i_like_cats.mp4 --ei position 30000 -p is.xyz.mpv
"
    (let* ((media-path (replace-regexp-in-string " " "\\\\ " (car args)))
           (start-time (if (> (length args) 1) (nth 1 args) 0))
           (start-time-ms (when (stringp start-time)
                            (* (string-to-number (replace-regexp-in-string "\\`--start=\\+" "" start-time)) 1000)))
           (is-remote (or (string-prefix-p "http://" media-path)
                          (string-prefix-p "https://" media-path)))
           (mpv-command (format "am start -a android.intent.action.VIEW -t video/* %s%s"
                                (if is-remote "-d " "-d file:///")
                                media-path)))
      (when start-time-ms
        (setq mpv-command (format "%s --ei position %d" mpv-command start-time-ms)))
      (setq mpv-command (format "%s -p is.xyz.mpv" mpv-command))
      (start-process "mpv-android" nil "sh" "-c" mpv-command)))

  ;; Add advice to mpv-start so it open the correct player each time
  (advice-add 'mpv-start :around #'dc/mpv-start--android-advice))

;;;;; Function - Prepending timestamp to screenshot filename
(defun dc/org-media-note--format-picture-file-name--prepend-timestamp-advice (orig-func &rest args)
  "Advice to prepend the current timestamp to the filename created by `org-media-note--format-picture-file-name'."
  (let ((original-filename (apply orig-func args))
        (timestamp (format-time-string "%Y-%m-%d_%H-%M-%S")))
    (concat timestamp "_" original-filename)))

(advice-add 'org-media-note--format-picture-file-name :around #'dc/org-media-note--format-picture-file-name--prepend-timestamp-advice)

;;;;; Function - Removing invalid characters from filename
(defun dc/remove-invalid-characters-from-filename (filename)
  "Remove invalid characters from filename in order for it to sync to Android using syncthing."
  (replace-regexp-in-string "[/*\":<>?|]" "" filename))

(defun dc/org-media-note--format-picture-file-name--remove-invalid-characters-from-filename-advice (orig-func &rest args)
  "Advice to remove invalid characters from filename in `org-media-note--format-picture-file-name'."
  (dc/remove-invalid-characters-from-filename (apply orig-func args)))

(advice-add 'org-media-note--format-picture-file-name :around #'dc/org-media-note--format-picture-file-name--remove-invalid-characters-from-filename-advice)

;;; Browsing & bookmarks
;;;; Package - eww
;;;;; Configuration
(use-package eww
  :config
  ;; Set default eww-bookmarks directory
  (setq eww-bookmarks-directory (concat dc-documents-directory "Emacs/"))
  )

;;;; Package - bookmark
;;;;; Configuration
(use-package bookmark
  :config
  ;; Set default bookmark file
  (setq bookmark-default-file (concat dc-documents-directory "Emacs/bookmarks"))
  )

;;;; Package - bookmark+
;;;;; Configuration
(use-package bookmark+
  :quelpa (bookmark+ :fetcher github :repo "emacsmirror/bookmark-plus")
  :config
  ;; Set default .emacs-bmk-bmenu-state.el file path
  (setq bmkp-bmenu-state-file (expand-file-name ".emacs-bmk-bmenu-state.el" user-emacs-directory))
  
  ;; Set default .emacs-bmk-bmenu-commands.el file path
  (setq bmkp-bmenu-commands-file (expand-file-name ".emacs-bmk-bmenu-commands.el" user-emacs-directory))
  )

;; Add keybindings
(define-key dc-bookmark-map (kbd "l") 'list-bookmarks)
(define-key dc-bookmark-map (kbd "s") 'bookmark-set)
(define-key dc-bookmark-map (kbd "d") 'bmkp-bmenu-delete-marked)
(define-key dc-bookmark-map (kbd "n") 'bmkp-bmenu-filter-bookmark-name-incrementally)
(define-key dc-bookmark-map (kbd "t") 'bmkp-bmenu-filter-tags-incrementally)

;;;;; Function - Integrating bookmarks between Android and Linux
(defun dc/bookmark-jump--modify-bookmark-path-advice (orig-fun &rest args)
  "Modify the bookmark filename and directory based on system type before opening."
  (let* ((bookmark (car args))
         (bookmark-data (bookmark-get-bookmark bookmark))
         (filename (alist-get 'filename bookmark-data))
         (dired-directory (alist-get 'dired-directory bookmark-data)))
    ;; Modify filename for file bookmarks
    (when filename
      (if (eq system-type 'android)
          (progn
            (when (string-match-p (regexp-quote dc-gnu-linux-home) filename)
              (setq filename (replace-regexp-in-string (regexp-quote dc-gnu-linux-home) dc-android-home filename)))
            (when (string-match-p (regexp-quote dc-gnu-linux-home-extended) filename)
              (setq filename (replace-regexp-in-string (regexp-quote dc-gnu-linux-home-extended) dc-android-home filename))))
        (when (string-match-p (regexp-quote dc-android-home) filename)
          (setq filename (replace-regexp-in-string (regexp-quote dc-android-home) dc-gnu-linux-home filename))))
      (setf (alist-get 'filename bookmark-data) filename))
    ;; Modify dired-directory for directory bookmarks
    (when dired-directory
      (if (eq system-type 'android)
          (progn
            (when (string-match-p (regexp-quote dc-gnu-linux-home) dired-directory)
              (setq dired-directory (replace-regexp-in-string (regexp-quote dc-gnu-linux-home) dc-android-home dired-directory)))
            (when (string-match-p (regexp-quote dc-gnu-linux-home-extended) dired-directory)
              (setq dired-directory (replace-regexp-in-string (regexp-quote dc-gnu-linux-home-extended) dc-android-home dired-directory))))
        (when (string-match-p (regexp-quote dc-android-home) dired-directory)
          (setq dired-directory (replace-regexp-in-string (regexp-quote dc-android-home) dc-gnu-linux-home dired-directory))))
      (setf (alist-get 'dired-directory bookmark-data) dired-directory))

    (apply orig-fun args)))

;; Add advice so bookmarks will be properly opened
(advice-add 'bookmark-jump :around #'dc/bookmark-jump--modify-bookmark-path-advice)

;;; Date & time
;;;; Function - Overriding current time
(defvar dc-adjusted-time nil
  "Adjusted time. This time will replace current time.")

(defvar dc-time-override-lock nil
  "Lock to prevent concurrent access to the time override.")

(defun dc/time-adjust-time (time)
  "Temporarily adjust `current-time' to the given TIME."
  (setq dc-adjusted-time (append (org-read-date nil t time) '(0 0))))

(defun dc/time-override-current-time ()
  "Override for `current-time' using `dc/time-adjust-time'."
  (or dc-adjusted-time (current-time)))

;;;; Package - time-stamp
;;;;; Configuration
(use-package time-stamp
  :config
  ;; Set up time-stamp format
  (setq time-stamp-format "%Y-%m-%d %H:%M"
        time-stamp-start "# Edited: "
        time-stamp-end "$")

  ;; Add hook to save time-stamp string on every file save
  (add-hook 'before-save-hook 'time-stamp)
  )

;;;; Package - calendar
;;;;; Configuration
(use-package calendar
  :config
  ;; Set calendar to start on Monday
  (setq calendar-week-start-day 1)
  )

;;; Encryption & authentication
;;;; Package - epa
;;;;; Configuration
(use-package epa
  :ensure t
  :config
  ;; Set the environment variable and configure EPA only if running on Android
  (when (eq system-type 'android)
    ;; Set the environment variable to use GPG on Termux
    (setenv "GNUPGHOME" "/data/data/com.termux/files/home/.gnupg"))

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

;;;; Package - auth-source
;;;;; Configuration
(use-package auth-source
  :ensure t
  :config
  ;; Set auth-sources files
  (setq auth-sources
        (cl-loop for file in (directory-files (concat dc-documents-directory "Emacs/.auth-sources/") t "\\.gpg$")
                 collect `(:source ,file)))

  ;; Enable authinfo-mode for auth-source files
  (add-to-list 'auto-mode-alist '("\\.authinfo.*\\.gpg\\'" . authinfo-mode))

  ;; Clear cached passwords after buffers are switched
  (add-hook 'buffer-list-update-hook 'auth-source-forget-all-cached)
  )

;;;;; Function - Getting auth-source secrets
(defun dc/auth-display-all (search-string)
  "Select an authentication entry based on SEARCH-STRING from `auth-sources', and briefly display its value.
Function decrypts all auth-source files and lets you search through them to find the SEARCH-STRING."
  (interactive "sEnter authentication type: ")
  (let* ((candidates (mapcar
                      (lambda (auth)
                        (cons (format "User '%s' on %s"
                                      (propertize (plist-get auth :user) 'face 'font-lock-keyword-face)
                                      (propertize (plist-get auth :host) 'face 'font-lock-string-face))
                              auth))
                      (seq-filter (lambda (auth) (string-prefix-p search-string (plist-get auth :host)))
                                  (auth-source-search :max 10000))))
         (selected-auth (cdr (assoc (completing-read (format "Pick an entry for '%s'> " search-string) candidates) candidates)))
         (password (funcall (plist-get selected-auth :secret))))
    ;; Temporarily disable logging in *Messages* buffer
    (let ((message-log-max nil))
      (message "Your authentication secret for '%s' is: %s"
               (propertize (plist-get selected-auth :host) 'face 'font-lock-keyword-face)
               (propertize password 'face 'font-lock-string-face)))))

(defun dc/auth-search (search-string)
  "Select a .gpg file from `auth-sources`, then select an authentication entry from that file, and briefly display its value.
Function decrypts only the chosen auth-source file and lets you search through it to find the SEARCH-STRING."
  (interactive "sEnter authentication type: ")
  (let* ((gpg-files (mapcar (lambda (source) (plist-get source :source)) auth-sources))
         (selected-file (completing-read "Select an auth-source file: " gpg-files))
         (candidates (mapcar
                      (lambda (auth)
                        (cons (format "User '%s' on %s"
                                      (propertize (plist-get auth :user) 'face 'font-lock-keyword-face)
                                      (propertize (plist-get auth :host) 'face 'font-lock-string-face))
                              auth))
                      (seq-filter (lambda (auth) (string-prefix-p search-string (plist-get auth :host)))
                                  (auth-source-search :max 10000 :source selected-file)))))
    (let* ((selected-entry (completing-read "Pick an entry> " candidates))
           (auth (cdr (assoc selected-entry candidates)))
           (password (funcall (plist-get auth :secret))))
      ;; Temporarily disable logging in *Messages* buffer
      (let ((message-log-max nil))
        (message "Your authentication secret for '%s' is: %s"
                 (propertize (plist-get auth :host) 'face 'font-lock-keyword-face)
                 (propertize password 'face 'font-lock-string-face))))))

;;;;; Function - Getting passwords
(defun dc/password-display-all ()
  "Uses `dc/auth-get-secret-all' function to retrieve the passwords from all files in `auth-sources' by searching for PASS string."
  (interactive)
  (dc/auth-display-all "PASS:"))

(defun dc/password-search ()
  "Uses `dc/auth-get-secret-one' function to retrieve the passwords from the chosen `.gpg' files in `auth-sources'
by searching for PASS string."
  (interactive)
  (dc/auth-search "PASS:"))

;;;;; Function - Getting TOTP
(require 'bindat)
(require 'gnutls)
(require 'hexl)

(defun totp--base32-char-to-n (char)
  "Return 5 bit integer value matching base32 CHAR."
  (cond ((<= ?A char ?Z) (- char ?A))
        ((<= ?a char ?z) (- char ?a))
        ((<= ?2 char ?7) (+ (- char ?2) 26))
        (t (error "Invalid number range"))))

(defun totp--base32-to-number (string)
  "Base32-decode STRING and return the result as number.
Handles interleaved whitespaces and missing padding charachters
gracefuly (The number of padding chars can be deduced from input
length)."
  (let* ((s (replace-regexp-in-string "\\([[:space:]]\\|=*$\\)" "" string))
         (ntrail (mod (* 5  (length s)) 8)))
    (ash (seq-reduce (lambda (acc char)
                       (+ (ash acc 5) (totp--base32-char-to-n char)))
                     s 0) (- ntrail))))

(defun totp--hex-decode-string (string)
  "Hex-decode STRING and return the result as a unibyte string."
  (apply #'unibyte-string
         (seq-map (lambda (s) (hexl-htoi (aref s 0) (aref s 1)))
                  (seq-partition string 2))))

(defun totp (string &optional time digits)
  "Return a TOTP token using the secret STRING and current time.
TIME is used as counter value instead of current time, if non-nil.
DIGITS is tre  number of pin digits and defaults to 6."
  (let* ((hex-string (if (string-match-p "^[0-9a-fA-F]\\{2\\}+$" string)
                         string		;already in hex format
                       (format "%X" (totp--base32-to-number string))))
         (key-bytes (totp--hex-decode-string (upcase hex-string)))
         (counter (truncate (/ (or time (time-to-seconds)) 30)))
         (digits (or digits 6))
         (format-string (format "%%0%dd" digits))
         ;; we have to manually split the 64 bit number (u64 not supported in Emacs 27.2)
         (counter-bytes (bindat-pack  '((:high u32) (:low u32))
                                      `((:high . ,(ash counter -32)) (:low . ,(logand counter #xffffffff)))))
         (mac (gnutls-hash-mac 'SHA1 key-bytes counter-bytes))
         (offset (logand (bindat-get-field (bindat-unpack '((:offset u8)) mac 19) :offset) #xf)))
    (format format-string
            (mod
             (logand (bindat-get-field (bindat-unpack '((:totp-pin u32)) mac  offset) :totp-pin)
                     #x7fffffff)
             (expt 10 digits)))))

(defun dc/totp-display-all (auth)
  "Select a TOTP AUTH from `auth-sources', display its TOTP, and show remaining valid time.
Function decrypts all auth-source files and lets you search through them to find the TOTP."
  (interactive
   (list
    (let ((candidates (mapcar
                       (lambda (auth)
                         (cons (format "User '%s' on %s"
                                       (propertize (plist-get auth :user) 'face 'font-lock-keyword-face)
                                       (propertize (plist-get auth :host) 'face 'font-lock-string-face))
                               auth))
                       (seq-filter (lambda (auth) (string-prefix-p "TOTP:" (plist-get auth :host)))
                                   (auth-source-search :max 10000)))))
      (cdr (assoc (completing-read "Pick a TOTP> " candidates) candidates)))))
  (let* ((current-time (time-to-seconds))
         (time-step 30)
         (time-remaining (- time-step (mod current-time time-step)))
         (code (totp (funcall (plist-get auth :secret)))))
    ;; Temporarily disable logging in *Messages* buffer
    (let ((message-log-max nil))
      (message "Your TOTP for '%s' is: %s (valid for %d more seconds)"
               (propertize (plist-get auth :host) 'face 'font-lock-keyword-face)
               (propertize code 'face 'font-lock-string-face)
               time-remaining))
    code))

(defun dc/totp-search ()
  "Select a TOTP AUTH from `auth-sources', display its TOTP, and show remaining valid time.
Function decrypts only the chosen auth-source file and lets you search through it to find the TOTP."
  (interactive)
  (let* ((gpg-files (mapcar (lambda (source) (plist-get source :source)) auth-sources))
         (selected-file (completing-read "Select an auth-source file: " gpg-files))
         (candidates (mapcar
                      (lambda (auth)
                        (cons (format "User '%s' on %s"
                                      (propertize (plist-get auth :user) 'face 'font-lock-keyword-face)
                                      (propertize (plist-get auth :host) 'face 'font-lock-string-face))
                              auth))
                      (seq-filter (lambda (auth) (string-prefix-p "TOTP:" (plist-get auth :host)))
                                  (auth-source-search :max 10000 :source selected-file)))))
    (let* ((selected-entry (completing-read "Pick a TOTP> " candidates))
           (auth (cdr (assoc selected-entry candidates)))
           (current-time (time-to-seconds))
           (time-step 30)
           (time-remaining (- time-step (mod current-time time-step)))
           (code (totp (funcall (plist-get auth :secret)))))
      ;; Temporarily disable logging in *Messages* buffer
      (let ((message-log-max nil))
        (message "Your TOTP for '%s' is: %s (valid for %d more seconds)"
                 (propertize (plist-get auth :host) 'face 'font-lock-keyword-face)
                 (propertize code 'face 'font-lock-string-face)
                 time-remaining))
      code)))

;;; init.el ends here
