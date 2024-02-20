;;; init.el -- Personal configuration file for Emacs

;;; Code:

;;; User
;;;; User credentials
;; User name and email
(setq user-full-name "Danijel Camdzic")
(setq user-mail-address "danijelcamdzic@tuta.com")

;;;; User directories
;; Define the home directories variables
(defvar dc-android-home "/storage/emulated/0/")
(defvar dc-gnu-linux-home "~/")
(defvar dc-gnu-linux-home-extended "/home/danijelcamdzic/")

;; Set the home directory based on system type
(setq dc-home-directory
      (cond
       ((eq system-type 'gnu/linux) dc-gnu-linux-home-extended)
       ((eq system-type 'android) dc-android-home)
       (t dc-gnu-linux-home)))

;; Set the user folders
(setq dc-books-directory (concat dc-home-directory "Books/"))
(setq dc-documents-directory (concat dc-home-directory "Documents/"))
(setq dc-download-directory (concat dc-home-directory "Download/")) 
(setq dc-music-directory (concat dc-home-directory "Music/"))         
(setq dc-notes-directory (concat dc-home-directory "Notes/"))
(setq dc-pictures-directory (concat dc-home-directory "Pictures/"))   
(setq dc-projects-directory (concat dc-home-directory "Projects/"))    
(setq dc-recordings-directory (concat dc-home-directory "Recordings/"))
(setq dc-videos-directory (concat dc-home-directory "Videos/"))

;;;;; Functions - Open user directory

(defun dc/open-user-directory ()
  "Open a user directory in dired."
  (interactive)
  (let* ((directories '(("Books" . dc-books-directory)
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
    (message "Opening directory: %s" directory) ; Debug message
    (dired directory)))

;;; Package managers
;;;; Package
;;;;; Configuration
(require 'package)

;; Temporarily disable signature checks
(setq package-check-signature nil)

;; Initialize packages
(package-initialize)

;;;; Melpa
;;;;; Configuration
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;;;; Use-package
;;;;; Configuration
;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;;; Quelpa
;;;;; Configuration
(use-package quelpa
  :ensure t
  :init
  ;; Disable self upgrades to reduce startup time
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-update-melpa-p nil)
  )

;;;; Quelpa-use-package
;;;;; Configuration
(use-package quelpa-use-package
  :ensure t
  :after (quelpa)
  :config
  ;; Activate quelpa-use-package
  (quelpa-use-package-activate-advice)
  )

;;; Editor
;;;; Theme
;; Install gruvbox-theme
(use-package gruvbox-theme
  :ensure t
  )

;; Set gruvbox-theme as the system theme
(load-theme 'gruvbox-dark-hard t)

;; Remove fringes
(set-fringe-mode 0)

;;;; Startup
;; Bind command call in Android to hardware keys volume up and volume down
(when (eq system-type 'android)
  ;; Volume up calls to execute the command
  (global-set-key (kbd "<volume-up>") 'execute-extended-command)
  ;; Volume down is bound by default to org-ctrl-c-ctrl-c
  (global-set-key (kbd "<volume-down>") 'org-ctrl-c-ctrl-c)

  ;; Volume down is also programmable
  (defun dc/bind-to-android-volume-down ()
    "Bind a command to the <volume-down> key on Android."
    (interactive)
    (let ((command (intern (completing-read "Command: " obarray 'commandp t))))
      (global-set-key (kbd "<volume-down>") command)))
  )

;; Remove startup screen
(setq inhibit-startup-screen t)

;;;; Files
;; Disable backup and lock files
(setq create-lockfiles nil
      auto-save-default nil
      make-backup-files nil)

;;;; Buffers
;; Change buffer behavior on Android
(when (eq system-type 'android)
  ;; Buffer display settings
  (setq display-buffer-alist
        '((".*" (display-buffer-same-window) (inhibit-same-window . nil))))
  
  ;; Touchscreen keyboard spawn
  (setq touch-screen-display-keyboard t))

;; Open org-agenda day view and org-roam daily note on startup
;; (unless called with a file argument)
(add-hook 'emacs-startup-hook
          (lambda ()
            (unless (> (length command-line-args) 1)
              (org-roam-dailies-goto-today)
              (dc/org-agenda-day-view)
              )))

;;;;; Functions - Killing all buffers
(defun dc/kill-background-buffers ()
  "Kill all buffers that are not currently visible in any window, except the *Messages* buffer."
  (interactive)
  (let ((visible-buffers (mapcar 'window-buffer (window-list))))
    (dolist (buffer (buffer-list))
      (unless (or (member buffer visible-buffers)
                  (string= (buffer-name buffer) "*Messages*"))
        (kill-buffer buffer)))))

;;;;; IBuffer
;;;;;; Configuration
(use-package ibuffer-sidebar
  :ensure t
  :config
  )

;;;;;; Functions - IBuffer-sidebar Toggle
(defun dc/ibuffer-sidebar-toggle ()
  "Toggle `ibuffer-sidebar'."
  (interactive)
  (ibuffer-sidebar-toggle-sidebar))

;;;;; Dired-sidebar
;;;;;; Configuration
(use-package dired-sidebar
  :ensure t
  :config
  ;; Make the window not fixed
  (setq dired-sidebar-window-fixed nil)
  )

;;;;;; Functions - Dired-sidebar toggle
(defun dc/dired-sidebar-toggle ()
  "Toggle `dired-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar))

;;;; Text editing
;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              indent-line-function 'insert-tab)

;; Text faces
(custom-set-faces
 '(bold ((t (:foreground "#008000" :weight bold))))
 '(italic ((t (:foreground "#B0A030" :slant italic))))
 '(strike-through ((t (:foreground "#8B0000" :strike-through t)))))

;;;;; Programming
;;;;;; C/CPP
(defun dc/c-cpp-mode-setup ()
  "Set basic c and cpp offset."
  (setq c-basic-offset 4))

;; Set hook to set indentation when in c/cpp file
(add-hook 'c-mode-common-hook 'dc/c-cpp-mode-setup)

;; Disable line numbers
(global-display-line-numbers-mode 0)

;;;; Version control
;;;;; Magit
;;;;;; Configuration
(use-package magit
  :ensure t
  )

;;;; Visual Modes
;; Enable outline-minor-mode as soon as .el file is opened
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

;;;;; Outline-minor-mode
;;;;;; Configuration
(use-package outline-minor-faces
  :ensure t
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    #'outline-minor-faces-mode)
  )

;;;;; Pretty-hydra
;;;;;; Configuration
(use-package pretty-hydra
  :ensure t
  )

;;;;; Which-key
;;;;;; Configuration
(use-package which-key
  :ensure t
  :config
  ;; Setup which-key-mode
  (which-key-mode)
  )

;;; Document viewing
;;;; Doc-view
;; Set higher resolution for viewing documents
(setq doc-view-resolution 400)

;;; Completion
;;;; Company
;;;;; Configuration
(use-package company
  :ensure t
  :config
  ;; Enable company mode
  (company-mode 1)

  ;; Add hook to enable company mode globally
  (add-hook 'after-init-hook 'global-company-mode)
  )

;;;; Orderless
;;;;; Configuration
(use-package orderless
  :ensure t
  )

;;;; Vertico
;;;;; Configuration
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

;;; GUI
;;;; Functions: GUI display show/hide
(defun dc/gui-hide-bars ()
  "Disable scroll bar, menu bar, and tool bar."
  (interactive)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(defun dc/gui-show-bars ()
  "Enable scroll bar, menu bar, and tool bar."
  (interactive)
  (scroll-bar-mode 1)
  (menu-bar-mode 1)
  (tool-bar-mode 1))

;;; Org-mode
;;;; Org
;;;;; Configuration
(use-package org
  :ensure t
  :config
  ;; Set org directory
  (setq org-directory dc-notes-directory)

  ;; Set org-mode preferences for buffer display
  (setq org-startup-indented t)
  (setq org-startup-folded t)
  (add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
  (add-hook 'org-mode-hook #'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'org-hide-block-all)
  (add-hook 'org-mode-hook 'org-hide-drawer-all)

  ;; Add new line before heading
  (setf org-blank-before-new-entry '((heading . t)))

  ;; Display inline images on startup
  (setq org-startup-with-inline-images t)

  ;; Set the width of the inline images to be the actual size
  (setq org-image-actual-width t)

  ;; Set custom faces for scheduled headings
  (custom-set-faces
   '(org-scheduled ((t (:foreground "#555555"))))
   '(org-scheduled-today ((t (:foreground "grey")))))

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

  ;; Python configuration
  (setq org-babel-python-command "python3")
  (setq python-indent-offset 4)
  (setq org-edit-src-content-indentation 0)

  ;; Don't ask for confirmation during org babel execution
  (setq org-confirm-babel-evaluate nil)

  ;; Set the org-todo-keywords and their states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i!)" "|" "DONE(d!)" "SKIP(s!)" "FAIL(f!)")))
  )

;;;;; Functions - Link insertion
(defun dc/org-insert-link-set-default-directory (dir)
  "Set the default directory for 'org-insert-link' to start from."
  (interactive "DSet default directory: ")
  (setq default-directory dir))

;;;;; Functions - Datetime insertion
(defun dc/org-insert-current-date-time ()
  "Insert the current date and time along with the three-letter weekday name in
the format YYYY-MM-DD Day H:M."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a %H:%M")))

;;;;; Functions - Clocking
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

;;;;; Functions - Scheduling
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

;;;;; Functions - State change
(defun dc/org-todo-change-state ()
  "Change state of a current heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo)
    (org-todo)))

(defun dc/org-todo-change-state-and-reschedule ()
  "Change state of a current heading."
  (interactive)
  (dc/org-todo-change-state)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "TODO")
    (org-todo "TODO"))
  (run-with-timer 0.1 nil 'dc/org-add-schedule))

(defun dc/org-todo-change-state-with-date ()
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

(defun dc/org-todo-change-state-with-date-and-reschedule ()
  "Change state of the current heading and log with a chosen date."
  (interactive)
  (dc/org-todo-change-state-with-date)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "TODO")
    (org-todo "TODO"))
  (run-with-timer 0.1 nil 'dc/org-add-schedule))

(defun dc/org-todo-skip-overdue-tasks ()
  "Mark tasks scheduled for yesterday or earlier as SKIP and
log them as changed on their scheduled date, but only if their
current state is TODO."
  (interactive)
  (dolist (file (directory-files org-directory t "\\.org$"))
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

;;;;; Functions - Notes
(defun dc/org-add-note ()
  "Add a note to an org heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-add-note)
    (org-add-note)))

;;;;; Functions - Logbook calendar display
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

;;;; Org-agenda
;;;;; Configuration
(use-package org-agenda
  :after org
  :config
  ;; Set org-agenda-files to all files in org-directory ending in _agenda.org
  ;; This is done so org-agenda-files does not search through many files
  (setq org-agenda-files
        (directory-files org-directory t "_agenda\\.org$"))
  
  ;; Customize org-agenda view
  (setq org-agenda-prefix-format  '((agenda . "  %t ")
                                    (todo . "%t ")
                                    (tags . "")
                                    (search . "%i")))
  (setq org-agenda-sorting-strategy '((agenda time-up priority-up category-keep)
                                      (todo priority-up time-up category-keep)
                                      (tags time-up priority-up category-keep)
                                      (search time-up priority-up category-keep)))
  (setq org-agenda-scheduled-leaders '("" ""))
  (setq org-agenda-span 7)
  (setq org-agenda-show-future-repeats 'next)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-time-grid
        '((daily weekly today require-timed)
          (0000 0200 0400 0600 0800 1000 1200 1400 1600 1800 2000 2200 2359)
          "......" "----------------"))
  )

;;;;; Functions - Agenda files
(defun dc/org-agenda-update-agenda-files ()
  "Update org-agenda-files with all files in org-directory ending in _agenda.org."
  (interactive)
  (setq org-agenda-files
        (directory-files org-directory t "_agenda\\.org$")))

;;;;; Functions - Agenda views
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

;;;; Org-super-agenda
;;;;; Configuration
(use-package org-super-agenda
  :after org-agenda
  :ensure t
  :config
  ;; Enable org-super-agenda mode
  (org-super-agenda-mode)
  )

;;;;; Functions - Auto parents
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

;;;;; Functions - TODO view
(defun dc/org-agenda-todo-view ()
  "Open Org Agenda in the todos view mode with super agenda. Use file title as groups"
  (interactive)
  (let ((org-super-agenda-groups '((:auto-parent t)))
        (org-agenda-sorting-strategy '((todo priority-down category-keep))))
    (org-agenda nil "t")
    (setq org-super-agenda-groups '())))

;;;; Org-roam
;;;;; Configuration
(use-package org-roam
  :after org
  :ensure t
  :config
  ;; Set directories
  (setq org-roam-directory org-directory)
  (setq org-roam-dailies-directory (concat org-directory "dailies/"))
  
  ;; Exclude gpg encrypted files from being processed by org-roam
  (setq org-roam-file-exclude-regexp "\\(\\.gpg\\)$")

  ;; Setup org-roam
  (org-roam-setup)
  )

;;;;; Functions - Node hierarchy
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

;;;;; Functions - Node display formatting
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
  "Return a string which indicates whether a node is a `@note' or a `@daily'."
  (let ((file-path (org-roam-node-file node)))
    (if (string-prefix-p (file-name-as-directory org-roam-dailies-directory) (file-name-directory file-path))
        " @daily"
      " @note")))

;; Set the hierarchy display formatting
(setq org-roam-node-display-template
      (concat "${hierarchy}" "${node-type}" (propertize "${colon-tags}" 'face 'org-tag)))

;;;;; Functions - Inserting nodes by tags
(defvar dc-org-roam-hierarchy-insert-separator
  (propertize "->" 'face '(shadow))
  "Separator for org-roam hierarchy insertion.")

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
              (insert link)
              (insert "\n")
              (run-hook-with-args 'org-roam-post-node-insert-hook
                                  id
                                  arrow-chain))))))
  (deactivate-mark))

;;;; Alert
;;;;; Configuration
(use-package alert
  :ensure t
  :config
  ;; Setup default icon for Android notifications
  (when (eq system-type 'android)
    ;; android.R.drawable icons must be used
    (setq alert-default-icon "ic_popup_reminder"))
  )

;;;;; Functions - Android notifications
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

;;;; Org-alert
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

;;;;; Functions - Notification titles
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

;;;; Org-tempo
;;;;; Configuration
(use-package org-tempo
  :after org
  )

;;;; Org-analyzer
;;;;; Configuration
(use-package org-analyzer
  :after org
  :ensure t
  :config
  ;; Set up directory
  (setq org-analyzer-org-directory org-directory)
  )

;;;; Websocket
;;;;; Configuration
(use-package websocket
  :after org-roam
  :ensure t
  )

;;;; Org-roam-ui
;;;;; Configuration
(use-package org-roam-ui
  :after org-roam
  :ensure t
  )

;;;; Org-transclusion
;;;;; Configuration
(use-package org-transclusion
  :after org
  :ensure t
  )

;;;;; Functions - Transclusion insertion
(defun dc/org-transclusion-insert-node ()
  "Insert a transcluded link to an org-roam node."
  (interactive)
  (let ((node (org-roam-node-read)))
    (when node
      (let ((link (format "#+transclude: [[id:%s][%s]]"
                          (org-roam-node-id node)
                          (org-roam-node-title node))))
        (insert link)))))

;;;; Org-attach
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

;;;;; Functions - Attach and insert attachment at once
(defun dc/org-attach-file-and-insert-link ()
  "Attach a file to the current Org entry and insert a link to it.
The attached file is copied to the attachment directory and a link is inserted at point."
  (interactive)
  (let ((file (read-file-name "Select file to attach: " default-directory)))
    (org-attach-attach file nil 'cp)
    (insert (format "[[attachment:%s]]" (file-name-nondirectory file)))))

;;;; Org-download
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

;;;;; Functions - Screenshot filename
(defun dc/org-download-clipboard--prompt-for-name-advice (orig-fun &optional basename)
  "Advice to prompt for a basename before calling `org-download-clipboard'."
  (message "Calling advice function")
  (let ((name (if (called-interactively-p 'any)
                  (read-string "Enter image name (without extension): ")
                basename)))
    (funcall orig-fun (if (string-empty-p name) basename (concat name ".png")))))

(advice-add 'org-download-clipboard :around #'dc/org-download-clipboard--prompt-for-name-advice)

;;;; Org-ref
;;;;; Configuration
(use-package org-ref
  :ensure t
  :after org
  )

;;;; Org-noter
;;;;; Configuration
(use-package org-noter
  :ensure t  
  :after org 
  :config
  ;; Set the location of the notes
  (setq org-noter-notes-search-path '(org-directory))
  )

;;;; Org-media-note
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

;;;;; Functions - Filename
(defun dc/org-media-note--format-picture-file-name--prepend-timestamp-advice (orig-func &rest args)
  "Advice to prepend the current timestamp to the filename created by `org-media-note--format-picture-file-name'."
  (let ((original-filename (apply orig-func args))
        (timestamp (format-time-string "%Y-%m-%d_%H-%M-%S")))
    (concat timestamp "_" original-filename)))

(advice-add 'org-media-note--format-picture-file-name :around #'dc/org-media-note--format-picture-file-name--prepend-timestamp-advice)

;;;;; Functions - Invalid characters
(defun dc/remove-invalid-characters-from-filename (filename)
  "Remove invalid characters from filename in order for it to sync to Android using syncthing."
  (replace-regexp-in-string "[/*\":<>?|]" "" filename))

(defun dc/org-media-note--format-picture-file-name--remove-invalid-characters-from-filename-advice (orig-func &rest args)
  "Advice to remove invalid characters from filename in `org-media-note--format-picture-file-name'."
  (dc/remove-invalid-characters-from-filename (apply orig-func args)))

(advice-add 'org-media-note--format-picture-file-name :around #'dc/org-media-note--format-picture-file-name--remove-invalid-characters-from-filename-advice)

;;; Bookmarks
;;;; Eww
;;;;; Configuration
(use-package eww
  :config
  ;; Set default eww-bookmarks directory
  (setq eww-bookmarks-directory (concat dc-documents-directory "Bookmarks/"))
  )

;;;; Bookmark
;;;;; Configuration
(use-package bookmark
  :config
  ;; Set default bookmark file
  (setq bookmark-default-file (concat dc-documents-directory "Bookmarks/bookmarks"))
  )

;;;; Bookmark+
;;;;; Configuration
(use-package bookmark+
  :quelpa (bookmark+ :fetcher github :repo "emacsmirror/bookmark-plus")
  :config
  ;; Set default .emacs-bmk-bmenu-state.el file path
  (setq bmkp-bmenu-state-file (expand-file-name ".emacs-bmk-bmenu-state.el" user-emacs-directory))
  
  ;; Set default .emacs-bmk-bmenu-commands.el file path
  (setq bmkp-bmenu-commands-file (expand-file-name ".emacs-bmk-bmenu-commands.el" user-emacs-directory))
  )

;;;;; Functions - URL bookmarks
(defun dc/bookmark-set-url (bookmark-name url)
  "Add a new URL bookmark."
  (interactive "sBookmark name: \nsURL: ")
  (let ((bookmark (list bookmark-name
                        (cons 'filename url)
                        (cons 'handler 'eww-bookmark-jump-handler)
                        (cons 'location url))))
    (bookmark-store bookmark-name bookmark nil)
    (bookmark-save)))

;;;;; Functions - Bookmark paths on different platforms
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

;;; Datetime
;;;; Functions - Time adjustment
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

;;;; Time-stamp
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

;;;; Calendar
;;;;; Configuration
(use-package calendar
  :config
  ;; Set calendar to start on Monday
  (setq calendar-week-start-day 1)
  )

;;; Encryption
;;;; Epa
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

;;; Authentication
;;;; Auth-source
;;;;; Configuration
(use-package auth-source
  :ensure t
  :config
  ;; Set auth-sources files
  (setq auth-sources
        (cl-loop for file in (directory-files (concat dc-documents-directory ".auth-sources/") t "\\.gpg$")
                 collect `(:source ,file)))

  ;; Enable authinfo-mode for auth-source files
  (add-to-list 'auto-mode-alist '("\\.authinfo.*\\.gpg\\'" . authinfo-mode))

  ;; Clear cached passwords after buffers are switched
  (add-hook 'buffer-list-update-hook 'auth-source-forget-all-cached)
  )

;;;;; Functions - TOTP
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

(defun dc/totp-display (auth)
  "Select a TOTP AUTH from `auth-sources', display its TOTP, and show remaining valid time."
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
    (let ((message-log-max nil))
      (message "Your TOTP for '%s' is: %s (valid for %d more seconds)"
               (propertize (plist-get auth :host) 'face 'font-lock-keyword-face)
               (propertize code 'face 'font-lock-string-face)
               time-remaining))
    code))

;;;;; Functions - Passwords
(defun dc/password-display (auth)
  "Select a password entry (PASS) from `auth-sources', and briefly display its password."
  (interactive
   (list
    (let ((candidates (mapcar
                       (lambda (auth)
                         (cons (format "User '%s' on %s"
                                       (propertize (plist-get auth :user) 'face 'font-lock-keyword-face)
                                       (propertize (plist-get auth :host) 'face 'font-lock-string-face))
                               auth))
                       (seq-filter (lambda (auth) (string-prefix-p "PASS:" (plist-get auth :host)))
                                   (auth-source-search :max 10000)))))
      (cdr (assoc (completing-read "Pick a PASS entry> " candidates) candidates)))))
  (let ((password (funcall (plist-get auth :secret))))
    ;; Temporarily disable logging in *Messages* buffer
    (let ((message-log-max nil))
      (message "Your password for '%s' is: %s"
               (propertize (plist-get auth :host) 'face 'font-lock-keyword-face)
               (propertize password 'face 'font-lock-string-face)))))

;;; init.el ends here
