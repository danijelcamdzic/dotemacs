;;; init.el -- Personal configuration file for Emacs

;;; Code:

;;; User
;;;; User Credentials
;; User name and email
(setq user-full-name "Danijel Camdzic")
(setq user-mail-address "danijelcamdzic@tuta.com")

;;;; User Directories
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

;;; Package Managers
;;;; Melpa
;;;;; Configuration
;; Add melpa package archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;;;; Package
;;;;; Configuration
;; Temporarily disable signature checks
(setq package-check-signature nil)

;; Initialize packages
(package-initialize)

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

;;; Theme
;; Install gruvbox-theme
(use-package gruvbox-theme
  :ensure t
  )

;; Set gruvbox-theme as the system theme
(load-theme 'gruvbox-dark-hard t)

;; Remove fringes
(set-fringe-mode 0)

;; Remove startup screen
(setq inhibit-startup-screen t)

;;; Editor
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

;;;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              indent-line-function 'insert-tab)

;;;; Text Faces
(custom-set-faces
 '(bold ((t (:foreground "#008000" :weight bold))))
 '(italic ((t (:foreground "#B0A030" :slant italic))))
 '(strike-through ((t (:foreground "#8B0000" :strike-through t)))))

;;;; Programming
;;;;; C/Cpp
(defun my/c-cpp-mode-setup ()
  "Set basic c and cpp offset."
  (setq c-basic-offset 4))

;; Set hook to set indentation when in c/cpp file
(add-hook 'c-mode-common-hook 'my/c-cpp-mode-setup)

;; Disable line numbers
(global-display-line-numbers-mode 0)

;;;; Outline
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

;;;; IBuffer
;;;;; Configuration
(use-package ibuffer-sidebar
  :ensure t
  :config
  )

;;;;; Functions - IBuffer-sidebar Toggle
(defun my/ibuffer-sidebar-toggle ()
  "Toggle `ibuffer-sidebar'"
  (interactive)
  (ibuffer-sidebar-toggle-sidebar)
  (ibuffer-sidebar-mode))

;;;; Dired-sidebar
;;;;; Configuration
(use-package dired-sidebar
  :ensure t
  :config
  )

;;;;; Functions - Dired-sidebar Toggle
(defun my/dired-sidebar-toggle ()
  "Toggle `dired-sidebar'"
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (dired-sidebar-mode))

;;;; Pretty-hydra
;;;;; Configuration
(use-package pretty-hydra
  :ensure t
  )

;;;; Which-key
;;;;; Configuration
(use-package which-key
  :ensure t
  :config
  ;; Setup which-key-mode
  (which-key-mode)
  )

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
;;;; Functions: GUI Display Show/Hide
(defun my/gui-hide-bars ()
  "Disable scroll bar, menu bar, and tool bar."
  (interactive)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(defun my/gui-show-bars ()
  "Enable scroll bar, menu bar, and tool bar."
  (interactive)
  (scroll-bar-mode 1)
  (menu-bar-mode 1)
  (tool-bar-mode 1))

;; Hide GUI on startup if in LINUX mode
(when (eq system-type 'gnu/linux)
  (my/gui-hide-bars))

;;;; GUI ToolBar Items
;; Add additional GUI options when Android system is detected
(when (eq system-type 'android)
  ;; Add tool-bar options for zooming in
  (tool-bar-add-item "zoom-in" 'text-scale-increase
                     'text-scale-increase
                     :help "Zoom In")

  ;; Add tool-bar options for zooming out
  (tool-bar-add-item "zoom-out" 'text-scale-decrease
                     'text-scale-decrease
                     :help "Zoom Out")

  ;; Add tool-bar option for Org Cycle
  (tool-bar-add-item "right-arrow" 'org-cycle
                     'org-cycle
                     :help "Org Cycle")

  ;; Add tool-bar option for Org Ctrl-C Ctrl-C
  (tool-bar-add-item "prev-node" 'org-ctrl-c-ctrl-c
                     'org-ctrl-c-ctrl-c
                     :help "Execute Org Ctrl-C Ctrl-C")
  )

;;;; Easymenu
;;;;; Configuration
(use-package easymenu
  :config
    ;; Define "Packages" menu
    (easy-menu-define my/packages-center-menu nil "My Packages Menu"
      '("Packages"
        ("Org"
         ("Schedule"
          ["Add" my/org-add-schedule t]
          ["Remove" my/org-remove-schedule t])
         ("States"
          ["Log as TODO" my/org-log-todo t]
          ["Log as DOING" my/org-log-doing t]
          ["Log as DONE" my/org-log-done t]
          ["Log as SKIP" my/org-log-skip t]
          ["Log as FAIL" my/log-log-fail t]
          ["Change State" my/org-change-state t]
          ["Skip all Overdue" my/org-skip-all-overdue-tasks t]
          ["Show States in Calendar" my/org-logbook-display-states-on-calendar t])
         ("Clock"
          ["Clock In" my/org-clock-in t]
          ["Clock Out" my/org-clock-out t]
          ["Display Clocks" org-clock-display]
          ["Show Clock Analysis" org-analyzer-start t])
         ("Notes"
          ["Add Note" my/org-add-note t]
          ["Show Notes in Calendar" my/org-logbook-display-notes-on-calendar t]))
        ("Org-Agenda"
         ["Day View" my/org-agenda-day-view t]
         ["Week View" my/org-agenda-week-view t]
         ["Year View" my/org-agenda-year-view t]
         ["Inventory View" my/org-agenda-inventory t])
        ("Org-Roam"
         ("Dailies"
          ["Go to Today" org-roam-dailies-goto-today t]
          ["Find Date" org-roam-dailies-find-date t])
         ("Nodes"
          ["Find Node" org-roam-node-find t]
          ["Open Graph" org-roam-ui-open t]))
        ("Bookmarks"
         ["List Bookmarks" list-bookmarks t])))

    ;; Add the packages center menu to the toolbar to the left of "Tools" section
    (easy-menu-add-item global-map '("menu-bar") my/packages-center-menu "Tools")

    ;; Define "Authentication" menu
    (easy-menu-define my-auth-menu nil "My Authentication Menu"
      '("Auth-Sources"
        ["Get TOTP" my/totp-display t]
        ["Get Password" my/pass-display t]))

    ;; Add the "Authentication" menu to the "Tools" menu
    (easy-menu-add-item nil '("tools") my-auth-menu "Games")

    ;; Add a line separator after the "Authentication" menu
    (easy-menu-add-item nil '("tools") "--" "Games")
    )

;;; Dashboard
;;;; Configuration
(use-package dashboard
  :ensure t
  :config
  ;; Set the startup appearance. Make it minimal and remove almost everything
  (setq dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-banner-logo-title "Welcome to Emacs"
        dashboard-items nil
        dashboard-set-footer nil
        dashboard-set-init-info t
        dashboard-set-heading-icons t
        dashboard-agenda-prefix-format "%-10:s %t"
        dashboard-agenda-time-string-format "%Y-%m-%d %H:%M"
        dashboard-agenda-sort-strategy '(time-up))
  (add-to-list 'dashboard-items '(agenda) t)

  ;; Set up startup hook
  (dashboard-setup-startup-hook)
  )

;;;; Functions - Agenda Dashboard Relative Days
(defun my/dashboard-agenda--formatted-time--use-relative-days-advice (orig-fun &rest args)
  "Modifies the display of time in the dashboard agenda.
If the time corresponds to 'today', 'yesterday', or 'tomorrow', it replaces the date with these words.
Keeps the time part unless it's exactly 00:00, in which case only the relative date is displayed."
  (let* ((original-time-string (apply orig-fun args))
         (entry-time (org-get-scheduled-time (point)))
         (relative-date (my/time-relative-date entry-time))
         (time-part (format-time-string "%H:%M" entry-time))
         (time-is-midnight (string= time-part "00:00")))
    (if relative-date
        (if time-is-midnight
            relative-date
          (concat relative-date " " time-part))
      original-time-string)))

;; Add advice to change the date format of agenda items to 'yesterday', 'today' or 'tomorrow'
(advice-add 'dashboard-agenda--formatted-time :around #'my/dashboard-agenda--formatted-time--use-relative-days-advice)

;;; Org-mode
;;;; Org
;;;;; Configuration
(use-package org
  :ensure t
  :config
  ;; Set org directory
  (setq org-directory my-notes-directory)

  ;; Set org-mode preferences for buffer display
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
  (add-hook 'org-mode-hook #'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'org-hide-block-all)
  (add-hook 'org-mode-hook 'org-hide-drawer-all)

  ;; Set custom faces for scheduled headings
  (custom-set-faces
   '(org-scheduled ((t (:foreground "#555555"))))
   '(org-scheduled-today ((t (:foreground "grey")))))

  ;; Configure heading logs to be logged in "LOGBOOK" drawer
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out t)

  ;; Tags excluded from inheritance
  (setq org-tags-exclude-from-inheritance '("goal" "food" "exercise"))

  ;; Display inline images on startup
  (setq org-startup-with-inline-images t)

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

;;;;; Functions - Clocking
(defun my/org-clock-in ()
  "Clock in the current org heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-clock-in)
    (org-clock-in)))

(defun my/org-clock-out ()
  "Clock out of the current org heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-clock-out)
    (org-clock-out)))

;;;;; Functions - Datetime Insertion
(defun my/org-insert-current-date-time ()
  "Insert the current date and time along with the three-letter weekday name in
the format YYYY-MM-DD Day H:M."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a %H:%M")))

;;;;; Functions - Scheduling
(defun my/org-add-schedule ()
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

(defun my/org-remove-schedule ()
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

;;;;; Functions - State Change
(defun my/org-log-todo ()
  "Mark current heading as TODO"
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "TODO")
    (org-todo "TODO")))

(defun my/org-log-doing ()
  "Mark current heading as DOING"
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "DOING")
    (org-todo "DOING")))

(defun my/org-log-done ()
  "Mark current heading as DONE"
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "DONE")
    (org-todo "DONE")))

(defun my/org-log-skip ()
  "Mark current heading as SKIP"
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "SKIP")
    (org-todo "SKIP")))

(defun my/org-log-fail ()
  "Mark current heading as FAIL"
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "FAIL")
    (org-todo "FAIL")))

(defun my/org-change-state ()
  "Change state of a current heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo)
    (org-todo)))

(defun my/org-skip-all-overdue-tasks ()
  "Mark tasks scheduled for yesterday or earlier as SKIP and
log them as changed on their scheduled date."
  (interactive)
  (dolist (file (directory-files org-directory t "\\.org$"))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-scheduled-time-regexp nil t)
          (let ((scheduled-time (org-get-scheduled-time (point))))
            (when (and scheduled-time
                       (< (time-to-days scheduled-time)
                          (time-to-days (current-time))))
              (unless my-time-override-lock
                (setq my-time-override-lock t)
                (my/time-adjust-time (format-time-string "<%Y-%m-%d %a>" scheduled-time))
                (advice-add 'current-time :override #'my/time-override-current-time)
                (org-todo "SKIP")
                (advice-remove 'current-time #'my/time-override-current-time)
                (setq my-adjusted-time nil)
                (setq my-time-override-lock nil)))))))))

;;;;; Functions - Notes
(defun my/org-add-note ()
  "Add a note to an org heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-add-note)
    (org-add-note)))

;;;;; Functions - Logbook Calendar Display
(defun my/org-logbook--parse-logbook-states (logbook beg buffer)
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

(defun my/org-logbook--parse-logbook-notes (logbook beg buffer)
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

(defvar my-logbook-calendar-view-active nil
  "Flag to indicate if the custom TODO calendar view is active.")

(defun my/org-logbook--reset-calendar-view-flag ()
  "Reset the  calendar view flag."
  (setq my-logbook-calendar-view-active nil))

;; Define custom faces for different TODO states
(defface my-mark-DONE '((t :background "#006400")) "")
(defface my-mark-SKIP '((t :background "#999900")) "")
(defface my-mark-FAIL '((t :background "#8B0000")) "")
(defface my-mark-DOING '((t :background "#4B0082")) "")
(defface my-mark-NOTE '((t :background "#006400")) "")

(defun my/org-logbook--mark-calendar-dates (entries)
  "Mark days in the calendar for each entry in ENTRIES."
  (setq my-logbook-marked-entries entries)
  (let ((last-date (current-time)))
    (dolist (entry entries)
      (let* ((state (car entry))
             (date (cadr entry))
             (next-entry (cadr (member entry entries)))
             (end-date (if next-entry (cadr next-entry) last-date)))
        (when (calendar-date-is-visible-p date)
          (calendar-mark-visible-date date (intern (concat "my-mark-" state))))
        (when (string= state "DOING")
          (let ((current-date date))
            (while (and (not (equal current-date end-date))
                        (calendar-date-is-visible-p current-date))
              (calendar-mark-visible-date current-date 'my-mark-DOING)
              (setq current-date (calendar-gregorian-from-absolute
                                  (+ 1 (calendar-absolute-from-gregorian current-date)))))))))))

(defun my/org-logbook--mark-calendar-date-reapply ()
  "Reapply markings to the calendar."
  (when my-logbook-calendar-view-active
    (my/org-logbook--mark-calendar-dates my-logbook-marked-entries)))

(defun my/org-logbook-display-states-on-calendar ()
  "Show the state history of the heading at point in the org-agenda buffer or an
org file on the year calendar."
  (interactive)
  (setq my-logbook-calendar-view-active t)
  (setq my-logbook-marked-entries '())
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
                (setq entries (my/org-logbook--parse-logbook-states logbook beg buffer))
                (calendar)
                (my/org-logbook--mark-calendar-dates entries))
            (error "No LOGBOOK found for this TODO.")))))))

(defun my/org-logbook-display-notes-on-calendar ()
  "Show the notes of the heading at point in the org-agenda buffer or an org file on the year calendar."
  (interactive)
  (setq my-logbook-calendar-view-active t)
  (setq my-logbook-marked-entries '())
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
                (setq entries (my/org-logbook--parse-logbook-notes logbook beg buffer))
                (calendar)
                (my/org-logbook--mark-calendar-dates entries))
            (error "No LOGBOOK found for this TODO.")))))))

(defun my/org-logbook--goto-entry (date)
  "Navigate to the logbook entry corresponding to DATE."
  (interactive (list (calendar-cursor-to-date t)))
  (if my-logbook-calendar-view-active
      (let ((entry (cl-find-if (lambda (entry) (equal date (cadr entry))) my-logbook-marked-entries)))
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
  (add-hook 'calendar-move-hook 'my/org-logbook--mark-calendar-date-reapply)

  ;; Add hook to reset the custom calendar view flag when the calendar is closed
  (add-hook 'calendar-exit-hook 'my/org-logbook--reset-calendar-view-flag)
  
  ;; Bind terminal emacs "Enter" clicks
  (define-key calendar-mode-map (kbd "RET") 'my/org-logbook--goto-entry)
  
  ;; Bind GUI emacs "Enter" clicks
  (define-key calendar-mode-map (kbd "<return>") 'my/org-logbook--goto-entry))

;;;; Org-agenda
;;;;; Configuration
(use-package org-agenda
  :after org
  :config
  ;; Set org-agenda-files to org-directory
  (setq org-agenda-files (list org-directory))
  
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

;;;;; Functions - Agenda Views
(defun my/org-agenda--switch-to-view (view-fn)
  "Switch to the given Org Agenda view function VIEW-FN."
  (if (eq major-mode 'org-agenda-mode)
      (progn
        (unless (eq org-agenda-type 'agenda)
          (org-agenda-exit)
          (org-agenda-list))
        (run-with-idle-timer 0.1 nil view-fn))
    (org-agenda-list)
    (run-with-idle-timer 0.1 nil view-fn)))

(defun my/org-agenda-day-view ()
  "Switch to the Org Agenda daily view from anywhere in Emacs."
  (interactive)
  (my/org-agenda--switch-to-view 'org-agenda-day-view))

(defun my/org-agenda-week-view ()
  "Switch to the Org Agenda weekly view from anywhere in Emacs."
  (interactive)
  (my/org-agenda--switch-to-view 'org-agenda-week-view))

(defun my/org-agenda-year-view ()
  "Switch to the Org Agenda yearly view from anywhere in Emacs."
  (interactive)
  (my/org-agenda--switch-to-view 'org-agenda-year-view))

;;;; Org-super-agenda
;;;;; Configuration
(use-package org-super-agenda
  :after org-agenda
  :ensure t
  :config
  ;; Enable org-super-agenda mode
  (org-super-agenda-mode)
  )

;;;;; Functions - Auto-Parents
(defun my/org-super-agenda-get-todo-parent (item)
  "Get the parent heading of ITEM, or if none, the file title or filename."
  (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
    (if (org-up-heading-safe)
        (org-entry-get nil "ITEM") 
      (let ((keywords (org-collect-keywords '("TITLE"))))
        (if keywords
            (car (cdr (assoc "TITLE" keywords))) 
          (file-name-nondirectory (buffer-file-name))))))) 

(org-super-agenda--def-auto-group parent "their parent heading or file title/filename"
  :key-form (my/org-super-agenda-get-todo-parent item))

;;;;; Functions - Inventory
(defun my/org-agenda-inventory ()
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

  ;; Setup preview of org-roam nodes
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:30}" 'face 'org-tag)))

  (org-roam-setup)
  )

;;;; Functions - Inserting Nodes by Tags
(defun my/org-roam-insert-nodes-by-tags--get-node-heirarchy (node)
  "Get the hierarchy of NODE as a list of titles.
The hierarchy includes the NODE title, its ancestor titles, and the parent node title."
  (let ((titles '())
        (title (org-roam-node-title node))
        (file-path (org-roam-node-file node))
        (file-title)
        (olp (org-roam-node-olp node)))
    (setq file-title (caar (org-roam-db-query [:select title :from nodes :where (= file $s1)] file-path)))
    (when (and file-title (not (equal file-title title)) (not (equal file-title (car olp))))
      (setq titles (append titles (list file-title))))
    (when olp
      (setq titles (append titles (nreverse olp))))
    (when title
      (setq titles (append titles (list title))))
    titles))

(defun my/org-roam-insert-nodes-by-tags(keywords exclude-keywords &optional filter-fn)
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
                                     (let ((hierarchy-a (mapconcat #'identity (my/org-roam-insert-nodes-by-tags--get-node-heirarchy a) "->"))
                                           (hierarchy-b (mapconcat #'identity (my/org-roam-insert-nodes-by-tags--get-node-heirarchy b) "->")))
                                       (string< hierarchy-a hierarchy-b))))))
          (dolist (node sorted-nodes)
            (let* ((id (org-roam-node-id node))
                   (hierarchy (my/org-roam-insert-nodes-by-tags--get-node-heirarchy node))
                   (arrow-chain (if (> (length hierarchy) 1)
                                    (mapconcat #'identity hierarchy "->")
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

;;;;; Functions - Android Notifications
(defun my/alert-android-notifications-notify (info)
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
                    :notifier #'my/alert-android-notifications-notify)

;;;; Org-alert
;;;;; Configuration
(use-package org-alert
  :ensure t
  :after org
  :custom
  ;; Use different backends depending on the platform
  (alert-default-style (if (eq system-type 'android)
                           'android-notifications
                         'libnotify))
  :config
  ;; Setup timing
  (setq org-alert-interval 300
        org-alert-notify-cutoff 10
        org-alert-notify-after-event-cutoff 10)
  
  ;; Setup notification title (if using 'custom)
  (setq org-alert-notification-title "Org Alert Reminder")
  
  ;; Use non-greedy regular expression
  (setq org-alert-time-match-string
        "\\(?:SCHEDULED\\|DEADLINE\\):.*?<.*?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\).*>")
  
  ;; Enable org-alert
  (org-alert-enable)
  )

;;;;; Functions - Notification Titles
(defvar my-org-alert-title-type 'custom
  "Control the title type for `org-alert' notifications.
   Possible values are:
      - 'custom: The usual workings of org-alert package. Uses `org-alert-notification-title'
                 as the title of notifications sent.
      - 'parent: Uses the immediate parent heading of the TODO as the title of the notification.
                 If the TODO does not have a parent, it uses the file title instead. If the file
                 does not have a title, it uses the filename as the title for notifications.")

(defun my/org-alert--get-todo-parent ()
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
`my/org-alert--get-todo-parent' function which retrieves the parent heading or file title/name."
  (let ((head (org-alert--strip-text-properties (org-get-heading t t t t)))
        (parent-or-file-head (my/org-alert--get-todo-parent)))
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

(defun my/org-alert-update-advices ()
  "Add or remove advice based on the value of `org-alert-title-type'."
  (cond ((eq my-org-alert-title-type 'parent)
         (advice-add 'org-alert--parse-entry :around #'org-alert--parse-entry--use-parent-as-title-advice)
         (advice-add 'org-alert--dispatch :around #'org-alert--dispatch--use-parent-as-title-advice))
        ((eq my-org-alert-title-type 'custom)
         (advice-remove 'org-alert--parse-entry #'org-alert--parse-entry--use-parent-as-title-advice)
         (advice-remove 'org-alert--dispatch #'org-alert--dispatch--use-parent-as-title-advice))))

;; Set up 'parent mode
(setq my-org-alert-title-type 'parent)
;; Update to set up or remove advices based on my-org-alert-title-type
(my/org-alert-update-advices)

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
  )

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

;;;;; Functions - Screenshot Filename
(defun my/org-download-clipboard--prompt-for-name-advice (orig-fun &optional basename)
  "Advice to prompt for a basename before calling `org-download-clipboard'."
  (message "Calling advice function")
  (let ((name (if (called-interactively-p 'any)
                  (read-string "Enter image name (without extension): ")
                basename)))
    (funcall orig-fun (if (string-empty-p name) basename (concat name ".png")))))

(advice-add 'org-download-clipboard :around #'my/org-download-clipboard--prompt-for-name-advice)

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
(defun my/org-media-note--format-picture-file-name--prepend-timestamp-advice (orig-func &rest args)
  "Advice to prepend the current timestamp to the filename created by `org-media-note--format-picture-file-name'."
  (let ((original-filename (apply orig-func args))
        (timestamp (format-time-string "%Y-%m-%d_%H-%M-%S")))
    (concat timestamp "_" original-filename)))

(advice-add 'org-media-note--format-picture-file-name :around #'my/org-media-note--format-picture-file-name--prepend-timestamp-advice)

;;;;; Functions - Invalid Characters
(defun my/remove-invalid-characters-from-filename (filename)
  "Remove invalid characters from filename in order for it to sync to Android using syncthing."
  (replace-regexp-in-string "[/*\":<>?|]" "" filename))

(defun my/org-media-note--format-picture-file-name--remove-invalid-characters-from-filename-advice (orig-func &rest args)
  "Advice to remove invalid characters from filename in `org-media-note--format-picture-file-name'."
  (my/remove-invalid-characters-from-filename (apply orig-func args)))

(advice-add 'org-media-note--format-picture-file-name :around #'my/org-media-note--format-picture-file-name--remove-invalid-characters-from-filename-advice)

;;; Bookmarks
;;;; Eww
;;;;; Configuration
(use-package eww
  :config
  ;; Set default eww-bookmarks directory
  (setq eww-bookmarks-directory (concat my-documents-directory "Bookmarks/"))
  )

;;;; Bookmarks
;;;;; Configuration
(use-package bookmark
  :config
  ;; Set default bookmark file
  (setq bookmark-default-file (concat my-documents-directory "Bookmarks/bookmarks"))
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

;;;;; Functions - Bookmark Paths on Different Platforms
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

;;; Date and Time
;;;; Functions - Relative Dates
(defun my/time-relative-date (time)
  "Determines if the given TIME is 'today', 'yesterday', or 'tomorrow'.
Returns the corresponding string or nil if the time doesn't match any of these.
TIME is expected to be in Emacs internal time format."
  (when time
    (let* ((current-time (current-time))
           (current-date (decode-time current-time))
           (entry-date (decode-time time))
           (current-day-num (time-to-days (apply #'encode-time (decode-time (current-time)))))
           (entry-day-num (time-to-days (apply #'encode-time (decode-time time))))
           (day-difference (- current-day-num entry-day-num)))
      (cond ((eq day-difference 0) "today    ")
            ((eq day-difference 1) "yesterday")
            ((eq day-difference -1) "tomorrow ")))))

;;;; Functions - Time Adjustment
(defvar my-adjusted-time nil
  "Adjusted time. This time will replace current time.")

(defvar my-time-override-lock nil
  "Lock to prevent concurrent access to the time override.")

(defun my/time-adjust-time (time)
  "Temporarily adjust `current-time' to the given TIME."
  (setq my-adjusted-time (append (org-read-date nil t time) '(0 0))))

(defun my/time-override-current-time ()
  "Override for `current-time' using `my/time-adjust-time'."
  (or my/time-adjust-time (current-time)))

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
        `((:source ,(concat my-documents-directory ".secrets/.authinfo-pass.gpg"))
          (:source ,(concat my-documents-directory ".secrets/.authinfo-totp.gpg"))))

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

(defun my/totp-display (auth)
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
(defun my/password-display (auth)
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
