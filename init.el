;; init.el
;;
;; Author: Danijel Camdzic
;; Maintainer: Danijel Camdzic <danijelcamdzic@tuta.com>
;;
;; License: NO LICENCE

;; --------- Melpa ---------

;; Add melpa package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Initialize packages
(package-initialize)

;; --------- Directories ---------

;; Define and set the directory names
;; Home directory
(setq my-home-directory
      (cond
       ((eq system-type 'gnu/linux) "~/")
       ((eq system-type 'android) "/storage/emulated/0/")
       (t "~/")))
;; Notes directory
(setq my-notes-directory (concat my-home-directory "Notes/"))
;; Documents directory
(setq my-documents-directory (concat my-home-directory "Documents/"))

;; --------- Text editing and completion ---------

;; Ensure that company package is installed and loaded
(unless (package-installed-p 'company)
  (package-install 'company))
(require 'company)

;; Enable company mode and add hook
(company-mode 1)
(add-hook 'after-init-hook 'global-company-mode)

;; Ensure that vertico package is installed and loaded
(unless (package-installed-p 'vertico)
  (package-install 'vertico))
(require 'vertico)

;; Ensure that orderless package is installed loaded
(unless (package-installed-p 'orderless)
  (package-install 'orderless))
(require 'orderless)

;; Enable and configure vertico
(vertico-mode 1)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

;; Configure text formatting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Disable displaying line numbers (in all file types)
(global-display-line-numbers-mode 0)

;; Disable backup and lock files
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Open everything in a separate buffer
(setq display-buffer-alist
      '((".*"
         (display-buffer-same-window)
         (inhibit-same-window . nil))))

;; Set touch-screen-display-keyboard if Android is the system type
(cond
 ((eq system-type 'android)
  (setq touch-screen-display-keyboard t)))

;; --------- Dashboard ---------

;; Ensure that dashboard package is installed and loaded
(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))
(require 'dashboard)

;; Configure dashboard
(setq dashboard-startup-banner 'official)
(setq dashboard-center-content t)
(setq dashboard-banner-logo-title "Welcome to Emacs")
(setq dashboard-items nil)
(setq dashboard-set-footer nil)
(setq dashboard-set-init-info t)
(setq dashboard-set-heading-icons t)
(setq dashboard-agenda-prefix-format "%-10:s %t")
(setq dashboard-agenda-time-string-format "%Y-%m-%d %H:%M")
(setq dashboard-agenda-sort-strategy '(time-up))
;; Filter the times shown
(setq dashboard-match-agenda-entry "+SCHEDULED<=\"<+1d>\"")
(add-to-list 'dashboard-items '(agenda) t)

(defun my/dashboard-agenda--formatted-time-advice-use-relative-days (orig-fun &rest args)
  "Modifies the display of time in the dashboard agenda.
If the time corresponds to 'today', 'yesterday', or 'tomorrow', it replaces the date with these words.
Keeps the time part unless it's exactly 00:00, in which case only the relative date is displayed."
  (let* ((original-time-string (apply orig-fun args))
         (entry-time (org-get-scheduled-time (point)))
         (relative-date (my/relative-date entry-time))
         (time-part (format-time-string "%H:%M" entry-time))
         (time-is-midnight (string= time-part "00:00")))
    (if relative-date
        (if time-is-midnight
            relative-date
          (concat relative-date " " time-part))
      original-time-string)))

(defun my/relative-date (time)
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
      (cond ((eq day-difference 0) "today")
            ((eq day-difference 1) "yesterday")
            ((eq day-difference -1) "tomorrow")))))

;; Add advice to change the date format to 'yesterday', 'today' or 'tomorrow'
(advice-add 'dashboard-agenda--formatted-time :around #'my/dashboard-agenda--formatted-time-advice-use-relative-days)

;; Start dashboard
(dashboard-setup-startup-hook)

;; --------- GUI menu ---------

;; Add tool-bar options for zooming in
(tool-bar-add-item "zoom-in" 'text-scale-increase
                   'text-scale-increase
                   :help "Zoom In")

;; Add tool-bar options for zooming out
(tool-bar-add-item "zoom-out" 'text-scale-decrease
                   'text-scale-decrease
                   :help "Zoom Out")

(require 'easymenu)

;; Add a toolbar falling menu for easy access to functions
(easy-menu-define my/command-center-menu nil "My Commands Menu"
  '("Command"
    ("Agenda"
     ["Day View" my/org-agenda-day-view t]
     ["Week View" my/org-agenda-week-view t]
     ["Year View" my/org-agenda-year-view t]
     ["Inventory View" my/org-agenda-inventory t])
    ("Roam"
     ("Dailies"
      ["Go to Today" org-roam-dailies-goto-today t]
      ["Find Date" org-roam-dailies-find-date t])
     ("Nodes"
      ["Find Node" org-roam-node-find t]
      ["Open Graph" org-roam-ui-open t]))
    ("Schedule"
     ["Add" my/add-schedule t]
     ["Remove" my/remove-schedule t])
    ("States"
     ["Log as TODO" my/todo-log-todo t]
     ["Log as DONE" my/todo-log-done t]
     ["Log as SKIP" my/todo-log-skip t]
     ["Log as FAIL" my/todo-log-fail t]
     ["Change State" my/todo-change-state t]
     ["Show in Calendar" my/show-states-in-calendar t])
    ("Clock"
     ["Clock In" my/clock-in t]
     ["Clock Out" my/clock-out t]
     ["Display Clocks" org-clock-display]
     ["Show Clock Analysis" org-analyzer-start t])
    ("Note"
     ["Add Note" my/add-note t]
     ["Show in Calendar" my/show-notes-in-calendar t])))

;; Add the menu option to the toolbas to the left of "Tools" section
(easy-menu-add-item global-map '("menu-bar") my/command-center-menu "Tools")

;; Define the "AI" menu
(easy-menu-define my-ai-menu nil "My AI Menu"
  '("AI"
    ("OpenAI"
     ["Open ChatGPT Shell" my/open-chatgpt-shell t])))

;; Add the "AI" menu to the "Tools" menu, before "Games"
(easy-menu-add-item nil '("tools") my-ai-menu "Games")
(easy-menu-add-item nil '("tools") '("--") "Games")

;; --------- Org-mode ---------

;; Use org-mode
(require 'org)

;; Use org-tempo
(require 'org-tempo)

;; Set the org-directory
(setq org-directory my-notes-directory)

;; Set the main Org Roam directory and the directory for daily notes
(setq org-roam-directory org-directory)
(setq org-roam-dailies-directory (concat org-directory "dailies/"))

;; Include only .org files and exclude all else
(setq org-roam-file-exclude-regexp "\\(\\.gpg\\)$")

;; Set the eww-bookmarks directory
(setq eww-bookmarks-directory (concat my-documents-directory "bookmarks/"))

;; Set the default bookmarks file
(setq bookmark-default-file (concat my-documents-directory "bookmarks/bookmarks"))

;; Customize custom faces in org-mode
(with-eval-after-load 'org
  (custom-set-faces
   '(bold ((t (:foreground "#008000" :weight bold))))
   '(italic ((t (:foreground "#B0A030" :slant italic))))
   '(org-scheduled ((t (:foreground "#555555"))))
   '(org-scheduled-today ((t (:foreground "grey"))))
   '(strike-through ((t (:foreground "#8B0000" :strike-through t))))))

;; Set up text indentation
(setq org-startup-indented t)

;; Add a hook to org-mode to limit the column width to 80 characters
(add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook #'turn-on-auto-fill)

;; Show everything when opening org file but hide code blocks and drawers
(add-hook 'org-mode-hook 'org-hide-block-all)
(add-hook 'org-mode-hook 'org-hide-drawer-all)

;; Set the time-stamp package to update the time an org file was last edited
(setq time-stamp-format "%Y-%m-%d %H:%M")
(setq time-stamp-start "# Last-edited: ")
(setq time-stamp-end "$")
(add-hook 'before-save-hook 'time-stamp)

(defun my/clock-in ()
  "Clock in the current org heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-clock-in)
    (org-clock-in)))

(defun my/clock-out ()
  "Clock out of the current org heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-clock-out)
    (org-clock-out)))

(defun my/insert-current-date-time ()
  "Insert the current date and time along with the three-letter weekday name in
the format YYYY-MM-DD Day H:M."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a %H:%M")))

;; --------- Org-roam ---------

;; Ensure org-roam package is installed and loaded
(unless (package-installed-p 'org-roam)
  (package-install 'org-roam))
(require 'org-roam)

;; Setup org-roam
(org-roam-setup)

;; Setup preview of org-roam nodes
(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:30}" 'face 'org-tag)))

(defun my/get-org-roam-node-hierarchy (node)
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

(defun my/insert-org-roam-nodes-by-tag(keywords exclude-keywords &optional filter-fn)
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
                                     (let ((hierarchy-a (mapconcat #'identity (my/get-org-roam-node-hierarchy a) "->"))
                                           (hierarchy-b (mapconcat #'identity (my/get-org-roam-node-hierarchy b) "->")))
                                       (string< hierarchy-a hierarchy-b))))))
          (dolist (node sorted-nodes)
            (let* ((id (org-roam-node-id node))
                   (hierarchy (my/get-org-roam-node-hierarchy node))
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

;; --------- Org-analyzer ---------

;; Ensure the org-analyzer package is installed and loaded
(unless (package-installed-p 'org-analyzer )
  (package-install 'org-analyzer ))
(with-eval-after-load 'org
  (require 'org-analyzer))

;; Set directory for org-analyzer
(setq org-analyzer-org-directory org-directory)

;; --------- Websocket ---------

;; Ensure the websocket package is installed and loaded
(unless (package-installed-p 'websocket)
  (package-install 'websocket))
(with-eval-after-load 'org-roam
  (require 'websocket))

;; --------- Org-roam-ui ---------

;; Ensure the org-roam-ui package is installed and loaded
(unless (package-installed-p 'org-roam-ui)
  (package-install 'org-roam-ui))
(with-eval-after-load 'org-roam
  (require 'org-roam-ui))

;; --------- Org-transclusion ---------

;; Ensure the org-transclusion package is installed and loaded
(unless (package-installed-p 'org-transclusion)
  (package-install 'org-transclusion))
(require 'org-transclusion)

;; --------- Org-download ---------

;; Ensure the org-download package is installed and loaded
(unless (package-installed-p 'org-download)
  (package-install 'org-download))
(require 'org-download)

;; --------- Org-agenda ---------

;; Ensure the org-super-agenda package is installed and loaded
(unless (package-installed-p 'org-super-agenda)
  (package-install 'org-super-agenda))
(require 'org-super-agenda)

;; Enable org-super-agenda
(org-super-agenda-mode)

;; Set the org-todo-keywords and their states
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)" "SKIP(s!)" "FAIL(f!)")))

;; Customize org-agenda view
(setq org-agenda-prefix-format  '((agenda . "  %t ")
                                  (todo . "%t ")
                                  (tags . "")
                                  (search . "%i")))
(setq org-agenda-sorting-strategy '((agenda time-up priority-down category-keep)
                                    (todo time-up priority-down category-keep)
                                    (tags time-up priority-down category-keep)
                                    (search time-up priority-down category-keep)))
(setq org-agenda-scheduled-leaders '("" ""))
(setq org-agenda-span 7)
(setq org-agenda-show-future-repeats 'next)

;; Set the org-agenda time-grid
(setq org-agenda-use-time-grid t)
(setq org-agenda-time-grid
      '((daily weekly today require-timed)
        (0000 0200 0400 0600 0800 1000 1200 1400 1600 1800 2000 2200 2359)
        "......" "----------------"))

;; Set the org-agenda-files to use every file in org-directory
(setq org-agenda-files (list org-directory))

;; Configure TODO properties to be logged in "LOGBOOK" drawer
(setq org-log-into-drawer t)
(setq org-log-note-clock-out t)

;; Set calendar to start on Monday
(setq calendar-week-start-day 1)

(defun my/org-agenda-switch-to-view (view-fn)
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
  (my/org-agenda-switch-to-view 'org-agenda-day-view))

(defun my/org-agenda-week-view ()
  "Switch to the Org Agenda weekly view from anywhere in Emacs."
  (interactive)
  (my/org-agenda-switch-to-view 'org-agenda-week-view))

(defun my/org-agenda-year-view ()
  "Switch to the Org Agenda yearly view from anywhere in Emacs."
  (interactive)
  (my/org-agenda-switch-to-view 'org-agenda-year-view))

(defun my/org-agenda-inventory ()
  "Open Org Agenda in the todos view mode with super agenda."
  (interactive)
  (let ((org-super-agenda-groups '((:auto-parent t))))
    (org-agenda nil "t")
    (setq org-super-agenda-groups '())))

(defun my/add-schedule ()
  "Add a scheduling timestamp to the current item in the Org Agenda or in
an org-mode file."
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

(defun my/remove-schedule ()
  "Remove the scheduling timestamp from the current item in the Org Agenda
or in an org-mode file."
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

(defun my/todo-log-todo ()
  "Mark current heading as TODO"
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "TODO")
    (org-todo "TODO")))

(defun my/todo-log-done ()
  "Mark current heading as DONE"
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "DONE")
    (org-todo "DONE")))

(defun my/todo-log-skip ()
  "Mark current heading as SKIP"
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "SKIP")
    (org-todo "SKIP")))

(defun my/todo-log-fail ()
  "Mark current heading as FAIL"
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "FAIL")
    (org-todo "FAIL")))

(defun my/todo-change-state ()
  "Change state of a TODO item."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo)
    (org-todo)))

(defun my/add-note ()
  "Add a note to an Org item."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-add-note)
    (org-add-note)))

;; Define custom faces for different TODO states which will color the calendar dates
;; based on the state of the TODO item on that day
(defface my-mark-DONE '((t :background "#006400")) "")
(defface my-mark-SKIP '((t :background "#999900")) "")
(defface my-mark-FAIL '((t :background "#8B0000")) "")
(defface my-mark-NOTE '((t :background "#008000")) "")

(defun my/parse-logbook-states (logbook beg buffer)
  "Parse a logbook string and return a list of entries."
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

(defun my/parse-logbook-notes (logbook beg buffer)
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

(defun my/mark-entries (entries)
  "Mark days in the calendar for each entry in ENTRIES."
  (setq my-marked-entries entries)
  (dolist (entry entries)
    (let ((state (car entry))
          (date (cadr entry)))
      (when (calendar-date-is-visible-p date)
        (calendar-mark-visible-date date (intern (concat "my-mark-" state)))))))

;; Global flag to determine if the custom calendar view is active
(defvar my/calendar-todo-view-active nil
  "Flag to indicate if the custom TODO calendar view is active.")

(defun my/show-states-in-calendar ()
  "Show the state history of the TODO at point in the org-agenda buffer or an
org file on the year calendar."
  (interactive)
  (setq my/calendar-todo-view-active t)
  (setq my-marked-entries '())
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
                (setq entries (my/parse-logbook-states logbook beg buffer))
                (calendar)
                (my/mark-entries entries))
            (error "No LOGBOOK found for this TODO.")))))))

(defun my/show-notes-in-calendar ()
  "Show the notes of the TODO at point in the org-agenda buffer or an org file on the year calendar."
  (interactive)
  (setq my/calendar-todo-view-active t)
  (setq my-marked-entries '())
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
                (setq entries (my/parse-logbook-notes logbook beg buffer))
                (calendar)
                (my/mark-entries entries))
            (error "No LOGBOOK found for this TODO.")))))))

(defun my/reapply-markings ()
  "Reapply markings to the calendar."
  (when my/calendar-todo-view-active
    (my/mark-entries my-marked-entries)))

;; Add hook to reapply markings each time the calendar is moved
(add-hook 'calendar-move-hook 'my/reapply-markings)

(defun my/reset-calendar-view-flag ()
  "Reset the custom calendar view flag."
  (setq my/calendar-todo-view-active nil))

;; Add hook to reset the custom calendar view flag when the calendar is closed
(add-hook 'calendar-exit-hook 'my/reset-calendar-view-flag)

(defun my/goto-logbook-entry (date)
  "Navigate to the logbook entry corresponding to DATE."
  (interactive (list (calendar-cursor-to-date t)))
  (if my/calendar-todo-view-active
      (let ((entry (cl-find-if (lambda (entry) (equal date (cadr entry))) my-marked-entries)))
        (if entry
            (progn
              (switch-to-buffer-other-window (nth 3 entry))
              (delete-other-windows)
              (goto-char (nth 2 entry))
              (recenter-top-bottom 0))
          (message "No logbook entry found for this date.")))
    (message "Not supported in this calendar.")))

;; Bind the mouse click on the date to logbook entry position
(with-eval-after-load 'calendar
;; for terminal emacs "Enter" clicks
  (define-key calendar-mode-map (kbd "RET") 'my/goto-logbook-entry)
;; for gui emacs "Enter" clicks
  (define-key calendar-mode-map (kbd "<return>") 'my/goto-logbook-entry))

;; Map keys to custom org-agenda functions
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "M-s") #'my/add-schedule)
  (define-key org-agenda-mode-map (kbd "M-r") #'my/remove-schedule)
  (define-key org-agenda-mode-map (kbd "M-t") #'my/todo-change-state)
  (define-key org-agenda-mode-map (kbd "M-n") #'my/add-note)
  (define-key org-agenda-mode-map (kbd "M-v") #'my/show-todo-in-calendar))

;; --------- EPA (GnuPG) ---------

;; Set the environment variable and configure EPA only if running on Android
(when (eq system-type 'android)
;; Set the environment variable to use GPG on Termux
  (setenv "GNUPGHOME" "/data/data/com.termux/files/home/.gnupg"))

;; Set the gpg default program
(setq epg-gpg-program "gpg2")

;; Require the epa-file package which provides Emacs Privacy Assistant
;; features for file encryption.
(require 'epa-file)

;; Enable the EPA file encryption/decryption features.
(epa-file-enable)

;; Set to nil to disable the key selection dialog
;; Emacs will use the default GPG key automatically.
(setq epa-file-select-keys nil)

;; Set the pinentry mode to loopback, allowing Emacs to
;; prompt for passphrases in the minibuffer.
;; This is useful when running Emacs in a terminal or
;; environment where GUI pinentry dialogs are not available.
(setq epa-pinentry-mode 'loopback)

;; --------- Auth-sources ---------

(require 'auth-source)

;; Set auth-sources files
;; Set auth-sources files using the my-documents-directory variable
(setq auth-sources
      `((:source ,(concat my-documents-directory ".auth-sources/.authinfo-api.gpg"))
        (:source ,(concat my-documents-directory ".auth-sources/.authinfo-totp.gpg"))
        (:source ,(concat my-documents-directory ".auth-sources/.authinfo-pass.gpg"))))

;; Enable authinfo-mode for auth-source files
(add-to-list 'auto-mode-alist '("\\.authinfo.*\\.gpg\\'" . authinfo-mode))

;; Clear cached passwords after buffers are switched
(add-hook 'buffer-list-update-hook 'auth-source-forget-all-cached)

;; --------- TOTP ---------

;; Taken from Jürgen Hötzel's `totp.el':
;; https://github.com/juergenhoetzel/emacs-totp

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

(defun totp--display (auth)
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
         (time-step 30) ;; TOTP is typically valid for 30 seconds
         (time-remaining (- time-step (mod current-time time-step)))
         (code (totp (funcall (plist-get auth :secret)))))
    (message "Your TOTP for '%s' is: %s (valid for %d more seconds, sent to kill ring)"
             (propertize (plist-get auth :host) 'face 'font-lock-keyword-face)
             (propertize code 'face 'font-lock-string-face)
             time-remaining)
    (kill-new code)
    code))

;; --------- Passwords ---------

(defun pass--display (auth)
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
    (message "Your password for '%s' is: %s"
             (propertize (plist-get auth :host) 'face 'font-lock-keyword-face)
             (propertize password 'face 'font-lock-string-face))))

;; --------- Chatgpt-shell ---------

;; Ensure that company package is installed and loaded
(unless (package-installed-p 'chatgpt-shell)
  (package-install 'chatgpt-shell))
(require 'chatgpt-shell)

;; Set API key to nil at the beginning
(setq chatgpt-shell-openai-key nil)

(defun my/set-chatgpt-shell-openai-key ()
  "Set the `chatgpt-shell-openai-key` variable from auth-source."
  (interactive)
  (setq chatgpt-shell-openai-key
        (auth-source-pick-first-password :host "api.openai.com")))

(defun my/open-chatgpt-shell ()
  "Set the OpenAI API key and then call the chatgpt-shell command."
  (interactive)
  (my/set-chatgpt-shell-openai-key)
  (chatgpt-shell))
