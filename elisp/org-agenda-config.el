;;; org-agenda.el
;;
;; Author: Danijel Camdzic
;; Maintainer: Danijel Camdzic <danijelcamdzic@tuta.com>
;;
;; License: NO LICENCE
;;
;;; Code:

;;; --------- Org-agenda ---------
;;
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
(setq org-agenda-prefix-format '((agenda . "%t ")
                                 (todo . "%t ")
                                 (tags . "")
                                 (search . "%i")))
(setq org-agenda-scheduled-leaders '("" ""))
(setq org-agenda-span 7)
(setq org-agenda-show-future-repeats 'next)

;; Set the org-agenda time-grid
(setq org-agenda-use-time-grid t)
(setq org-agenda-time-grid
      '((daily weekly today require-timed)
        (0000 0200 0400 0600 0800 1000 1200 1400 1600 1800 2000 2200 2359)
        "......" "----------------"))

;; Set the org-agenda-files to use every file in org-directory (except "/dailies")
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

(defun my/todo-log-done ()
  "Mark current org task as DONE from the agenda view or directly on
the TODO heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "DONE")
    (org-todo "DONE")))

(defun my/todo-log-skip ()
  "Mark current org task as SKIP from the agenda view or directly on
the TODO heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "SKIP")
    (org-todo "SKIP")))

(defun my/todo-log-fail ()
  "Mark current org task as FAIL from the agenda view or directly on
the TODO heading."
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

(defun my/parse-logbook (logbook beg buffer)
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

(defun my/mark-todo-entries (entries)
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

(defun my/show-todo-in-calendar ()
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
                (setq entries (my/parse-logbook logbook beg buffer))
                (calendar)
                (my/mark-todo-entries entries))
            (error "No LOGBOOK found for this TODO.")))))))

(defun my/reapply-markings ()
  "Reapply markings to the calendar."
  (when my/calendar-todo-view-active
    (my/mark-todo-entries my-marked-entries)))

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

;; Provide package for use
(provide 'org-agenda-config)
