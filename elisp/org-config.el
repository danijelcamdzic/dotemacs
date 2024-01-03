;;; org-config.el -- Org and supporting/extending packages configuration

;;; Code:

;; Dependencies
(require 'user-config)                  ; User details and directory configuration
(require 'package-manager-config)       ; Package manager configuration (melpa and quelpa)
(require 'viewer-config)                ; Viewer packages configuration
(require 'time-management-config)       ; Calendar and other time related configuration

;; Org-mode configuration
(use-package org
  :ensure t
  :config
  (progn ;; Directories configuration
    (setq org-directory my-notes-directory))

  (progn ;; Appearance configuration
    ;; Custom faces
    (custom-set-faces
     '(org-scheduled ((t (:foreground "#555555"))))
     '(org-scheduled-today ((t (:foreground "grey")))))

    ;; Visibility of org items (drawer, code blocks, headings)
    (setq org-startup-indented t)
    (add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
    (add-hook 'org-mode-hook #'turn-on-auto-fill)
    (add-hook 'org-mode-hook 'org-hide-block-all)
    (add-hook 'org-mode-hook 'org-hide-drawer-all)

    ;; Display inline images on startup
    (setq org-startup-with-inline-images t))

  (progn ;; Org babel configuration
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
    (setq org-confirm-babel-evaluate nil))

  (progn ;; States and logging configuration
    ;; Set the org-todo-keywords and their states
    (setq org-todo-keywords
          '((sequence "TODO(t)" "DOING(i!)" "|" "DONE(d!)" "SKIP(s!)" "FAIL(f!)")))

    ;; Configure TODO properties to be logged in "LOGBOOK" drawer
    (setq org-log-into-drawer t)
    (setq org-log-note-clock-out t)

    ;; Tags excluded from inheritance
    (setq org-tags-exclude-from-inheritance '("goal" "food" "exercise"))

    ;; Define custom faces for different TODO states
    (defface my-mark-DONE '((t :background "#006400")) "")
    (defface my-mark-SKIP '((t :background "#999900")) "")
    (defface my-mark-FAIL '((t :background "#8B0000")) "")
    (defface my-mark-DOING '((t :background "#4B0082")) "")
    (defface my-mark-NOTE '((t :background "#006400")) ""))

  (progn ;; Binding configuration
    ;; Bind the mouse click on the date to logbook entry position
    (with-eval-after-load 'calendar
      ;; for terminal emacs "Enter" clicks
      (define-key calendar-mode-map (kbd "RET") 'my/goto-logbook-entry)
      ;; for gui emacs "Enter" clicks
      (define-key calendar-mode-map (kbd "<return>") 'my/goto-logbook-entry)))
  )

;; Org-mode clocking functions
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

;; Org-mode date and time function
(defun my/insert-current-date-time ()
  "Insert the current date and time along with the three-letter weekday name in
    the format YYYY-MM-DD Day H:M."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a %H:%M")))

;; Org-mode scheduling functions
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

;; Org-mode state-change functions
(defun my/todo-log-todo ()
  "Mark current heading as TODO"
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "TODO")
    (org-todo "TODO")))

(defun my/todo-log-doing ()
  "Mark current heading as DOING"
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo "DOING")
    (org-todo "DOING")))

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

;; Org-mode skip overdue tasks function
(defun my/skip-overdue-tasks ()
  "Mark tasks scheduled for yesterday or earlier as SKIP and log them as changed on their scheduled date."
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
              (unless my/time-override-lock
                (setq my/time-override-lock t)
                (my/adjust-time (format-time-string "<%Y-%m-%d %a>" scheduled-time))
                (advice-add 'current-time :override #'my/current-time-override)
                (org-todo "SKIP")
                (advice-remove 'current-time #'my/current-time-override)
                (setq my/adjusted-time nil)
                (setq my/time-override-lock nil)))))))))

;; Org-mode note functions
(defun my/add-note ()
  "Add a note to an Org item."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-add-note)
    (org-add-note)))

;; Org-mode logbook parsing functions
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

;; Org-mode logbook calendar view functions
(defvar my/calendar-todo-view-active nil
  "Flag to indicate if the custom TODO calendar view is active.")

(defun my/reset-calendar-view-flag ()
  "Reset the custom calendar view flag."
  (setq my/calendar-todo-view-active nil))

(defun my/mark-entries (entries)
  "Mark days in the calendar for each entry in ENTRIES."
  (setq my-marked-entries entries)
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

(defun my/reapply-markings ()
  "Reapply markings to the calendar."
  (when my/calendar-todo-view-active
    (my/mark-entries my-marked-entries)))

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

(with-eval-after-load 'calendar
  ;; Add hook to reapply markings each time the calendar is moved
  (add-hook 'calendar-move-hook 'my/reapply-markings)

  ;; Add hook to reset the custom calendar view flag when the calendar is closed
  (add-hook 'calendar-exit-hook 'my/reset-calendar-view-flag))

;; Org-agenda configuration
(use-package org-agenda
  :after org
  :config
  (progn ;; Directories configuration
    (setq org-agenda-files (list org-directory)))
  
  (progn ;; Appearance configuration
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
            "......" "----------------")))
  )

;; Org-agenda buffer view functions
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

;; Org-super-agenda configuration
(use-package org-super-agenda
  :after org-agenda
  :ensure t
  :config
  (progn ;; Setup
    ;; Enable org-super-agenda mode
    (org-super-agenda-mode))
  )

;; Org-super-agenda functions
(defun my/org-super-agenda-get-file-title-or-parent (item)
  "Get the parent heading of ITEM, or if none, the file title."
  (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
    (if (org-up-heading-safe)
        (org-entry-get nil "ITEM")  ; If there is a parent heading, use it
      (let ((keywords (org-collect-keywords '("TITLE"))))
        (if keywords
            (car (cdr (assoc "TITLE" keywords)))  ; If there is a file title, use it
          (file-name-nondirectory (buffer-file-name)))))))  ; Otherwise, use the file name

(org-super-agenda--def-auto-group parent "their parent heading or file title"
  :key-form (my/org-super-agenda-get-file-title-or-parent item))

(defun my/org-agenda-inventory ()
  "Open Org Agenda in the todos view mode with super agenda. Use file title as groups"
  (interactive)
  (let ((org-super-agenda-groups '((:auto-parent t)))
        (org-agenda-sorting-strategy '((todo priority-down category-keep))))
    (org-agenda nil "t")
    (setq org-super-agenda-groups '())))

;; Org-roam configuration
(use-package org-roam
  :after org
  :ensure t
  :config
  (progn ;; Directories configuration
    (setq org-roam-directory org-directory)
    (setq org-roam-dailies-directory (concat org-directory "dailies/"))
    ;; Exclude gpg encrypted files from being processed by org-roam
    (setq org-roam-file-exclude-regexp "\\(\\.gpg\\)$"))

  (progn ;; Appearance configuration
    ;; Setup preview of org-roam nodes
    (setq org-roam-node-display-template
          (concat "${title:*} "
                  (propertize "${tags:30}" 'face 'org-tag))))

  (progn ;; Setup
    (org-roam-setup))
  )

;; Org-roam insert functions
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

;; Org-alert functions
(require 'alert)

(defun alert-android-notifications-notify (info)
  (unless (eq system-type 'android)
    (error "Android notifications are only supported on Android systems"))
  
  "Send INFO using android-notifications-notify."
  (let ((title (or (plist-get info :title) "Org Alert Reminder"))
        (body (or (plist-get info :message) ""))
        (urgency (let ((severity (plist-get info :severity)))
                   (cond ((eq severity 'urgent) 'critical)
                         ((eq severity 'high) 'critical)
                         ((eq severity 'moderate) 'normal)
                         ((eq severity 'low) 'low)
                         ((eq severity 'trivial) 'low)
                         (t 'normal))))
        (icon (or (plist-get info :icon) alert-default-icon)))
    (android-notifications-notify
     :title title
     :body body
     :urgency urgency
     :icon icon
     )))

(alert-define-style 'android-notifications :title "Android Notifications"
                    :notifier #'alert-android-notifications-notify
                    )

;; Org-alert configuration
(use-package org-alert
  :ensure t
  :after org
  :custom
  ;; Use different backends depending on the platform
  (alert-default-style (if (eq system-type 'android)
                           'android-notifications
                         'libnotify))
  :config
  (progn ;; Setup
    (setq org-alert-interval 300
          org-alert-notification-title "Org Alert Reminder")
    (org-alert-enable)
    )
  )

;; Org-tempo configuration
(use-package org-tempo
  :after org
  )

;; Org-analyzer configuration
(use-package org-analyzer
  :after org
  :ensure t
  :config
  (progn ;; Directories configuration
    (setq org-analyzer-org-directory org-directory))
  )

;; Websocket configuration
(use-package websocket
  :after org-roam
  :ensure t
  )

;; Org-roam-ui configuration
(use-package org-roam-ui
  :after org-roam
  :ensure t
  )

;; Org-transclusion configuration
(use-package org-transclusion
  :after org
  :ensure t
  )

;; Org-download configuration
(use-package org-download
  :ensure t
  :after org
  :config
  (progn ;; Directory setup
    (setq org-download-method 'directory)
    (setq-default org-download-image-dir (concat my-notes-directory "images"))
    (setq-default org-download-heading-lvl nil)
    (setq org-download-image-org-width 600)
    (setq org-download-link-format "[[file:%s]]\n"
          org-download-abbreviate-filename-function #'file-relative-name)
    (setq org-download-link-format-function #'org-download-link-format-function-default))
  )

;; Org-download functions
(defun my/org-download-clipboard-prompt-for-name-advice (orig-fun &optional basename)
  "Advice to prompt for a basename before calling `org-download-clipboard'."
  (message "Calling advice function")
  (let ((name (if (called-interactively-p 'any)
                  (read-string "Enter image name (without extension): ")
                basename)))
    (funcall orig-fun (if (string-empty-p name) basename (concat name ".png")))))

(advice-add 'org-download-clipboard :around #'my/org-download-clipboard-prompt-for-name-advice)

;; Org-ref configuration
(use-package org-ref
  :ensure t
  :after org
  )

;; Org-noter configuration
(use-package org-noter
  :ensure t  
  :after org 
  :config
  (progn ;; Directory configuration
    ;; Set the location of the notes
    (setq org-noter-notes-search-path '(org-directory)))
  )

;; Org-media-note configuration
(use-package org-media-note
  :quelpa (org-media-note :fetcher github :repo "yuchen-lea/org-media-note")
  :hook (org-mode .  org-media-note-mode)
  :bind (("H-v" . org-media-note-hydra/body))
  :config
  (progn ;; Directory configuration
    (setq org-media-note-screenshot-image-dir (concat my-notes-directory "images")))
  )

;; Org-media-note configuration
(defun my/remove-invalid-characters-from-filename (filename)
  "Remove invalid characters from FILENAME in order for it to sync to Android folders using syncthing."
  (replace-regexp-in-string "[/*\":<>?|]" "" filename))

(defun my/org-media-note-insert-screenshot-check-filename-advice (orig-func &rest args)
  "Advice to sanitize screenshot filename in `org-media-note-insert-screenshot'."
  (let* ((old-func (symbol-function 'org-media-note--format-picture-file-name))
         (new-func (lambda (orig-name)
                     (my/remove-invalid-characters-from-filename (funcall old-func orig-name)))))
    (cl-letf (((symbol-function 'org-media-note--format-picture-file-name) new-func))
      (apply orig-func args))))

(advice-add 'org-media-note-insert-screenshot :around #'my/org-media-note-insert-screenshot-check-filename-advice)

(provide 'org-config)

;;; org-config.el ends here
