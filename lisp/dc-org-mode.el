;;; dc-org-mode.el --- Org-mode configuration

;;; Code:

;;                   -------------------------
;;                          Package: org  
;;                   -------------------------

(use-package org
  :ensure nil
  :config
  ;; Directory and startup settings
  (setq org-directory dc-journal-directory)
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
   '(org-tag ((t (:foreground "#008B8B" :weight bold)))))

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

(defun dc/insert-custom-clock-entry ()
  (interactive)
  (insert "CLOCK: ")
  (org-time-stamp-inactive)
  (insert "--")
  ;; Inserts the current time by default.
  (let ((current-prefix-arg '(4))) (call-interactively 'org-time-stamp-inactive))
  (org-ctrl-c-ctrl-c))

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
;;                       Package: org-yank 
;;                   -------------------------

;; This is a new addition to the Emacs 29.

;; Register yank-media handlers
(with-eval-after-load 'org
  (setq yank-media--registered-handlers '(("image/.*" . #'dc/org-mode--image-yank-handler))))

(yank-media-handler "image/.*" #'dc/org-mode--image-yank-handler)

(defun dc/org-mode--image-yank-handler (type image)
  "Save IMAGE of TYPE into the Org heading's attachment directory.
Prompt for a basename, prepend a timestamp, then use .png as the extension."
  (let* ((basename (read-string "Enter image name (without extension): "))
         (timestamp (format-time-string "%Y-%m-%d_%H-%M-%S"))
         ;; If user presses ENTER with no basename, just use timestamp.png
         (final-name (if (string-empty-p basename)
                         (concat timestamp ".png")
                       (concat timestamp "_" basename ".png")))
         (attach-dir (org-attach-dir t))               
         (filename (expand-file-name final-name attach-dir)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert image)
      (write-region (point-min) (point-max) filename))
    (org-attach-sync)
    (insert (format "[[attachment:%s]]\n"
                    (file-relative-name filename attach-dir)))))

;; Add keybindings
(define-key dc-org-map (kbd "p") 'yank-media)



;;                   -------------------------
;;                      Package: org-tempo  
;;                   -------------------------

(use-package org-tempo
  :after org
  )



;; Provide package
(provide 'dc-org-mode)

;;; dc-org-mode.el ends here
