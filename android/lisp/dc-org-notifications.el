;;; dc-org-notifications.el --- Org notifications configuration

;;; Code:

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



;; Provide package
(provide 'dc-org-notifications)

;;; dc-org-notifications.el ends here
