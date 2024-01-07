;;; dashboard-config.el -- Dashboard configuration

;;; Code:
(provide 'dashboard-config)

;;; Dependencies
(require 'package-manager-config)
(require 'time-management-config)

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

;;; dashboard-config.el ends here
