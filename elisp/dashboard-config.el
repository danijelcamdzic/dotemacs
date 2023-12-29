;;; dashboard-config.el -- Dashboard package configuration

;;; Code:

;; Dependencies
(require 'package-archive-config)       ; Melpa and use-package setup
(require 'user-config)                  ; User name and directories

;; Dashboard configuration
(use-package dashboard
  :ensure t
  :config
  (progn ;; Appearance configuration
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
    (add-to-list 'dashboard-items '(agenda) t))

  (progn ;; Setup
    (dashboard-setup-startup-hook))
  )

;; Dashboard functions
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
      (cond ((eq day-difference 0) "today    ")
            ((eq day-difference 1) "yesterday")
            ((eq day-difference -1) "tomorrow ")))))

;; Add advice to change the date format to 'yesterday', 'today' or 'tomorrow'
(advice-add 'dashboard-agenda--formatted-time :around #'my/dashboard-agenda--formatted-time-advice-use-relative-days)


(provide 'dashboard-config)

;;; dashboard-config.el ends here
