;;; time-management-config.el -- Calendar and other time related configuration

;;; Code:
(provide 'time-management-config)

;;; Dependencies
(require 'package-manager-config)

;;; Functions - Relative Date
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

;;; Functions - Time Adjustment
(defvar my-adjusted-time nil
  "Adjusted time. This time will replace current time.")

(defvar my-override-lock nil
  "Lock to prevent concurrent access to the time override.")

(defun my/time-adjust-time (time)
  "Temporarily adjust `current-time' to the given TIME."
  (setq my-adjusted-time (append (org-read-date nil t time) '(0 0))))

(defun my/time-override-current-time ()
  "Override for `current-time' using `my/time-adjust-time'."
  (or my/time-adjust-time (current-time)))

;;; Time-stamp
;;;; Configuration
(use-package time-stamp
  :config
  ;; Set up time-stamp format
  (setq time-stamp-format "%Y-%m-%d %H:%M"
        time-stamp-start "# Edited: "
        time-stamp-end "$")

  ;; Add hook to save time-stamp string on every file save
  (add-hook 'before-save-hook 'time-stamp)
  )

;;; Calendar
;;;; Configuration
(use-package calendar
  :config
  ;; Set calendar to start on Monday
  (setq calendar-week-start-day 1)
  )

;;; time-management-config.el ends here
