;;; time-config.el -- Calendar and other time related settings

;;; Code:

;; Dependencies
(require 'package-archive-config)       ; Melpa and use-package setup
(require 'user-config)                  ; User name and directories

;; Time adjustment functions
(defvar my/adjusted-time nil
  "Adjusted time for the duration of the my/skip-overdue-tasks function.")

(defvar my/time-override-lock nil
  "Lock to prevent concurrent access to the time override.")

(defun my/adjust-time (time)
  "Temporarily adjust `current-time' to the given TIME."
  (setq my/adjusted-time (append (org-read-date nil t time) '(0 0))))

(defun my/current-time-override ()
  "Override for `current-time' using `my/adjusted-time'."
  (or my/adjusted-time (current-time)))

;; Calendar configuration
(use-package calendar
  :config
  (progn ;; Appearance configuration
    ;; Set calendar to start on Monday
    (setq calendar-week-start-day 1))
  )


(provide 'time-config)

;;; time-config.el ends here
