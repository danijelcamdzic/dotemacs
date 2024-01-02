;;; time-management-config.el -- Calendar and other time related configuration

;;; Code:

;; Dependencies
(require 'package-manager-config)       ; Package manager configuration (melpa and quelpa)

;; Time adjustment functions
(defvar my/adjusted-time nil
  "Adjusted time. This time will replace current time.")

(defvar my/time-override-lock nil
  "Lock to prevent concurrent access to the time override.")

(defun my/adjust-time (time)
  "Temporarily adjust `current-time' to the given TIME."
  (setq my/adjusted-time (append (org-read-date nil t time) '(0 0))))

(defun my/current-time-override ()
  "Override for `current-time' using `my/adjusted-time'."
  (or my/adjusted-time (current-time)))

;; Time-stamp configuration
(use-package time-stamp
  :config
  (progn ;; Setup
    (setq time-stamp-format "%Y-%m-%d %H:%M"
          time-stamp-start "# Edited: "
          time-stamp-end "$")
    (add-hook 'before-save-hook 'time-stamp))
  )

;; Calendar configuration
(use-package calendar
  :config
  (progn ;; Appearance configuration
    ;; Set calendar to start on Monday
    (setq calendar-week-start-day 1))
  )

(provide 'time-management-config)

;;; time-management-config.el ends here
