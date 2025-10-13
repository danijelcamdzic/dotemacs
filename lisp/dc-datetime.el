;;; dc-datetime.el --- Date-time configuration

;;; Code:

;;                   -------------------------
;;                        Package: calendar  
;;                   -------------------------

(use-package calendar
  :ensure nil
  :config
  ;; Set calendar to start on Monday
  (setq calendar-week-start-day 1)
  )



;;                   -------------------------
;;                      Package: time-stamp  
;;                   -------------------------

(use-package time-stamp
  :ensure t
  :config
  ;; Set up time-stamp format
  (setq time-stamp-format "%Y-%m-%d %H:%M"
        time-stamp-start "# Edited: "
        time-stamp-end "$")

  ;; Add hook to save time-stamp string on every file save
  (add-hook 'before-save-hook 'time-stamp)
  )



;; Provide package
(provide 'dc-datetime)

;;; dc-datetime.el ends here
