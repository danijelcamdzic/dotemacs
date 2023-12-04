;;; editor-config.el
;;
;; Author: Danijel Camdzic
;; Maintainer: Danijel Camdzic <danijelcamdzic@tuta.com>
;;
;; License: NO LICENCE
;;
;;; Code:

;;; --------- Text editing and completion ---------
;; Ensure that company package is installed and loaded
(unless (package-installed-p 'company)
  (package-install 'company))
(require 'company)

;; Enable company mode and add hook
(company-mode 1)
(add-hook 'after-init-hook 'global-company-mode)

;; Ensure that vertico package is installed and loaded
(unless (package-installed-p 'vertico)
  (package-install 'vertico))
(require 'vertico)

;; Enable and configure company mode
(vertico-mode 1)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

;; Ensure that orderless package is installed loaded
(unless (package-installed-p 'orderless)
  (package-install 'orderless))
(require 'orderless)

;; Configure text formatting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(global-display-line-numbers-mode 0)

;; Disable backup and lock files
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Open everything in a separate buffer
(setq display-buffer-alist
      '((".*"
         (display-buffer-same-window)
         (inhibit-same-window . nil))))

;; Set touch-screen-display-keyboard if Android is the system
;; type
(cond
 ((eq system-type 'android)
  (setq touch-screen-display-keyboard t)))

;;; --------- Dashboard ---------
;; Ensure that dashboard package is installed and loaded
(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))
(require 'dashboard)

;; Configure dashboard
(setq dashboard-startup-banner 'official)
(setq dashboard-center-content t)
(setq dashboard-banner-logo-title "Welcome to Emacs")
(setq dashboard-items nil)
(setq dashboard-set-footer nil)
(setq dashboard-set-init-info t)
(setq dashboard-set-heading-icons t)
(setq dashboard-agenda-time-string-format "%Y-%m-%d %H:%M")
(add-to-list 'dashboard-items '(agenda) t)

(defun my/dashboard-agenda--formatted-time-advice (orig-fun &rest args)
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
      (cond ((eq day-difference 0) "today")
            ((eq day-difference 1) "yesterday")
            ((eq day-difference -1) "tomorrow")))))

;; Add advice to change the date format to 'yesterday', 'today' or 'tomorrow' if it suits
(advice-add 'dashboard-agenda--formatted-time :around #'my/dashboard-agenda--formatted-time-advice)

;; Set up sorting strategy
(setq dashboard-agenda-sort-strategy  '((agenda time-up priority-down category-keep)
                                        (todo time-up priority-down category-keep)
                                        (tags time-up priority-down category-keep)
                                        (search time-up priority-down category-keep)))
(dashboard-setup-startup-hook)

;; Provide package for use
(provide 'editor-config)

;; editor-config.el ends here
