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
(setq dashboard-agenda-sort-strategy  '((agenda time-up priority-down category-keep)
                                        (todo time-up priority-down category-keep)
                                        (tags time-up priority-down category-keep)
                                        (search time-up priority-down category-keep)))
(add-to-list 'dashboard-items '(agenda) t)
(dashboard-setup-startup-hook)

;; Provide package for use
(provide 'editor-config)

;; editor-config.el ends here
