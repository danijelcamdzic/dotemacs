;;; notifications-config.el -- Notifications configuration

;;; Code:
(provide 'notifications-config)

;;; Dependencies
(require 'package-manager-config)

;;; Alert
;;;; Configuration
(use-package alert
  :ensure t
  :config
  ;; Setup default icon for Android notifications
  (when (eq system-type 'android)
    ;; android.R.drawable icons must be used
    (setq alert-default-icon "ic_popup_reminder"))
  )

;;;; Functions - Android Notifications
(defun my/alert-android-notifications-notify (info)
  "Send notifications using `android-notifications-notify'.
`android-notifications-notify' is a built-in function in the native Emacs
Android port."
  (let ((title (or (plist-get info :title) "Android Notifications Alert"))
        (body (or (plist-get info :message) ""))
        (urgency (cdr (assq (plist-get info :severity)
                            alert-notifications-priorities)))
        (icon (or (plist-get info :icon) alert-default-icon))
        (replaces-id (gethash (plist-get info :id) alert-notifications-ids)))
    (android-notifications-notify
     :title title
     :body body
     :urgency urgency
     :icon icon
     :replaces-id replaces-id)))

(alert-define-style 'android-notifications :title "Android Notifications"
                    :notifier #'my/alert-android-notifications-notify)

;;; notifications-config.el ends here
