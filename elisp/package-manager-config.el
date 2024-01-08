;;; package-manager-config.el -- Package manager configuration

;;; Code:
(provide 'package-manager-config)

;;; Dependencies
(require 'package)

;;; Melpa
;;;; Configuration
;; Add melpa package archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Temporarily disable signature checks
(setq package-check-signature nil)

;; Initialize packages
(package-initialize)

;;; Use-package
;;;; Configuration
;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;; Quelpa
;;;; Configuration
(use-package quelpa
  :ensure t
  :init
  ;; Disable self upgrades to reduce startup time
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-update-melpa-p nil)
  )

;;; Quelpa-use-package
;;;; Configuration
(use-package quelpa-use-package
  :ensure t
  :after (quelpa)
  :config
  ;; Activate quelpa-use-package
  (quelpa-use-package-activate-advice)
  )

;;; package-manager-config.el ends here
