;;; package-manager-config.el -- Package manager configuration (melpa and quelpa)

;;; Code:

;; Melpa configuration
;; Add melpa package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Use-package configuration
;; Ensure that use-package is installed and loaded
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Quelpa configuration
(use-package quelpa
  :ensure t
  :init
  ;; Disable self upgrades to reduce startup time
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-update-melpa-p nil)
  )

;; Quelpa-use-package configuration
(use-package quelpa-use-package
  :ensure t
  :after (quelpa)
  :config
  ;; Activate quelpa-use-package
  (quelpa-use-package-activate-advice)
  )


(provide 'package-manager-config)

;;; package-manager-config.el ends here
