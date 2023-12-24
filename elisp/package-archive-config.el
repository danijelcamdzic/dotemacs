;;; package-archive-config.el -- Melpa, Quelpa and use-package setup

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
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-update-melpa-p nil)
  )

;; Quelpa-use-package configuration
(use-package quelpa-use-package
  :ensure t
  :after (quelpa)
  :config
  (quelpa-use-package-activate-advice)
  )


(provide 'package-archive-config)

;;; package-archive-config.el ends here
