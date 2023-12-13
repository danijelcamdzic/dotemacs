;;; package-archive-config.el -- Melpa and use-package setup

;;; Code:

;; Melpa
;; Add melpa package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Use-package
;; Ensure that use-package is installed and loaded
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


(provide 'package-archive-config)

;;; package-archive-config.el ends here
