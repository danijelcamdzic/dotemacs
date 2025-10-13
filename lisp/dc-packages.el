;;; dc-packages.el --- All package management setup

;;; Code:

;;                   -------------------------
;;                            Package  
;;                   -------------------------

;; Use package
(require 'package)

;; Disable package signature checks
;; Some packages don't have signatures so
;; installing with Melpa creates problems
;; if package signature check is enabled
(setq package-check-signature nil)

;; Initialize packages
(package-initialize)

;; Add melpa
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)



;;                   -------------------------
;;                          Use-package  
;;                   -------------------------

;; Use use-package
(require 'use-package)



;; Provide package
(provide 'dc-packages)

;;; dc-packages.el ends here
