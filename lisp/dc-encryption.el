;;; dc-encryption.el --- Encryption configuration

;;; Code:

;;                   -------------------------
;;                          Package: epa  
;;                   -------------------------

(use-package epa
  :ensure nil
  :config
  ;; Set the gpg default program
  (setq epg-gpg-program "gpg2")

  ;; Enable the EPA file encryption/decryption features
  (epa-file-enable)

  ;; Set to nil to disable the key selection dialog
  ;; Emacs will use the default GPG key automatically
  (setq epa-file-select-keys nil)

  ;; Set the pinentry mode to loopback, allowing Emacs to
  ;; prompt for passphrases in the minibuffer
  ;; This is useful when running Emacs in a terminal or
  ;; environment where GUI pinentry dialogs are not available
  (setq epa-pinentry-mode 'loopback)
  )



;; Provide package
(provide 'dc-encryption)

;;; dc-encryption.el ends here
