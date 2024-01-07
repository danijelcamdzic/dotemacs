;;; authorization-config.el -- GnuPg and auth-sources configuration

;;; Code:
(provide 'authorization-config)

;;; Dependencies
(require 'user-config)
(require 'package-manager-config)
(require 'bindat)
(require 'gnutls)
(require 'hexl)

;;; Epa
;;;; Configuration
(use-package epa
  :ensure t
  :config
  ;; Set the environment variable and configure EPA only if running on Android
  (when (eq system-type 'android)
    ;; Set the environment variable to use GPG on Termux
    (setenv "GNUPGHOME" "/data/data/com.termux/files/home/.gnupg"))

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

;;; Auth-source
;;;; Configuration
(use-package auth-source
  :ensure t
  :config
  ;; Set auth-sources files
  (setq auth-sources
        `((:source ,(concat my-documents-directory ".secrets/.authinfo-pass.gpg"))
          (:source ,(concat my-documents-directory ".secrets/.authinfo-totp.gpg"))))

  ;; Enable authinfo-mode for auth-source files
  (add-to-list 'auto-mode-alist '("\\.authinfo.*\\.gpg\\'" . authinfo-mode))

  ;; Clear cached passwords after buffers are switched
  (add-hook 'buffer-list-update-hook 'auth-source-forget-all-cached)
  )

;;;; Functions - TOTP
(defun totp--base32-char-to-n (char)
  "Return 5 bit integer value matching base32 CHAR."
  (cond ((<= ?A char ?Z) (- char ?A))
        ((<= ?a char ?z) (- char ?a))
        ((<= ?2 char ?7) (+ (- char ?2) 26))
        (t (error "Invalid number range"))))

(defun totp--base32-to-number (string)
  "Base32-decode STRING and return the result as number.
    Handles interleaved whitespaces and missing padding charachters
    gracefuly (The number of padding chars can be deduced from input
    length)."
  (let* ((s (replace-regexp-in-string "\\([[:space:]]\\|=*$\\)" "" string))
         (ntrail (mod (* 5  (length s)) 8)))
    (ash (seq-reduce (lambda (acc char)
                       (+ (ash acc 5) (totp--base32-char-to-n char)))
                     s 0) (- ntrail))))

(defun totp--hex-decode-string (string)
  "Hex-decode STRING and return the result as a unibyte string."
  (apply #'unibyte-string
         (seq-map (lambda (s) (hexl-htoi (aref s 0) (aref s 1)))
                  (seq-partition string 2))))

(defun totp (string &optional time digits)
  "Return a TOTP token using the secret STRING and current time.
    TIME is used as counter value instead of current time, if non-nil.
    DIGITS is tre  number of pin digits and defaults to 6."
  (let* ((hex-string (if (string-match-p "^[0-9a-fA-F]\\{2\\}+$" string)
                         string		;already in hex format
                       (format "%X" (totp--base32-to-number string))))
         (key-bytes (totp--hex-decode-string (upcase hex-string)))
         (counter (truncate (/ (or time (time-to-seconds)) 30)))
         (digits (or digits 6))
         (format-string (format "%%0%dd" digits))
         ;; we have to manually split the 64 bit number (u64 not supported in Emacs 27.2)
         (counter-bytes (bindat-pack  '((:high u32) (:low u32))
                                      `((:high . ,(ash counter -32)) (:low . ,(logand counter #xffffffff)))))
         (mac (gnutls-hash-mac 'SHA1 key-bytes counter-bytes))
         (offset (logand (bindat-get-field (bindat-unpack '((:offset u8)) mac 19) :offset) #xf)))
    (format format-string
            (mod
             (logand (bindat-get-field (bindat-unpack '((:totp-pin u32)) mac  offset) :totp-pin)
                     #x7fffffff)
             (expt 10 digits)))))

(defun my/totp-display (auth)
  "Select a TOTP AUTH from `auth-sources', display its TOTP, and show remaining valid time."
  (interactive
   (list
    (let ((candidates (mapcar
                       (lambda (auth)
                         (cons (format "User '%s' on %s"
                                       (propertize (plist-get auth :user) 'face 'font-lock-keyword-face)
                                       (propertize (plist-get auth :host) 'face 'font-lock-string-face))
                               auth))
                       (seq-filter (lambda (auth) (string-prefix-p "TOTP:" (plist-get auth :host)))
                                   (auth-source-search :max 10000)))))
      (cdr (assoc (completing-read "Pick a TOTP> " candidates) candidates)))))
  (let* ((current-time (time-to-seconds))
         (time-step 30)
         (time-remaining (- time-step (mod current-time time-step)))
         (code (totp (funcall (plist-get auth :secret)))))
    (let ((message-log-max nil))
      (message "Your TOTP for '%s' is: %s (valid for %d more seconds)"
               (propertize (plist-get auth :host) 'face 'font-lock-keyword-face)
               (propertize code 'face 'font-lock-string-face)
               time-remaining))
    code))

;;;; Functions - Passwords
(defun my/pass-display (auth)
  "Select a password entry (PASS) from `auth-sources', and briefly display its password."
  (interactive
   (list
    (let ((candidates (mapcar
                       (lambda (auth)
                         (cons (format "User '%s' on %s"
                                       (propertize (plist-get auth :user) 'face 'font-lock-keyword-face)
                                       (propertize (plist-get auth :host) 'face 'font-lock-string-face))
                               auth))
                       (seq-filter (lambda (auth) (string-prefix-p "PASS:" (plist-get auth :host)))
                                   (auth-source-search :max 10000)))))
      (cdr (assoc (completing-read "Pick a PASS entry> " candidates) candidates)))))
  (let ((password (funcall (plist-get auth :secret))))
    ;; Temporarily disable logging in *Messages* buffer
    (let ((message-log-max nil))
      (message "Your password for '%s' is: %s"
               (propertize (plist-get auth :host) 'face 'font-lock-keyword-face)
               (propertize password 'face 'font-lock-string-face)))))

;;; auth-config.el ends here
