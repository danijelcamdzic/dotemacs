;;; early-init.el --- Pre-init script to setup Emacs to use Termux packages on Android

;;; Code:

;;; Emacs/Termux Android Collab
;; Set up path so Termux-installed packages can be used by Emacs on Android
(setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		               (getenv "PATH")))
(setenv "LD_LIBRARY_PATH" (format "%s:%s"
				                  "/data/data/com.termux/files/usr/lib"
				                  (getenv "LD_LIBRARY_PATH")))
(push "/data/data/com.termux/files/usr/bin" exec-path)

;;; early-init.el ends here
