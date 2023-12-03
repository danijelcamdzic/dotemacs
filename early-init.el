(setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		       (getenv "PATH")))
(setenv "LD_LIBRARY_PATH" (format "%s:%s"
				  "/data/data/com.termux/files/usr/lib"
				  (getenv "LD_LIBRARY_PATH")))
(push "/data/data/com.termux/files/usr/bin" exec-path)
