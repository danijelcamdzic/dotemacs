;;; dc-bookmarks.el --- Bookmarks configuration

;;; Code:

;;                   -------------------------
;;                          Package: eww  
;;                   -------------------------

(use-package eww
  :ensure nil
  :config
  ;; Set default eww-bookmarks directory
  (setq eww-bookmarks-directory dc-notes-directory)
  )



;;                   -------------------------
;;                       Package: bookmark  
;;                   -------------------------

(use-package bookmark
  :ensure nil
  :config
  ;; Set default bookmark file
  (setq bookmark-default-file (concat dc-notes-directory "bookmarks"))
  )

;; Add keybinding menu for bookmarks
(define-prefix-command 'dc-bookmark-map)
(define-key dc-buffer-map (kbd "b") 'dc-bookmark-map)

;; Add keybindings
(define-key dc-bookmark-map (kbd "l") 'list-bookmarks)
(define-key dc-bookmark-map (kbd "s") 'bookmark-set)



;;                   -------------------------
;;                       Package: bookmark+
;;                   -------------------------

(use-package bookmark+
  :config
  ;; Set default .emacs-bmk-bmenu-state.el file path
  (setq bmkp-bmenu-state-file (expand-file-name ".emacs-bmk-bmenu-state.el" user-emacs-directory))
  
  ;; Set default .emacs-bmk-bmenu-commands.el file path
  (setq bmkp-bmenu-commands-file (expand-file-name ".emacs-bmk-bmenu-commands.el" user-emacs-directory))
  )



;; Provide package
(provide 'dc-bookmarks)

;;; dc-bookmarks.el ends here
