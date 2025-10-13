;;; dc-gui.el --- Graphical user interface configuration

;;; Code:

(defun dc/gui-hide-all-bars ()
  "Disable scroll bar, menu bar, and tool bar."
  (interactive)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(defun dc/gui-show-all-bars ()
  "Enable scroll bar, menu bar, and tool bar."
  (interactive)
  (scroll-bar-mode 1)
  (menu-bar-mode 1)
  (tool-bar-mode 1))

(defun dc/gui-scrolless-mode ()
  "Disable scroll bar."
  (interactive)
  (scroll-bar-mode -1))

;; Start Emacs without scroll bar
(dc/gui-scrolless-mode)

;; Add keybinding menu for GUI
(define-prefix-command 'dc-gui-map)
(global-set-key (kbd "C-c G") 'dc-gui-map)

;; Add keybindings
(define-key dc-gui-map (kbd "a") 'dc/gui-show-all-bars)
(define-key dc-gui-map (kbd "h") 'dc/gui-hide-all-bars)
(define-key dc-gui-map (kbd "s") 'dc/gui-scrolless-mode)



;; Provide package
(provide 'dc-gui)

;;; dc-gui.el ends here
