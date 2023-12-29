;;; gui-config.el -- GUI configuration

;;; Code:

;; Dependencies
(require 'package-archive-config)       ; Melpa and use-package setup
(require 'user-config)                  ; User name and directories
(require 'org-config)                   ; Org and supporting/extending packages configuration
(require 'bookmarks-config)             ; Eww and regular bookmarks configuration
(require 'authorization-config)         ; GnuPg and auth-sources configuration
(require 'ai-config)                    ; AI tools configuration

;; GUI mode functions
(defun my/hide-gui-bar ()
  "Disable scroll bar, menu bar, and tool bar."
  (interactive)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(defun my/show-gui-bar ()
  "Enable scroll bar, menu bar, and tool bar."
  (interactive)
  (scroll-bar-mode 1)
  (menu-bar-mode 1)
  (tool-bar-mode 1))

;; Hide GUI on startup if in LINUX mode
(when (eq system-type 'gnu/linux)
  (my/hide-gui-bar))

;; Add additional GUI options when Android system is detected
(when (eq system-type 'android)
  ;; Add tool-bar options for zooming in
  (tool-bar-add-item "zoom-in" 'text-scale-increase
                     'text-scale-increase
                     :help "Zoom In")

  ;; Add tool-bar options for zooming out
  (tool-bar-add-item "zoom-out" 'text-scale-decrease
                     'text-scale-decrease
                     :help "Zoom Out")

  ;; Add tool-bar option for Org Cycle
  (tool-bar-add-item "right-arrow" 'org-cycle
                     'org-cycle
                     :help "Org Cycle")

  ;; Add tool-bar option for Org Ctrl-C Ctrl-C
  (tool-bar-add-item "prev-node" 'org-ctrl-c-ctrl-c
                     'org-ctrl-c-ctrl-c
                     :help "Execute Org Ctrl-C Ctrl-C")
  )

;; Easymenu configuration
(use-package easymenu
  :config
  (progn ;; Menu configuration
    ;; Define "Commands" menu
    (easy-menu-define my/command-center-menu nil "My Commands Menu"
      '("Command"
        ("Agenda"
         ["Day View" my/org-agenda-day-view t]
         ["Week View" my/org-agenda-week-view t]
         ["Year View" my/org-agenda-year-view t]
         ["Inventory View" my/org-agenda-inventory t])
        ("Roam"
         ("Dailies"
          ["Go to Today" org-roam-dailies-goto-today t]
          ["Find Date" org-roam-dailies-find-date t])
         ("Nodes"
          ["Find Node" org-roam-node-find t]
          ["Open Graph" org-roam-ui-open t]))
        ("Schedule"
         ["Add" my/add-schedule t]
         ["Remove" my/remove-schedule t])
        ("States"
         ["Log as TODO" my/todo-log-todo t]
         ["Log as DONE" my/todo-log-done t]
         ["Log as SKIP" my/todo-log-skip t]
         ["Log as FAIL" my/todo-log-fail t]
         ["Change State" my/todo-change-state t]
         ["Automatically SKIP Overdues" my/skip-overdue-tasks]
         ["Show in Calendar" my/show-states-in-calendar t])
        ("Clock"
         ["Clock In" my/clock-in t]
         ["Clock Out" my/clock-out t]
         ["Display Clocks" org-clock-display]
         ["Show Clock Analysis" org-analyzer-start t])
        ("Note"
         ["Add Note" my/add-note t]
         ["Show in Calendar" my/show-notes-in-calendar t])
        ("Bookmarks"
         ["List Bookmarks" list-bookmarks t]
         ["Show All Bookmarks" bmkp-bmenu-show-all t]
         ["Show URL Bookmarks" bmkp-bmenu-show-only-url-bookmarks t]
         ["Filter Bookmarks by Name" bmkp-bmenu-filter-bookmark-name-incrementally t]
         ["Filter Bookmarks By Tag" bmkp-bmenu-filter-bookmark-tags-incrementally t]
         ["Filter Bookmarks By Annotation" bmkp-bmenu-filter-annotation-incrementally t])))

    ;; Add the command center menu to the toolbar to the left of "Tools" section
    (easy-menu-add-item global-map '("menu-bar") my/command-center-menu "Tools")

    ;; Define "Authentication" menu
    (easy-menu-define my-auth-menu nil "My Authentication Menu"
      '("Authentication"
        ("TOTP"
         ["Generate TOTP token" my/totp-display t])
        ("Passwords" 
         ["Reveal Password" my/pass-display t])))

    ;; Add the "Authentication" menu to the "Tools" menu
    (easy-menu-add-item nil '("tools") my-auth-menu "Games")

    ;; Define "AI" menu
    (easy-menu-define my-ai-menu nil "My AI Menu"
      '("AI"
        ("OpenAI"
         ["Open ChatGPT Shell" my/open-chatgpt-shell t])))

    ;; Add the "AI" menu to the "Tools" menu
    (easy-menu-add-item nil '("tools") my-ai-menu "Games")

    ;; Remove "Games" from the "Tools" menu
    (define-key global-map [menu-bar tools games] nil))
  )


(provide 'gui-config)

;;; gui-config.el ends here
