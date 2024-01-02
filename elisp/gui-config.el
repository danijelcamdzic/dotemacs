;;; gui-config.el -- GUI configuration

;;; Code:

;; Dependencies
(require 'user-config)                  ; User details and directory configuration
(require 'package-manager-config)       ; Package manager configuration (melpa and quelpa)
(require 'org-config)                   ; Org and supporting/extending packages configuration
(require 'bookmarks-config)             ; Bookmarks configuration
(require 'authorization-config)         ; GnuPg and auth-sources configuration
(require 'gpt-config)                   ; GPT tools configuration

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
    ;; Define "Command" menu
    (easy-menu-define my/command-center-menu nil "My Commands Menu"
      '("Command"
        ("Org-Mode"
         ("Schedule"
          ["Add" my/add-schedule t]
          ["Remove" my/remove-schedule t])
         ("States"
          ["Log as TODO" my/todo-log-todo t]
          ["Log as DOING" my/todo-log-doing t]
          ["Log as DONE" my/todo-log-done t]
          ["Log as SKIP" my/todo-log-skip t]
          ["Log as FAIL" my/todo-log-fail t]
          ["Change State" my/todo-change-state t]
          ["Skip all Overdue" my/skip-overdue-tasks]
          ["Show States in Calendar" my/show-states-in-calendar t])
         ("Clock"
          ["Clock In" my/clock-in t]
          ["Clock Out" my/clock-out t]
          ["Display Clocks" org-clock-display]
          ["Show Clock Analysis" org-analyzer-start t])
         ("Notes"
          ["Add Note" my/add-note t]
          ["Show Notes in Calendar" my/show-notes-in-calendar t]))
        ("Org-Agenda"
         ["Day View" my/org-agenda-day-view t]
         ["Week View" my/org-agenda-week-view t]
         ["Year View" my/org-agenda-year-view t]
         ["Inventory View" my/org-agenda-inventory t])
        ("Org-Roam"
         ("Dailies"
          ["Go to Today" org-roam-dailies-goto-today t]
          ["Find Date" org-roam-dailies-find-date t])
         ("Nodes"
          ["Find Node" org-roam-node-find t]
          ["Open Graph" org-roam-ui-open t]))
        ("Bookmarks"
         ["List Bookmarks" list-bookmarks t])))

    ;; Add the command center menu to the toolbar to the left of "Tools" section
    (easy-menu-add-item global-map '("menu-bar") my/command-center-menu "Tools")

    ;; Define "Authentication" menu
    (easy-menu-define my-auth-menu nil "My Authentication Menu"
      '("Auth-Sources"
        ["Get TOTP" my/totp-display t]
        ["Get Password" my/pass-display t]))

    ;; Add the "Authentication" menu to the "Tools" menu
    (easy-menu-add-item nil '("tools") my-auth-menu "Games")

    ;; Add a menu separator after the "Authentication" menu
    (easy-menu-add-item nil '("tools") "--" "Games")

    ;; Define "GPT" menu
    (easy-menu-define my-gpt-menu nil "My GPT Menu"
      '("GPT"
        ["Set API Key" my/set-gptel-openai-api-key t]))

    ;; Add the "GPT" menu to the "Tools" menu
    (easy-menu-add-item nil '("tools") my-gpt-menu "Games"))
  )


(provide 'gui-config)

;;; gui-config.el ends here
