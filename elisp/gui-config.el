;;; gui-config.el -- GUI configuration

;;; Code:
(provide 'gui-config)

;;; Dependencies
(require 'package-manager-config)
(require 'org-config)
(require 'bookmarks-config)
(require 'authorization-config)

;;; Functions: GUI Display Show/Hide
(defun my/gui-hide-bars ()
  "Disable scroll bar, menu bar, and tool bar."
  (interactive)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(defun my/gui-show-bars ()
  "Enable scroll bar, menu bar, and tool bar."
  (interactive)
  (scroll-bar-mode 1)
  (menu-bar-mode 1)
  (tool-bar-mode 1))

;; Hide GUI on startup if in LINUX mode
(when (eq system-type 'gnu/linux)
  (my/gui-hide-bars))

;;; GUI ToolBar Items
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

;;; Easymenu
;;;; Configuration
(use-package easymenu
  :config
    ;; Define "Commands" menu
    (easy-menu-define my/commands-center-menu nil "My Commands Menu"
      '("Commands"
        ("Org"
         ("Schedule"
          ["Add" my/org-add-schedule t]
          ["Remove" my/org-remove-schedule t])
         ("States"
          ["Log as TODO" my/org-log-todo t]
          ["Log as DOING" my/org-log-doing t]
          ["Log as DONE" my/org-log-done t]
          ["Log as SKIP" my/org-log-skip t]
          ["Log as FAIL" my/log-log-fail t]
          ["Change State" my/org-change-state t]
          ["Skip all Overdue" my/org-skip-all-overdue-tasks t]
          ["Show States in Calendar" my/org-logbook-display-states-on-calendar t])
         ("Clock"
          ["Clock In" my/org-clock-in t]
          ["Clock Out" my/org-clock-out t]
          ["Display Clocks" org-clock-display]
          ["Show Clock Analysis" org-analyzer-start t])
         ("Notes"
          ["Add Note" my/org-add-note t]
          ["Show Notes in Calendar" my/org-logbook-display-notes-on-calendar t]))
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

    ;; Add the commands center menu to the toolbar to the left of "Tools" section
    (easy-menu-add-item global-map '("menu-bar") my/commands-center-menu "Tools")

    ;; Define "Authentication" menu
    (easy-menu-define my-auth-menu nil "My Authentication Menu"
      '("Auth-Sources"
        ["Get TOTP" my/totp-display t]
        ["Get Password" my/pass-display t]))

    ;; Add the "Authentication" menu to the "Tools" menu
    (easy-menu-add-item nil '("tools") my-auth-menu "Games")

    ;; Add a line separator after the "Authentication" menu
    (easy-menu-add-item nil '("tools") "--" "Games")
  )

;;; gui-config.el ends here
