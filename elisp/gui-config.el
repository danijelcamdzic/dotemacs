;;; gui-config.el
;;
;; Author: Danijel Camdzic
;; Maintainer: Danijel Camdzic <danijelcamdzic@tuta.com>
;;
;; License: NO LICENCE
;;
;;; Code:

;;; --------- GUI menu ---------
;; Add tool-bar options for zooming in
(tool-bar-add-item "zoom-in" 'text-scale-increase
                   'text-scale-increase
                   :help "Zoom In")

;; Add tool-bar options for zooming out
(tool-bar-add-item "zoom-out" 'text-scale-decrease
                   'text-scale-decrease
                   :help "Zoom Out")

(require 'easymenu)

;; Add a toolbar falling menu for easy access to functions
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
      ["Go to Date" org-roam-dailies-find-date t])
     ("Nodes"
      ["Find Node" org-roam-node-find t]
      ["Open Graph" org-roam-ui-open t]))
    ("Schedule"
     ["Add" my/add-schedule t]
     ["Remove" my/remove-schedule t])
    ("States"
     ["Log as DONE" my/todo-log-done t]
     ["Log as SKIP" my/todo-log-skip t]
     ["Log as FAIL" my/todo-log-fail t]
     ["Change State" my/todo-change-state t]
     ["Show in Calendar" my/show-todo-in-calendar t])
    ("Clock"
     ["Clock In" my/clock-in t]
     ["Clock Out" my/clock-out t]
     ["Display Clocks" org-clock-display]
     ["Show Clock Analysis" org-analyzer-start t])
    ("Note"
     ["Add Note" my/add-note t])
    ("Insert"
     ["Insert Date and Time" my/insert-current-date-time t]
     ["Insert Node" org-roam-node-insert t]
     ["Insert Nodes with Tags" my/insert-org-roam-nodes-by-tag t])))

;; Add the menu option to the toolbas to the left of "Tools" section
(easy-menu-add-item global-map '("menu-bar") my/command-center-menu "Tools")

;; Define the "AI" menu
(easy-menu-define my-ai-menu nil "My AI Menu"
  '("AI"
    ("OpenAI"
     ["Open ChatGPT Shell" my/open-chatgpt-shell t])))

;; Add the "AI" menu to the "Tools" menu, before "Games"
(easy-menu-add-item nil '("tools") my-ai-menu "Games")
(easy-menu-add-item nil '("tools") '("--") "Games")

;; Provide package for use
(provide 'gui-config)

;; gui-config.el ends here
