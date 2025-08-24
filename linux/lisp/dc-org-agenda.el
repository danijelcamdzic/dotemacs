;;; dc-org-agenda.el --- Org-agenda configuration

;;; Code:

;;                   -------------------------
;;                      Package: org-agenda  
;;                   -------------------------

(use-package org-agenda
  :ensure nil
  :after org
  :config  
  ;; Agenda view settings
  (setq org-agenda-span 7)
  (setq org-agenda-show-future-repeats 'next)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-time-grid
        '((daily weekly today require-timed)
          (0000 0200 0400 0600 0800 1000 1200 1400 1600 1800 2000 2200 2359)
          "......" "----------------"))

  ;; Agenda item formatting
  (setq org-agenda-prefix-format  '((agenda . "  %t %c: ")
                                    (todo . "%t ")
                                    (tags . "")
                                    (search . "%i")))
  (setq org-agenda-scheduled-leaders '("" ""))
  
  ;; Agenda sorting strategy
  (setq org-agenda-sorting-strategy '((agenda time-up priority-up category-keep)
                                      (todo priority-up time-up category-keep)
                                      (tags time-up priority-up category-keep)
                                      (search time-up priority-up category-keep)))
  
  ;; Agenda custom faces
  (custom-set-faces
   '(org-scheduled ((t (:foreground "grey"))))
   '(org-scheduled-today ((t (:foreground "white"))))
   '(org-agenda-done ((t (:foreground "grey" :slant italic))))
   '(org-agenda-calendar-event ((t (:foreground "grey" :slant italic)))))
  )

(defun dc/org-agenda--switch-to-view (view-fn)
  "Switch to the given Org Agenda view function VIEW-FN and insert timeline."
  (if (eq major-mode 'org-agenda-mode)
      (progn
        (unless (eq org-agenda-type 'agenda)
          (org-agenda-exit)
          (org-agenda-list))
        (run-with-idle-timer 0.1 nil view-fn))
    (org-agenda-list)
    (run-with-idle-timer 0.1 nil view-fn)))

(defun dc/org-agenda-day-view ()
  "Switch to the Org Agenda daily view from anywhere in Emacs."
  (interactive)
  (dc/org-agenda--switch-to-view 'org-agenda-day-view))

(defun dc/org-agenda-week-view ()
  "Switch to the Org Agenda weekly view from anywhere in Emacs."
  (interactive)
  (dc/org-agenda--switch-to-view 'org-agenda-week-view))

(defun dc/org-agenda-year-view ()
  "Switch to the Org Agenda yearly view from anywhere in Emacs."
  (interactive)
  (dc/org-agenda--switch-to-view 'org-agenda-year-view))

(defun dc/org-agenda-open-logbook-mode ()
  "Open logbook mode in Org Agenda. To see full logbook view manually press
'l + [' on day, week or year agenda views."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (progn
        (org-agenda-log-mode)
        (org-agenda-manipulate-query-add))))

;; Add keybinding menu for agenda
(define-prefix-command 'dc-agenda-map)
(global-set-key (kbd "C-c A") 'dc-agenda-map)

;; Add keybindings
(define-key dc-agenda-map (kbd "d") 'dc/org-agenda-day-view)
(define-key dc-agenda-map (kbd "w") 'dc/org-agenda-week-view)
(define-key dc-agenda-map (kbd "y") 'dc/org-agenda-year-view)

;; Add org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "d") 'dc/org-agenda-day-view)
  (define-key org-agenda-mode-map (kbd "w") 'dc/org-agenda-week-view)
  (define-key org-agenda-mode-map (kbd "y") 'dc/org-agenda-year-view))



;;                   -------------------------
;;                   Package: org-super-agenda  
;;                   -------------------------

(use-package org-super-agenda
  :after org-agenda
  :ensure t
  :config
  ;; Enable org-super-agenda mode
  (org-super-agenda-mode)
  )

;; Redefine the auto-category group of the org-super-agenda
(org-super-agenda--def-auto-group category "their org-category property"
  :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
              (org-get-category))
  :header-form key)

(defun dc/org-agenda-todo-view ()
  "Open Org Agenda in the todos view mode with super agenda. Use category as groups"
  (interactive)
  (let ((org-super-agenda-groups '((:auto-category t)))
        (org-agenda-sorting-strategy '((todo priority-down category-keep))))
    (org-agenda nil "t")
    (setq org-super-agenda-groups '())))

;; Add keybindings
(define-key dc-agenda-map (kbd "v") 'dc/org-agenda-todo-view)

;; Add org-agenda keybindings
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "v") 'dc/org-agenda-todo-view))



;; Provide package
(provide 'dc-org-agenda)

;;; dc-org-agenda.el ends here
