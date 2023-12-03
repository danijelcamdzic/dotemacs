;;; org-mode-config.el
;;
;; Author: Danijel Camdzic
;; Maintainer: Danijel Camdzic <danijelcamdzic@tuta.com>
;;
;; License: NO LICENCE
;;
;;; Code:

;;; --------- Org-mode ---------
;; Use org-mode
(require 'org)

;; Use org-tempo
(require 'org-tempo)

;; Define and set the home directory based on system type
(setq my-home-directory
      (cond
       ((eq system-type 'gnu/linux) "~/")
       ((eq system-type 'android) "/storage/emulated/0/")
       (t "~/")))

;; Set the org-directory
(setq org-directory (concat my-home-directory "Notes/"))

;; Set the main Org Roam directory and the directory for daily notes
(setq org-roam-directory org-directory)
(setq org-roam-dailies-directory (concat org-directory "dailies/"))

;; Include only .org files and exclude all else
(setq org-roam-file-exclude-regexp "\\(\\.gpg\\)$")

;; Set the eww-bookmarks directory
(setq eww-bookmarks-directory (concat my-home-directory "Documents/bookmarks/"))

;; Set the default bookmarks file
(setq bookmark-default-file (concat my-home-directory "Documents/bookmarks/bookmarks"))

;; Customize custom faces in org-mode
(with-eval-after-load 'org
  (custom-set-faces
   '(bold ((t (:foreground "#008000" :weight bold))))
   '(italic ((t (:foreground "#B0A030" :slant italic))))
   '(org-scheduled ((t (:foreground "#555555"))))
   '(org-scheduled-today ((t (:foreground "grey"))))
   '(strike-through ((t (:foreground "#8B0000" :strike-through t))))))

;; Set up text indentation
(setq org-startup-indented t)

;; Add a hook to org-mode to limit the column width to 80 characters
(add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook #'turn-on-auto-fill)

(defun my/clock-in ()
  "Clock in the current org heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-clock-in)
    (org-clock-in)))

(defun my/clock-out ()
  "Clock out of the current org heading."
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-clock-out)
    (org-clock-out)))

(defun my/insert-current-date-time ()
  "Insert the current date and time along with the three-letter weekday name in
the format YYYY-MM-DD Day H:M."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a %H:%M")))

;;; --------- Org-roam ---------
;; Ensure org-roam package is installed and loaded
(unless (package-installed-p 'org-roam)
  (package-install 'org-roam))
(require 'org-roam)

;; Setup org-roam
(org-roam-setup)

;; Setup preview of org-roam nodes
(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:30}" 'face 'org-tag)))

(defun my/get-org-roam-node-hierarchy (node)
  "Get the hierarchy of NODE as a list of titles.
The hierarchy includes the NODE title, its ancestor titles, and the parent node title."
  (let ((titles '())
        (title (org-roam-node-title node))
        (file-path (org-roam-node-file node))
        (file-title)
        (olp (org-roam-node-olp node)))
    (setq file-title (caar (org-roam-db-query [:select title :from nodes :where (= file $s1)] file-path)))
    (when (and file-title (not (equal file-title title)) (not (equal file-title (car olp))))
      (setq titles (append titles (list file-title))))
    (when olp
      (setq titles (append titles (nreverse olp))))
    (when title
      (setq titles (append titles (list title))))
    titles))

(defun my/insert-org-roam-nodes-by-tag(keywords exclude-keywords &optional filter-fn)
  "Inserts all Org-roam nodes connected to the provided keywords and not connected to the exclude keywords.
KEYWORDS is a space-separated list of keywords to find the connected nodes.
EXCLUDE-KEYWORDS is a space-separated list of keywords to exclude nodes.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
  (interactive "sKeywords: \nsExclude Keywords: ")
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* ((all-nodes (org-roam-node-list))
               (keywords-list (if (string= keywords "") '() (split-string keywords " ")))
               (exclude-keywords-list (if (string= exclude-keywords "") '() (split-string exclude-keywords " ")))
               (filtered-nodes (cl-remove-if-not
                                (lambda (node)
                                  (and (if keywords-list
                                           (cl-every (lambda (keyword)
                                                       (or (string-match-p (regexp-quote keyword) (org-roam-node-title node))
                                                           (cl-some (lambda (tag)
                                                                      (string-match-p (regexp-quote keyword) tag))
                                                                    (org-roam-node-tags node))))
                                                     keywords-list)
                                         t)
                                       (if exclude-keywords-list
                                           (cl-notany (lambda (exclude-keyword)
                                                        (or (string-match-p (regexp-quote exclude-keyword) (org-roam-node-title node))
                                                            (cl-some (lambda (tag)
                                                                       (string-match-p (regexp-quote exclude-keyword) tag))
                                                                     (org-roam-node-tags node))))
                                                      exclude-keywords-list)
                                         t)
                                       (or (not filter-fn) (funcall filter-fn node))))
                                all-nodes))
               (sorted-nodes (sort filtered-nodes
                                   (lambda (a b)
                                     (let ((hierarchy-a (mapconcat #'identity (my/get-org-roam-node-hierarchy a) "->"))
                                           (hierarchy-b (mapconcat #'identity (my/get-org-roam-node-hierarchy b) "->")))
                                       (string< hierarchy-a hierarchy-b))))))
          (dolist (node sorted-nodes)
            (let* ((id (org-roam-node-id node))
                   (hierarchy (my/get-org-roam-node-hierarchy node))
                   (arrow-chain (if (> (length hierarchy) 1)
                                    (mapconcat #'identity hierarchy "->")
                                  (org-roam-node-title node)))
                   (link (org-link-make-string (concat "id:" id) arrow-chain)))
              (insert link)
              (insert "\n")
              (run-hook-with-args 'org-roam-post-node-insert-hook
                                  id
                                  arrow-chain))))))
  (deactivate-mark))

;;; --------- Org-analyzer ---------
;; Ensure the org-analyzer package is installed and loaded
(unless (package-installed-p 'org-analyzer )
  (package-install 'org-analyzer ))
(with-eval-after-load 'org
  (require 'org-analyzer))

;; Set directory for org-analyzer
(setq org-analyzer-org-directory org-directory)

;;; --------- Websocket ---------
;; Ensure the websocket package is installed and loaded
(unless (package-installed-p 'websocket)
  (package-install 'websocket))
(with-eval-after-load 'org-roam
  (require 'websocket))

;;; --------- Org-roam-ui ---------
;; Ensure the org-roam-ui package is installed and loaded
(unless (package-installed-p 'org-roam-ui)
  (package-install 'org-roam-ui))
(with-eval-after-load 'org-roam
  (require 'org-roam-ui))

;;; --------- Org-transclusion ---------
;; Ensure the org-transclusion package is installed and loaded
(unless (package-installed-p 'org-transclusion)
  (package-install 'org-transclusion))
(require 'org-transclusion)

;;; --------- Org-download ---------
;; Ensure the org-download package is installed and loaded
(unless (package-installed-p 'org-download)
  (package-install 'org-download))
(require 'org-download)

;; Provide package for use
(provide `org-mode-config)

;; org-mode-config.el ends here
