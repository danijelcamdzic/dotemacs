;;; dc-org-roam.el --- Org-roam configuration

;;; Code:

;;                   -------------------------
;;                      Package: org-roam 
;;                   -------------------------

(use-package org-roam
  :after org
  :ensure t
  :config
  ;; Set directories
  (setq org-roam-directory org-directory)
  (setq org-roam-dailies-directory (concat org-directory "dailies/"))

  ;; Setup org-roam
  (org-roam-setup)
  )

(defun dc/org-roam--get-node-heirarchy (node)
  "Get the hierarchy of NODE as a list of titles, excluding non-node headings.
The hierarchy includes the NODE title and its ancestor node titles."
  (let ((titles '())
        (title (org-roam-node-title node))
        (file-path (org-roam-node-file node))
        (file-title)
        (olp (org-roam-node-olp node)))
    (setq file-title (caar (org-roam-db-query [:select title :from nodes :where (= file $s1)] file-path)))
    (when (and file-title (not (equal file-title title)))
      (push file-title titles))
    (dolist (heading olp)
      (let ((heading-title (car (last (split-string heading "/"))))) ;; Extract the heading title
        (when (caar (org-roam-db-query [:select id :from nodes :where (= title $s1)] heading-title))
          (push heading-title titles))))
    (push title titles)
    (nreverse titles)))

(defvar dc-org-roam-hierarchy-display-separator
  (propertize "->" 'face '(shadow))
  "Separator for org-roam hierarchy displaying.")

(defun dc/org-roam--create-node-hierarchy-chain (node)
  "Return the hierarchy of NODE as a string with a predefine separator."
  (let ((hierarchy (dc/org-roam--get-node-heirarchy node)))
    (if (cdr hierarchy)
        (let* ((last-element (car (last hierarchy)))
               (non-last-elements (butlast hierarchy))
               (shadow-italicized-elements (mapcar (lambda (element)
                                                     (propertize element 'face '(shadow italic)))
                                                   non-last-elements)))
          (concat (mapconcat 'identity shadow-italicized-elements dc-org-roam-hierarchy-display-separator) dc-org-roam-hierarchy-display-separator last-element))
      (mapconcat 'identity hierarchy dc-org-roam-hierarchy-display-separator))))

(cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
  "Method for obtaining hierarchy display for org-roam nodes."
  (dc/org-roam--create-node-hierarchy-chain node))

(cl-defmethod org-roam-node-colon-tags ((node org-roam-node))
  "Tag formatting for org-roam nodes."
  (let ((tags (org-roam-node-tags node)))
    (if tags
        (concat " :" (mapconcat 'identity tags ":") ":")
      "")))

(cl-defmethod org-roam-node-node-type ((node org-roam-node))
  "Return a string which indicates whether a node is a `@roam' or a `@daily'."
  (let ((file-path (org-roam-node-file node)))
    (if (string-prefix-p (file-name-as-directory org-roam-dailies-directory) (file-name-directory file-path))
        " @daily"
      " @roam")))

;; Set the hierarchy display formatting
(setq org-roam-node-display-template
      (concat "${hierarchy}" "${node-type}" (propertize "${colon-tags}" 'face 'org-tag)))

(defvar dc-org-roam-hierarchy-insert-separator
  (propertize "->" 'face '(shadow))
  "Separator for org-roam hierarchy insertion.")

(defvar dc-org-roam-link-prefix ""
  "Prefix to be added to org-roam links before insertion.")

(defun dc/org-roam-reset-link-prefix ()
  "Sets the dc-org-roam-link-prefix to an empty string."
  (interactive)
  (setq dc-org-roam-link-prefix ""))

(defun dc/org-roam-set-link-prefix ()
  "Sets the dc-org-roam-link-prefix to prefix-string."
  (interactive)
  (setq prefix-string (read-string "Prefix string: "))
  (setq dc-org-roam-link-prefix prefix-string))

(defun dc/org-roam-insert-nodes-by-name-tag-or-property ()
  "Interactive function to insert Org-roam nodes filtered by titles, tags, and properties into the current buffer.
This function allows the user to choose between filtering nodes based on titles, tags, or properties, individually or in combination.
For titles and tags, the user can specify keywords to include and exclude.
The user can continuously add different properties to filter by, specifying the property name and value for each.
Nodes that match all specified criteria are then inserted with their hierarchy and linked using Org-roam's ID scheme."
  (interactive)
  (let ((use-titles (y-or-n-p "Filter by title? "))
        title-keywords title-exclude-keywords
        (use-tags)
        tag-keywords tag-exclude-keywords
        property-list
        all-nodes filtered-nodes sorted-nodes)
    
    (when use-titles
      (setq title-keywords (read-string "Include title keywords: "))
      (setq title-exclude-keywords (read-string "Exclude title keywords: ")))
    
    (setq use-tags (y-or-n-p "Filter by tags? "))
    (when use-tags
      (setq tag-keywords (read-string "Include tags: "))
      (setq tag-exclude-keywords (read-string "Exclude tags: ")))
    
    (while (y-or-n-p "Filter by a property? ")
      (let ((property-name (read-string "Property name: "))
            (property-value (read-string "Property value: ")))
        (push (cons property-name property-value) property-list)))

    (unwind-protect
        (atomic-change-group
          (setq all-nodes (org-roam-node-list))
          (setq filtered-nodes
                (cl-remove-if-not
                 (lambda (node)
                   (and (if use-titles
                            (let ((title-include-list (if (string= title-keywords "") '() (split-string title-keywords " ")))
                                  (title-exclude-list (if (string= title-exclude-keywords "") '() (split-string title-exclude-keywords " "))))
                              (and (or (null title-include-list)
                                       (cl-some (lambda (keyword)
                                                  (string-match-p (regexp-quote keyword) (org-roam-node-title node)))
                                                title-include-list))
                                   (or (null title-exclude-list)
                                       (cl-notany (lambda (exclude-keyword)
                                                    (string-match-p (regexp-quote exclude-keyword) (org-roam-node-title node)))
                                                  title-exclude-list))))
                          t)
                        (if use-tags
                            (let ((tag-include-list (if (string= tag-keywords "") '() (split-string tag-keywords " ")))
                                  (tag-exclude-list (if (string= tag-exclude-keywords "") '() (split-string tag-exclude-keywords " "))))
                              (and (or (null tag-include-list)
                                       (cl-every (lambda (keyword)
                                                  (cl-some (lambda (tag)
                                                             (string-match-p (regexp-quote keyword) tag))
                                                           (org-roam-node-tags node)))
                                                tag-include-list))
                                   (or (null tag-exclude-list)
                                       (cl-notany (lambda (exclude-keyword)
                                                    (cl-some (lambda (tag)
                                                               (string-match-p (regexp-quote exclude-keyword) tag))
                                                             (org-roam-node-tags node)))
                                                  tag-exclude-list))))
                          t)
                        (cl-every (lambda (prop-pair)
                                    (let ((node-prop (assoc (car prop-pair) (org-roam-node-properties node))))
                                      (and node-prop
                                           (string= (cdr node-prop) (cdr prop-pair)))))
                                  property-list)))
                 all-nodes))
          (setq sorted-nodes
                (sort filtered-nodes
                      (lambda (a b)
                        (let ((hierarchy-a (mapconcat #'identity (dc/org-roam--get-node-heirarchy a) dc-org-roam-hierarchy-insert-separator))
                              (hierarchy-b (mapconcat #'identity (dc/org-roam--get-node-heirarchy b) dc-org-roam-hierarchy-insert-separator)))
                          (string< hierarchy-a hierarchy-b)))))
          (dolist (node sorted-nodes)
            (let* ((id (org-roam-node-id node))
                   (hierarchy (dc/org-roam--get-node-heirarchy node))
                   (arrow-chain (if (> (length hierarchy) 1)
                                    (mapconcat #'identity hierarchy dc-org-roam-hierarchy-insert-separator)
                                  (org-roam-node-title node)))
                   (link (org-link-make-string (concat "id:" id) arrow-chain)))
              (insert (concat dc-org-roam-link-prefix link "\n"))
              (run-hook-with-args 'org-roam-post-node-insert-hook id arrow-chain))))
      (deactivate-mark))))

(defun dc/org-roam-find-nodes-by-name-tag-or-property ()
  "Interactive function to find  Org-roam nodes filtered by titles, tags, and properties.
This function allows the user to choose between filtering nodes based on titles, tags, or properties, individually or in combination.
For titles and tags, the user can specify keywords to include and exclude.
The user can continuously add different properties to filter by, specifying the property name and value for each.
Nodes that match all specified criteria are then displayed with their hierarchy."
  (interactive)
  (let ((use-titles (y-or-n-p "Filter by title? "))
        title-keywords title-exclude-keywords
        (use-tags)
        tag-keywords tag-exclude-keywords
        property-list
        all-nodes filtered-nodes sorted-nodes)
    
    (when use-titles
      (setq title-keywords (read-string "Include title keywords: "))
      (setq title-exclude-keywords (read-string "Exclude title keywords: ")))
    
    (setq use-tags (y-or-n-p "Filter by tags? "))
    (when use-tags
      (setq tag-keywords (read-string "Include tags: "))
      (setq tag-exclude-keywords (read-string "Exclude tags: ")))
    
    (while (y-or-n-p "Filter by a property? ")
      (let ((property-name (read-string "Property name: "))
            (property-value (read-string "Property value: ")))
        (push (cons property-name property-value) property-list)))

    (let ((filter-fn (lambda (node)
                       (and (if use-titles
                                (let ((title-include-list (if (string= title-keywords "") '() (split-string title-keywords " ")))
                                      (title-exclude-list (if (string= title-exclude-keywords "") '() (split-string title-exclude-keywords " "))))
                                  (and (or (null title-include-list)
                                           (cl-some (lambda (keyword)
                                                      (string-match-p (regexp-quote keyword) (org-roam-node-title node)))
                                                    title-include-list))
                                       (or (null title-exclude-list)
                                           (cl-notany (lambda (exclude-keyword)
                                                        (string-match-p (regexp-quote exclude-keyword) (org-roam-node-title node)))
                                                      title-exclude-list))))
                              t)
                            (if use-tags
                                (let ((tag-include-list (if (string= tag-keywords "") '() (split-string tag-keywords " ")))
                                      (tag-exclude-list (if (string= tag-exclude-keywords "") '() (split-string tag-exclude-keywords " "))))
                                  (and (or (null tag-include-list)
                                           (cl-some (lambda (keyword)
                                                      (cl-some (lambda (tag)
                                                                 (string-match-p (regexp-quote keyword) tag))
                                                               (org-roam-node-tags node)))
                                                    tag-include-list))
                                       (or (null tag-exclude-list)
                                           (cl-notany (lambda (exclude-keyword)
                                                        (cl-some (lambda (tag)
                                                                   (string-match-p (regexp-quote exclude-keyword) tag))
                                                                 (org-roam-node-tags node)))
                                                      tag-exclude-list))))
                              t)
                            (cl-every (lambda (prop-pair)
                                        (let ((node-prop (assoc (car prop-pair) (org-roam-node-properties node))))
                                          (and node-prop
                                               (string= (cdr node-prop) (cdr prop-pair)))))
                                      property-list)))))
      (let ((node (org-roam-node-read nil filter-fn)))
        (when node
          (org-roam-node-visit node))))))

(defun dc/org-roam-copy-tags-from-node ()
  "Prompt for an Org-roam node (the “source”) and copy its tags into the current Org-roam file."
  (interactive)
  (let* ((source-node (org-roam-node-read))
         (source-tags (when source-node
                        (org-roam-node-tags source-node))))
    (if (and source-node source-tags)
        (progn
          (org-roam-tag-add source-tags)
          (message "Copied tags from '%s' to current node: %s"
                   (org-roam-node-title source-node)
                   (string-join source-tags ", ")))
      (message "No tags found for '%s'."
               (when source-node (org-roam-node-title source-node))))))

;; Add keybinding menu for org-roam
(define-prefix-command 'dc-roam-map)
(global-set-key (kbd "C-c R") 'dc-roam-map)

;; Add keybindings
(define-key dc-roam-map (kbd "d") 'org-roam-dailies-find-date)
(define-key dc-roam-map (kbd "t") 'org-roam-dailies-goto-today)
(define-key dc-roam-map (kbd "f") 'org-roam-node-find)
(define-key dc-roam-map (kbd "i") 'org-roam-node-insert)
(define-key dc-roam-map (kbd "p") 'dc/org-roam-copy-tags-from-node)



;;                   -------------------------
;;                       Package: org-ql 
;;                   -------------------------

(use-package org-ql
  :after org
  :ensure t
  )



;;                   -------------------------
;;                       Package: websocket 
;;                   -------------------------

(use-package websocket
  :after org-roam
  :ensure t
  )



;;                   -------------------------
;;                     Package: org-roam-ui
;;                   -------------------------

(use-package org-roam-ui
  :after (org-roam websocket)
  :ensure t
  )



;; Provide package
(provide 'dc-org-roam)

;;; dc-org-roam.el ends here
