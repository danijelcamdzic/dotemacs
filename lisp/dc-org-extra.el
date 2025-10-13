;;; dc-extra.el --- Extra org packages

;;; Code:

;;                   -------------------------
;;                   Package: org-transclusions
;;                   -------------------------

(use-package org-transclusion
  :ensure t  
  :custom
  (org-transclusion-exclude-elements nil))

(defun dc/org-transclusion-set-link-prefix ()
  "Sets the dc-org-roam-link-prefix to #+transclude: .
Used to add a prefix to the function which inserts org-roam
nodes based on tags."
  (interactive)
  (setq dc-org-roam-link-prefix "#+transclude: "))

(defun dc/org-transclusion-insert-node ()
  "Insert a transcluded link to an org-roam node."
  (interactive)
  (let ((node (org-roam-node-read)))
    (when node
      (let ((link (format "#+transclude: [[id:%s][%s]]"
                          (org-roam-node-id node)
                          (org-roam-node-title node))))
        (insert link)))))

;; Add keybindings
(define-key dc-org-map (kbd "z") 'dc/org-transclusion-insert-node)



;;                   -------------------------
;;                       Package: org-ref 
;;                   -------------------------

(use-package org-ref
  :defer t
  :ensure t
  :after org
  )



;;                   -------------------------
;;                       Package: org-noter 
;;                   -------------------------

(use-package org-noter
  :defer t
  :ensure t  
  :after org 
  :config
  ;; Set the location of the notes
  (setq org-noter-notes-search-path '(org-directory))
  )



;;                   -------------------------
;;                         Package: mpv 
;;                   -------------------------

(use-package mpv
  :defer t
  :ensure t
  )



;;                   -------------------------
;;                    Package: org-media-note 
;;                   -------------------------

(use-package org-media-note
  :defer t
  :hook (org-mode .  org-media-note-mode)
  :bind (("H-v" . org-media-note-hydra/body))
  :config
  ;; Set up save method
  (setq org-media-note-screenshot-save-method 'attach)
  (setq org-media-note-screenshot-link-type-when-save-in-attach-dir 'attach)
  )

(defun dc/org-media-note--format-picture-file-name--prepend-timestamp-advice (orig-func &rest args)
  "Advice to prepend the current timestamp to the filename created by `org-media-note--format-picture-file-name'."
  (let ((original-filename (apply orig-func args))
        (timestamp (format-time-string "%Y-%m-%d_%H-%M-%S")))
    (concat timestamp "_" original-filename)))

(advice-add 'org-media-note--format-picture-file-name :around #'dc/org-media-note--format-picture-file-name--prepend-timestamp-advice)

(defun dc/remove-invalid-characters-from-filename (filename)
  "Remove invalid characters from filename in order for it to sync to different systems using syncthing."
  (replace-regexp-in-string "[/*\":<>?|]" "" filename))

(defun dc/org-media-note--format-picture-file-name--remove-invalid-characters-from-filename-advice (orig-func &rest args)
  "Advice to remove invalid characters from filename in `org-media-note--format-picture-file-name'."
  (dc/remove-invalid-characters-from-filename (apply orig-func args)))

(advice-add 'org-media-note--format-picture-file-name :around #'dc/org-media-note--format-picture-file-name--remove-invalid-characters-from-filename-advice)



;; Provide package
(provide 'dc-org-extra)

;;; dc-org-extra.el ends here
