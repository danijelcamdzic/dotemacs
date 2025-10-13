;;; dc-programming.el --- Programming configuration

;;; Code:

;;                   -------------------------
;;                              C/C++  
;;                   -------------------------

(defun dc/setup-c-cpp-mode ()
  "Set basic c and cpp offset."
  (setq c-basic-offset 4))

;; Enable line numbers for  C modes
(add-hook 'c-mode-common-hook (lambda () (display-line-numbers-mode 1)))

;; Set hook to set indentation when in c/cpp file
(add-hook 'c-mode-common-hook 'dc/setup-c-cpp-mode)



;;                   -------------------------
;;                             Python 
;;                   -------------------------

;; Set the indentation level for Python code
(setq python-indent-offset 4)

;; Enable line numbers for Python mode
(add-hook 'python-mode-hook (lambda () (display-line-numbers-mode 1)))



;;                   -------------------------
;;                        Package: magit  
;;                   -------------------------

(use-package magit
  :defer t
  :ensure t
  )



;; Provide package
(provide 'dc-programming)

;;; dc-programming.el ends here
