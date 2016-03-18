;;

;; Personal emacs files
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Use Unix line end
(setq default-buffer-file-coding-system 'iso-latin-1-unix)
(prefer-coding-system 'iso-latin-1-unix)
(normal-erase-is-backspace-mode)
(set-default 'truncate-lines t)

;; make passwords invisible
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; Use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Font lock mode: lazy lock (fontifies only when necessary)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Let % show the matching parenthesis
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; C/C++ preferences
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Formatting Code
(load "/usr/share/emacs/site-lisp/clang-format-3.6/clang-format.el")
(defun clang-format-mode-hook ()
  (defun clang-format-buffer ()
    (interactive)
    (save-excursion
      (call-interactively 'mark-whole-buffer)
      (call-interactively 'clang-format-region)))
  (local-set-key (kbd "<f12>") 'clang-format-buffer))
(add-hook 'c-mode-common-hook 'clang-format-mode-hook)
(add-hook 'before-save-hook
         'delete-trailing-whitespace)

;; Turn off line truncation in *compilation* buffer
(defun my-compilation-mode-hook ()
  (setq truncate-lines nil) ;; automatically becomes buffer-local
  (set (make-local-variable 'truncate-partial-width-windows) nil))
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(require 'yaml-mode)

;; Python Preferences
(setq python-indent-offset 2)
(setq-default indent-tabs-mode nil)
(defun py-indent (arg)
  "change python indent on the fly"
  (interactive "p")
  (setq py-indent-offset arg))

;; Enable tab completion in normal buffers
(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (hippie-expand arg)
      ;; (dabbrev-expand arg)
    (indent-according-to-mode)))

;; Add tab fix hook to various modes
(defun my-tab-fix ()
  (local-set-key [tab] 'indent-or-expand))

(add-hook 'c++-mode-hook        'my-tab-fix)
(add-hook 'c-mode-hook          'my-tab-fix)
(add-hook 'cmake-mode-hook      'my-tab-fix)
(add-hook 'conf-mode-hook       'my-tab-fix)
(add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
(add-hook 'html-mode-hook       'my-tab-fix)
(add-hook 'java-mode-hook       'my-tab-fix)
(add-hook 'makefile-mode-hook   'my-tab-fix)
(add-hook 'nxml-mode-hook       'my-tab-fix)
(add-hook 'python-mode-hook     'my-tab-fix)
(add-hook 'sh-mode-hook         'my-tab-fix)
(add-hook 'text-mode-hook       'my-tab-fix)
(add-hook 'yaml-mode-hook       'my-tab-fix)
(add-hook 'markdown-mode-hook   'my-tab-fix)


;; Remove unneeded functions from hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev 
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially 
        try-complete-file-name
        try-expand-all-abbrevs 
        try-expand-list 
        try-expand-line
        try-expand-dabbrev-from-kill))

;; keys and mice
(global-set-key "\M-g" 'goto-line)
;;(mwheel-install)  ; turn on the mouse wheel
;;(setq mouse-wheel-scroll-amount '(10 . 1))
;;(setq mouse-yank-at-point t)

;;;; in emacs-nw in a term, there's weirdness with keys
(defvar termtype nil "terminal type being used")
(set-variable 'termtype (getenv "TERM"))
(if (string= termtype "xterm" )
    (progn
      (global-set-key [backspace] 'backward-delete-char)))

;; Add filename mode hook bindings
(setq auto-mode-alist
      (mapcar 'purecopy
               '(
                 ("BUILD"                . python-mode) ;; Bazel build file.  python is close.
                 ("CMakeLists.txt\\'"    . makefile-mode)
                 ("\\.bash\\'"           . sh-mode)
                 ("\\.bashrc\\'"         . sh-mode)
                 ("\\.c\\'"              . c-mode)
                 ("\\.cc\\'"             . c++-mode)
                 ("\\.cpp\\'"            . c++-mode)
                 ("\\.cxx\\'"            . c++-mode)
                 ("\\.emacs\\'"          . emacs-lisp-mode)
                 ("\\.h\\'"              . c-mode)
                 ("\\.hh\\'"             . c++-mode)
                 ("\\.hpp\\'"            . c++-mode)
                 ("\\.ini\\'"            . conf-mode)
                 ("\\.launch\\'"         . nxml-mode)
                 ("\\.md"                . markdown-mode)
                 ("\\.py\\'"             . python-mode)
                 ("\\.sh\\'"             . sh-mode)
                 ("\\.txt\\'"            . text-mode)
                 ("\\.xml\\'"            . nxml-mode)
                 ("\\.yml\\'"            . yaml-mode)
                 ("\\.yaml\\'"           . yaml-mode)
                 ("\\.htm\\'"            . html-mode)
                 ("\\.html\\'"           . html-mode)
                 ("\\.mako\\'"           . html-mode) ;; mako template file
               )))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(buffers-menu-max-size 30)
 '(cua-mode t nil (cua-base))
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
(put 'downcase-region 'disabled nil)
