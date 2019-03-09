 ;; -*- lexical-binding: t -*-
;;  -*- coding: utf-8 -*-
;; file coding system


(prefer-coding-system 'utf-8)

(package-initialize)

;; it did not work 
;; org-mode reinstall


;; gnutls 
(with-eval-after-load 'gnutls
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem"))
;; Tell emacs where is your personal elisp lib dir
;; bootstrap

(add-to-list 'load-path "/home/s/.emacs.d/lisp/")
(add-to-list 'load-path "/home/s/.emacs.d/pack/")
(add-to-list 'load-path "/home/s/.emacs.d/org-pack/")

;; auto-compile
(require 'auto-compile)

;; (require 'org)
;; disabled to use habits
(setq user-emacs-directory "/home/s/.emacs.d/lisp")
;; security

(setq network-security-level 'high)
(global-linum-mode 1)

(setq max-specpdl-size 10000)

;; spec

;; nnir

(load-file "/home/s/.emacs.d/lisp/imap.el")
(require 'nnir)
(setq nnir-search-engine 'namazu)
(setq nnir-namazu-index-directory (expand-file-name "~/.namazu-mail"))
(setq nnir-namazu-remove-prefix (expand-file-name "~/Mail"))
(setq nnir-mail-backend gnus-select-method)      

;; Graphviz dot

;; ###autoload
(with-eval-after-load "org"
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))


;; inline todo

(require 'org-inlinetask)
(setq org-inlinetask-default-state "TO-READ")

;; UTF-8
(require 'un-define "un-define" t)
(set-buffer-file-coding-system 'utf-8 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)
;;

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; agenda

(custom-set-variables
 '(org-directory "/home/s/org")
 '(org-agenda-files (list org-directory)))
(require 'org-install)
(require 'org-habit)
;; agenda
;; caused an issue last time , conactenated ~/org/~org as an org agenda file
;;(defvar dir-where-you-store-org-files "/home/s/org")
;; (setq 
;; org-agenda-files 
;; (mapcar (lambda (x) (concat dir-where-you-store-org-files x))
 ;;        '("/home/s/org")))

;; clock in
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; view

(setq org-agenda-span 100
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d")

;; package-list
;; update
(setq debug-on-error t)
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (package-initialize))
(add-to-list 'load-path (expand-file-name "/home/s/.emacs.d/lisp"))
(require 'org-install)
;;ucf
(autoload 'ucf-mode "ucf-mode" "Xilinx UCF mode" t)
(add-to-list 'auto-mode-alist '("\\.ucf\\'" . ucf-mode))
;; ide
(setq max-lisp-eval-depth 10000)
;  (setq max-specpdl-size 10000)

(setq ggtags-executable-directory "/usr/bin/gtags")
(setq ggtags-use-idutils t)
(setq ggtags-use-project-gtagsconf nil)
;;  Versioning test
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; slime 
(load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl")

;; modularizing init
;; load the packaged named xyz.
(load "auto-compile")
(load "colorg")
(load "blog")
(load "echo-server")
(load "shared-buffer")
(load "list2csv")
(load "org-fixup")
(load "ox-html")
(load "org-publish")
(load "mmencode")
(load "safe-tls-defaults")
(load "ox-rss")
(load "org-dial")
(load "org-git-link")

;; evil-undo

;; ox-rss
(add-to-list 'load-path "/home/s/.emacs.d/org/lisp/org-mode/contrib/lisp/")
 
(require 'ox-rss)

;; tangle

      (require 'ob-tangle)

;;  (add-to-list 'load-path "/home/s/Work/org-mode/install/org-mode/emacs/site-lisp/org/")
(with-eval-after-load 'org
(org-babel-do-load-languages 'org-babel-load-languages '((sql . t)
							 (python . t)
							 
)))

(require 'package)
(setq load-prefer-newer t
      package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(setq package-archives
'(("ELPA" . "http://tromey.com/elpa/")
   ("gnu" . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")))



;; Bootstrap
;;(unless (package-installed-p 'use-package)
;;  (package-refresh-contents)
;;  (package-install 'use-package))
;; (use-package org :pin gnu)
;; Install org-mode 1
;; check src of download and modify function
(defun package-from-archive (f &rest args)
  (and (apply f args)
       (assq (car args) package-alist)))

(advice-add 'package-installed-p :around 'package-from-archive)
;; bootstrap straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ;; Install org

;; '(use-package org)

;; Contingency install

(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load
(unless (package-installed-p 'org)  ;; Make sure the Org package is
  (package-install 'org))           ;; installed, install it if not
(package-initialize)                ;; Initialize & Install Package


;; Install org-mode 2

(eval-and-compile
  (let ((clp))
    (while (setq clp (locate-library "org"))
      (setq load-path
        (delete
         (directory-file-name (file-name-directory clp)) load-path))))

  (dolist (S (append (apropos-internal (concat "^" (symbol-name 'org) "-"))
             (apropos-internal (concat "^global-" (symbol-name 'org) "-"))))
    (when (and (fboundp S)
           (let ((sf (symbol-function S)))
         (and (listp sf) (eq (car sf) 'autoload))))
      (fmakunbound S))))

;; Just follow git-controlled links without asking
(setq-default vc-follow-symlinks t)

;; install 3


(add-to-list 'load-path "~/lib/elisp/")
;; (add-to-list 'load-path "/home/s/.emacs.d/elpa/org-20150302/")
;; (add-to-list 'load-path "/home/s/.emacs.d/elpa/xclip-1.3/")
;; (add-to-list 'load-path "/home/s/.emacs.d/elpa/htmlize-20130207.1202/")
(add-to-list 'load-path "/home/s/.emacs.d/elpa/polymode-20151013.814/")
(add-to-list 'load-path "/home/s/.emacs.d/elpa/lua-mode-20150518.942/")
(add-to-list 'load-path "/home/s/.emacs.d/elpa/toc-org-20150801.748/")
(add-to-list 'load-path "/home/s/.emacs.d/elpa/epresent-20150324.610/")



(add-to-list 'org-modules "org-habit")
(setq org-habit-preceding-days 7
      org-habit-following-days 1
      org-habit-graph-column 80
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)
;; require  org-html

(require 'font-lock)      
(require 'cc-mode) 
(c-after-font-lock-init)

(setq auto-mode-alist
   (append (mapcar 'purecopy
      '(("\\.c$"   . c-mode)
        ("\\.h$"   . c-mode)
        ("\\.c.simp$" . c-mode)
        ("\\.h.simp$" . c-mode)
        ("\\.a$"   . c-mode)
        ("\\.w$"   . cweb-mode)
        ("\\.cc$"   . c++-mode)
        ("\\.S$"   . asm-mode)
        ("\\.s$"   . asm-mode)
        ("\\.p$"   . pascal-mode)
        ("\\.Rmd$" . poly-markdown-mode)
        ("\\.pas$" . pascal-mode)
        ("\\.tex$" . LaTeX-mode)
        ("\\.txi$" . Texinfo-mode)
        ("\\.el$"  . emacs-lisp-mode)
        ("emacs"  . emacs-lisp-mode)
        ("\\.ml[iylp]?" . tuareg-mode)
        ("[mM]akefile" . makefile-mode)
        ("[mM]akefile.*" . makefile-mode)
        ("\\.mak" . makefile-mode)
        ("\\.cshrc" . sh-mode)
        ("\\.html$" . html-mode)
        ("\\.org$" . org-mode)
)) auto-mode-alist))

(setq inhibit-splash-screen t)

(setq frame-title-format
  '("Emacs - " (buffer-file-name "%f"
    (dired-directory dired-directory "%b"))))

;; (set-default-font "9x15")
;; (set-frame-font "-1ASC-Droid Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-frame-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-29-*-*-*-m-0-iso10646-1")

(global-font-lock-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flyspell-incorrect ((t (:inverse-video t)))))
;;  (set-face-attribute 'flyspell-incorrect (t (:inverse-video t)))

(line-number-mode 1)
(column-number-mode 1)

(load-library "paren")
(show-paren-mode 1)
(transient-mark-mode t)
(require 'paren)

(setq visible-bell t)

(global-set-key (kbd "C-g") 'keyboard-quit-cleanup)

(global-set-key (kbd "C-c i") 
(lambda() (interactive)(org-babel-load-file "/home/s/.emacs.d/init.org")))

(cond
 ((string-equal system-type "darwin")   ; Mac OS X
  (progn
    (setq
     ns-command-modifier 'meta         ; Apple/Command key is Meta
         ns-alternate-modifier nil         ; Option is the Mac Option key
         ns-use-mac-modifier-symbols  nil  ; display standard Emacs (and not standard Mac) modifier symbols
         ))
  )
 )

;; (cua-mode t)

(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

(global-set-key "\^x\^e" 'compile)

(defun jump-mark ()
  (interactive)
  (set-mark-command (point)))
(defun beginning-of-defun-and-mark ()
  (interactive)
  (push-mark (point))
  (beginning-of-defun))
(defun end-of-defun-and-mark ()
  (interactive)
  (push-mark (point))
  (end-of-defun))

(global-set-key "\^c\^b" 'beginning-of-defun-and-mark)
(global-set-key "\^c\^e" 'end-of-defun-and-mark)
(global-set-key "\^c\^j" 'jump-mark)
(global-set-key [S-f6] 'jump-mark)              ;; jump from mark to mark

(global-set-key "\M-g" 'goto-line)

(setq select-active-regions nil)
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)
(setq mouse-drag-copy-region t)

;;  (if(string-equal system-type "gnu/linux")   ; Linux!
;;      (
;;       (require (quote xclip))
;;       (xclip-mode 1)
;;      )()
;;        )

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; C-x C-0 restores the default font size

;; Inspired from http://tex.stackexchange.com/questions/166681/changing-language-of-flyspell-emacs-with-a-shortcut
;; (defun spell (choice)
;;    "Switch between language dictionaries."
;;    (interactive "cChoose:  (a) American | (f) Francais")
;;     (cond ((eq choice ?1)
;;            (setq flyspell-default-dictionary "american")
;;            (setq ispell-dictionary "american")
;;            (ispell-kill-ispell))
;;           ((eq choice ?2)
;;            (setq flyspell-default-dictionary "francais")
;;            (setq ispell-dictionary "francais")
;;            (ispell-kill-ispell))
;;           (t (message "No changes have been made."))) )

(define-key global-map (kbd "C-c s a") (lambda () (interactive) (ispell-change-dictionary "american")))
(define-key global-map (kbd "C-c s f") (lambda () (interactive) (ispell-change-dictionary "francais")))
(define-key global-map (kbd "C-c s r") 'flyspell-region)
(define-key global-map (kbd "C-c s b") 'flyspell-buffer)
(define-key global-map (kbd "C-c s s") 'flyspell-mode)
;; quit
(defun my-keyboard-quit()
  "Escape the minibuffer or cancel region consistently using 'Control-g'.
Normally if the minibuffer is active but we lost focus (say, we clicked away or set the cursor into another buffer)
we can quit by pressing 'ESC' three times. This function handles it more conveniently, as it checks for the condition 
of not beign in the minibuffer but having it active. Otherwise simply doing the ESC or (keyboard-escape-quit) would 
brake whatever split of windows we might have in the frame."
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
      (if (or mark-active (active-minibuffer-window))
          (keyboard-escape-quit))
    (keyboard-quit)))


(define-key global-map (kbd "C-g") 'my-keyboard-quit)

;; misc
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
;; (global-magit-file-mode 1)

(define-key global-map (kbd "C-c w") 'visual-line-mode)

(defun auto-fill-mode-on () (TeX-PDF-mode 1))
(add-hook 'tex-mode-hook 'TeX-PDF-mode-on)
(add-hook 'latex-mode-hook 'TeX-PDF-mode-on)
(setq TeX-PDF-mode t)

(defun auto-fill-mode-on () (auto-fill-mode 1))
(add-hook 'text-mode-hook 'auto-fill-mode-on)
(add-hook 'emacs-lisp-mode 'auto-fill-mode-on)
(add-hook 'tex-mode-hook 'auto-fill-mode-on)
(add-hook 'latex-mode-hook 'auto-fill-mode-on)

(setq c-default-style "k&r")
(setq c-basic-offset 2)

(defun c-reformat-buffer()
   (interactive)
   (save-buffer)
   (setq sh-indent-command (concat
                            "indent -i2 -kr --no-tabs"
                            buffer-file-name
                            )
         )
   (mark-whole-buffer)
   (universal-argument)
   (shell-command-on-region
    (point-min)
    (point-max)
    sh-indent-command
    (buffer-name)
    )
   (save-buffer)
   )
 (define-key c-mode-base-map [f7] 'c-reformat-buffer)

(defalias 'yes-or-no-p 'y-or-n-p)

;;(setq org-directory "/home/s/org/")

(setq org-hide-leading-stars t)
(setq org-alphabetical-lists t)
(setq org-src-fontify-natively t)  ;; you want this to activate coloring in blocks
(setq org-src-tab-acts-natively t) ;; you want this to have completion in blocks
(setq org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
(setq org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-default-notes-file "/home/s/org/notes.org")
     (define-key global-map "\C-cd" 'org-capture)
(setq org-capture-templates (quote (("t" "Todo" entry (file+headline "/home/s/org/todo.org" "Tasks") "* TODO %?
  %i
  %a" :prepend t) ("j" "Journal" entry (file+datetree "/home/s/org/journal.org") "* %?
Entered on %U
  %i
  %a"))))
(add-to-list
 'org-capture-templates
 '("l" "Ledger Entry" plain (file "/home/s/org/inbox.ledger")
   "%<%Y/%m/%d> %^{To whom}
    Assets:Checking  $ -%^{How much}
    Expenses:%?" :unnarrowed t) t)
(setq org-agenda-include-all-todo t)
(setq org-agenda-include-diary t)

; (setq org-agenda-files (quote ("/home/s/org/todo.org" "/home/s/org/sales.org")))
; (setq revert-without-query (quote ("sales.org")))

(setq org-id-method (quote uuidgen))

;; (global-visual-line-mode t)

;; see http://thread.gmane.org/gmane.emacs.orgmode/42715
(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))

(defun ndk/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
            (if (match-end 1)
                (if (equal (match-string 1) "100%")
                    ;; all done - do the state change
                    (org-todo 'done)
                  (org-todo 'todo))
              (if (and (> (match-end 2) (match-beginning 2))
                       (equal (match-string 2) (match-string 3)))
                  (org-todo 'done)
                (org-todo 'todo)))))))

(add-to-list 'load-path "/home/s/.emacs.d/elpa/org-download-20190209.1034")
(require 'org-download)
(setq org-download-method 'attach)
(global-set-key (kbd "C-c S") 'org-download-screenshot)

(global-set-key (kbd "C-c d") 'insert-date)
(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "** %Y-%m-%d")
                   ((equal prefix '(4)) "[%Y-%m-%d]"))))
      (insert (format-time-string format))))

(global-set-key (kbd "C-c t") 'insert-time-date)
(defun insert-time-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "[%H:%M:%S; %d.%m.%Y]")
                   ((equal prefix '(4)) "[%H:%M:%S; %Y-%m-%d]"))))
      (insert (format-time-string format))))

(global-set-key (kbd "C-c v") 'org-show-todo-tree)

(global-set-key (kbd "C-c l") 'org-store-link)

;; (require 'org-git-link) ;; Made some personal modifications
;; (global-set-key (kbd "C-c g") 'org-git-insert-link-interactively)

(global-set-key (kbd "C-c e") (lambda ()
                  (interactive)
                  (insert "** data#\n*** git:\n#+begin_src sh\ngit log -1\n#+end_src\n*** Notes:" )))
                  ;;(insert "** data#\n[[shell:git log -1][git]]\n" )))
                  ;;(insert "** data#\n[[shell:git log -1][git]]\n" (format-time-string "[%H:%M:%S; %d.%m.%Y]" ))))

(global-set-key (kbd "C-c <up>") 'outline-up-heading)
(global-set-key (kbd "C-c <left>") 'outline-previous-visible-heading)
(global-set-key (kbd "C-c <right>") 'outline-next-visible-heading)

(defun org-goto-last-heading-in-tree ()
  (interactive)
  (org-forward-heading-same-level 1)     ; 1. Move to next tree
  (outline-previous-visible-heading 1)   ; 2. Move to last heading in previous tree
  (let ((org-special-ctrl-a/e t))        ; 3. Ignore tags when
    (org-end-of-line)))                  ;    moving to the end of the line
(define-key org-mode-map (kbd "C-c g") 'org-goto-last-heading-in-tree)
(defun goto-last-heading ()
  (interactive)
  (org-end-of-subtree))
(global-set-key (kbd "C-c <down>") 'goto-last-heading)

;; (setq org-export-babel-evaluate nil) ;; This is for org-mode<9 only. It breaks everything otherwise
;;  To activate this feature, you need to set #+PROPERTY: header-args :eval never-export in the beginning or your document
(setq org-confirm-babel-evaluate nil)
;; active Babel languages

;; add additional languages with '((language . t)))
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (emacs-lisp . t)
   ))

;; templates
(setq org-src-preserve-indentation t)
(eval-after-load 'org
'(progn
     (add-to-list 'org-structure-template-alist
'(("s" . "src")))))
;; broken templates
;; ;; (add-to-list 'org-structure-template-alist
;; ;;             '("s" "#+NAME: ?\n#+BEGIN_SRC \n\n#+END_SRC"))
;; (add-to-list 'org-structure-template-alist
;;         '("S" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>"))

;; (add-to-list 'org-structure-template-alist
;;         '("m" "#+begin_src emacs-lisp\n\n#+end_src" "<src lang=\"emacs-lisp\">\n\n</src>"))

;; ;; (add-to-list 'org-structure-template-alist
;; ;;        '("r" "#+begin_src R :results output :session *R* :exports both\n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

;; ;; (add-to-list 'org-structure-template-alist
;; ;;        '("R" "#+begin_src R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_sr;;c" "<src lang=\"R\">\n\n</src>"))

;; ;;(add-to-list 'org-structure-template-alist
;; ;;        '("RR" "#+begin_src R :results output graphics :file  (org-babel-temp-f;;  (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\;;  ".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<s;; lang=\"R\">\n\n</src>"))

;; (add-to-list 'org-structure-template-alist
;;         '("p" "#+begin_src python :results output :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

;; (add-to-list 'org-structure-template-alist
;;         '("P" "#+begin_src python :results output :session *python* :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

;; (add-to-list 'org-structure-template-alist
;;         '("b" "#+begin_src shell :results output :exports both\n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

;; (add-to-list 'org-structure-template-alist
;;         '("B" "#+begin_src shell :session *shell* :results output :exports both \n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

;; (add-to-list 'org-structure-template-alist
;;         '("g" "#+begin_src dot :results output graphics :file \"/tmp/graph.pdf\" :exports both
;;    digraph G {
;;       node [color=black,fillcolor=white,shape=rectangle,style=filled,fontname=\"Helvetica\"];
;;       A[label=\"A\"]
;;       B[label=\"B\"]
;;       A->B
;;    }\n#+end_src" "<src lang=\"dot\">\n\n</src>"))

(global-set-key (kbd "C-c S-t") 'org-babel-execute-subtree)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images) 
(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-babel-result-hide-all)

(unless (boundp 'org-latex-classes) (setq org-latex-classes nil))

(add-to-list 'org-latex-classes '("acm-proc-article-sp" "\\documentclass{acm_proc_article-sp}\n \[NO-DEFAULT-PACKAGES]\n \[EXTRA]\n  \\usepackage{graphicx}\n  \\usepackage{hyperref}"  ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}")                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")                       ("\\paragraph{%s}" . "\\paragraph*{%s}")                       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-to-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f ; bibtex `basename %f | sed 's/\.tex//'` ; pdflatex -interaction nonstopmode -output-directory  %o %f ; pdflatex -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-classes '("article" "\\documentclass{article}\n \[NO-DEFAULT-PACKAGES]\n \[EXTRA]\n  \\usepackage{graphicx}\n  \\usepackage{hyperref}"  ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}")                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")                       ("\\paragraph{%s}" . "\\paragraph*{%s}")                       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'auto-mode-alist '("\\.eml\\'" . org-mode))

(require 'tramp)
(setq tramp-default-method "scp")

;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;;file to save todo items
;; (setq org-agenda-files (quote ("/home/s/org")))

;;set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;;capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "/root/task.org" "Tasks")
         "* TODO [#A] %?")))
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/task.org" "Tasks")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode agenda options                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))
;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))
;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;don't give awarning colour to tasks with impending deadlines
;;if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;;don't show tasks that are scheduled or have deadlines in the
;;normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))
;;sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
  (quote
   ((agenda deadline-up priority-down)
    (todo priority-down category-keep)
    (tags priority-down category-keep)
    (search category-keep))))

 (setq org-agenda-custom-commands
      '(("P" "Project List"
          ( (tags "PROJECT")
          )
        )
        ("O" "Office"
          ( (agenda)
            (tags-todo "OFFICE")
          )
        )
        ("W" "Weekly Plan"
          ( (agenda)
            (todo "TODO")
            (tags "PROJECT")
          )
        )
        ("H" "Home NA Lists"
          ( (agenda)
            (tags-todo "HOME")
            (tags-todo "COMPUTER")
          )
        )
       )
  )

(defun org-todo-force-notes ()
  (interactive)
  (let ((org-todo-log-states
         (mapcar (lambda (state)
                   (list state 'note 'time))
                 (apply 'append org-todo-sets))))
    (call-interactively 'org-todo)))


(define-key org-mode-map (kbd "C-c C-S-t") 'org-todo-force-notes)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(package-selected-packages
   (quote
    (ox-pandoc magit s aria2 ecb tabbar eide outshine outorg ggtags gtags poporg twittering-mode twitter edit-server gmail-message-mode ebdb material-theme impatient-mode paredit slime emacs-cl togetherly ##  rudel org-download cl-print ))))
(put 'upcase-region 'disabled nil)


