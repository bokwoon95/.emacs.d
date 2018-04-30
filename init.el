;; -*- lexical-binding: t; -*-
; (setq gc-cons-threshold 402653184
;       gc-cons-percentage 0.6)
; (defvar doom--file-name-handler-alist file-name-handler-alist)
; (setq file-name-handler-alist nil)
; If emacs has some weird emacsclient error, try commenting out the above

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(emacs-init-time) ; C-M-x
;; C-u 0 M-x byte-recompile-directory to force recompile everything

;; Add LaTeX binary to PATH
;; Needed for orgmode inline LaTeX previews
(setenv "PATH" (concat "/Library/TeX/texbin" (getenv "PATH")))
(setq exec-path (append '("/Library/TeX/texbin") exec-path))
;; Add Homebrew binaries to PATH
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/local/bin:") exec-path))

;; fetch the list of packages available
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
;; Install use-package automatically
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
; List the packages you want
(setq package-list '(evil
                     counsel
                     writeroom-mode
                     feebleline
                     ample-theme
                     vdiff))

; evil - counsel (Ivy+Swiper) : MUST HAVE PACKAGES
; writeroom - feebleline : Minimalist is king
; ample-theme - alternative colorschemes
; vdiff - vim diff for emacs

; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Railwaycat's Emacs-mac: use option as meta
(setq-default mac-option-modifier 'meta)

;; Set Hyper key
(setq mac-command-modifier 'hyper)
(global-set-key (kbd "C-H-n") (kbd "C-u 5 C-n"))
(global-set-key (kbd "C-H-p") (kbd "C-u 5 C-p"))
(global-set-key (kbd "H-h") 'ns-do-hide-emacs)

;; Quick access to init.el
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c l") 'find-user-init-file)
;; Quick reload init.el
(defun reload-init-file ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;;Strip away silly GUI defaults
(setq inhibit-startup-screen t)
(fringe-mode '(0 . 0)) ;; remove fringes at the side
(tool-bar-mode -1) ; this is the real trash
;; (toggle-scroll-bar -1)
(unless (display-graphic-p)
   (menu-bar-mode -1))
;;GUI settings
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; start emacs in fullscreen
(set-default-font "Source Code Pro 13")
;; (set-face-font 'variable-pitch "Garamond 17")
(set-face-font 'variable-pitch "Vollkorn 16")
;; (set-face-font 'variable-pitch "Crimson Text SemiBold 17")
;; (set-face-font 'variable-pitch "Cardo 17")
(setq-default line-spacing 1)
(global-visual-line-mode t) ;;wrap text
(setq scroll-conservatively 101)
;; Set colorscheme depending on whether GUI or TUI (doesn't seem to work for emacs -nw)
(if (display-graphic-p)
    (load-theme 'whiteboard t)
  (load-theme 'wheatgrass t))
;; Strip the modeline away?
;; (setq-default mode-line-format nil)
(window-divider-mode 1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places 'bottom-only)

;; temp function that supposedly resizes emacs
(defun bjm-frame-resize-r ()
  (interactive)
  (set-frame-width (selected-frame) 625 nil 'pixelwise)
  (set-frame-position (selected-frame) (- (display-pixel-width) (frame-pixel-width)) 0))
;; SANER DEFAULTS
(setq save-interprogram-paste-before-kill t) ; sane pasting from external source
(setq-default tab-always-indent 'complete) ; tab to indent, tab again to get completion (doesn't seem to work?)
(setq ring-bell-function 'ignore) ; mute annoying bell
(global-prettify-symbols-mode +1)
;; Faster paren matching
(show-paren-mode 1)
(setq show-paren-delay 0)
;; Vim's autoread
(global-auto-revert-mode t) ; not really sure what the difference is between this line and the one below
(add-hook 'focus-in-hook 'auto-revert-mode) ; global-auto-revert-mode is too slow when I switch focus to emacs
;; Save backup files in system temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; Saner Emacs Keybinds
(global-set-key (kbd "C-a") 'back-to-indentation) ; C-a obeys indentation. Default key for this is M-m
(defun C0Ck-with-indent ()
  (interactive)
  (delete-region (point) (line-beginning-position))
  (indent-according-to-mode))
(global-set-key (kbd "C-H-u") 'C0Ck-with-indent)
; Flip "C-x k" & "C-x C-k"
(global-set-key (kbd "C-x C-k") 'kill-buffer)
; (global-set-key (kbd "C-x k RET") 'kmacro-edit-macro) ; DO NOT turn this on it will cause your emacs to crash on init
;; Sane scrolling
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;;one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;;don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;;scroll window under mouse
;; Mouse scrolling in terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
            (interactive)
            (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
            (interactive)
            (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )
;; suppress beginning/end of buffer messages (does this make emacs laggy?)
(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))
(setq command-error-function #'my-command-error-function)
;; Enable recent files (used by counsel-recentf)
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)
;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))
;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)
;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)
;; Stop opening help windows all over the god damn place
;; (add-to-list 'display-buffer-alist
;;              '("*Help*" display-buffer-same-window))

;; TabsAreEvil?
;; (setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Load lisp scripts
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
;; emacs autosave (get script from https://github.com/redguardtoo/emacs.d/blob/master/site-lisp/auto-save/auto-save.el)
(require 'auto-save)
(auto-save-enable)
;; Useful Commands
(require 'useful-commands)
;; Single dired window
(eval-after-load 'dired '(progn (require 'joseph-single-dired)))
;; Orgmode latex cursor toggle
;; (require 'org-latex-cursor-toggle)
;; Save and Restore Org Folds
(eval-after-load 'org-mode '(progn (require 'org-fold)))
;; Targets.vim for emacs
(require 'targets)

;; Filetype specific settings
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)
(add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode)
;; (add-hook 'org-mode-hook 'writeroom-mode)
(if (display-graphic-p)
    (progn (add-hook 'org-mode-hook 'writeroom-mode)
        (add-hook 'text-mode-hook 'writeroom-mode)))
;; (add-hook 'text-mode-hook 'variable-pitch-mode) ; handled by mixed-pitch.el

;; q kills buffer instead of closing window
(defun q-kills-buffer ()
  (local-set-key (kbd "q") (kbd "C-x k RET")))
(add-hook 'ibuffer-mode-hook 'q-kills-buffer) ; q terminates ibuffer; previously bound to quit-window

;; half-page scroll
(require 'view)
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "C-H-v") 'View-scroll-half-page-backward)

;; org-tips
;; to expand all trees in org, "C-u C-u C-u TAB" in emacs mode

;; Org Mode
(setq org-M-RET-may-split-line nil)
(setq org-cycle-separator-lines 1) ; only 1 blank line is needed to keep org folds apart
(setq org-blank-before-new-entry '((heading) (plain-list-item))) ; do not insert blank lines on M-RET
(setq org-startup-folded nil) ; open with all folds unfolded
(setq org-link-frame-setup '((file . find-file))) ; open links in current buffer
(setq org-image-actual-width nil) ; do use actual size of image when inlining them
(setq org-hide-emphasis-markers t) ; hide *bold*, /italic/ & _underline_ characters
(setq org-startup-indented t) ; start org with text indented to outline structure
(require 'org-inlinetask) ; "C-c C-x t" embed todo without treating it as an outline heading
;; customize org level colors (M-x list-colors-display to see colors list)
;; (custom-theme-set-faces 'user
;;                         `(org-level-3 ((t (:foreground "black"))))
;;                      `(org-level-4 ((t (:foreground "dim gray"))))
;;                      `(org-level-5 ((t (:foreground "dark gray")))))
;; Tab Indentation in Org Mode
(defun untabify-whole-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))
;; (add-hook 'org-mode-hook
;;           (add-hook 'evil-insert-state-exit-hook 'untabify-whole-buffer))
;; Enable M-e to open counsel-find-file
;; (define-key org-mode-map (kbd "M-e") 'counsel-find-file)
(with-eval-after-load 'org-mode
  (define-key org-mode-map (kbd "M-e") 'counsel-find-file))
;; Bimodal TAB-bing
;; You need to manually enable this for a header using M-x my/toggle-bimodal-cycling
;; (advice-add 'org-cycle :around #'my/org-cycle)
;; (defun my/toggle-bimodal-cycling (&optional pos)
;;   "Enable/disable bimodal cycling behavior for the current heading."
;;   (interactive)
;;   (let* ((enabled (org-entry-get pos "BIMODAL-CYCLING")))
;;     (if enabled
;;         (org-entry-delete pos "BIMODAL-CYCLING")
;;       (org-entry-put pos "BIMODAL-CYCLING" "yes"))))
;; (defun my/org-cycle (fn &optional arg)
;;   "Make org outline cycling bimodal (FOLDED and SUBTREE) rather than trimodal (FOLDED, CHILDREN, and SUBTREE) when a heading has a :BIMODAL-CYCLING: property value."
;;   (interactive)
;;   (if (and (org-at-heading-p)
;;            (org-entry-get nil "BIMODAL-CYCLING"))
;;       (my/toggle-subtree)
;;     (funcall fn arg)))
;; (defun my/toggle-subtree ()
;;   "Show or hide the current subtree depending on its current state."
;;   (interactive)
;;   (save-excursion
;;     (outline-back-to-heading)
;;     (if (not (outline-invisible-p (line-end-position)))
;;         (outline-hide-subtree)
;;       (outline-show-subtree))))

;; Dired-tips
;; C-x C-q to edit dired buffer, C-c C-c to save changes or C-c ESC to abort

;; Dired mode
(require 'dired-x)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(define-key dired-mode-map (kbd "-") 'dired-up-directory)
(defun my-dired-keybinds ()
  (local-set-key (kbd "C-n") (kbd "3 n"))
  (local-set-key (kbd "C-p") (kbd "3 p"))
    "make q terminate dired buffer; previously bound to quit-window"
  (local-set-key (kbd "q") (kbd "C-x k RET")))
(add-hook 'dired-mode-hook 'my-dired-keybinds)
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
              (seq bol "." (not (any "."))) ;; dot-files
              (seq "~" eol)                 ;; backup-files
              (seq bol "CVS" eol)           ;; CVS dirs
              )))
(setq dired-omit-extensions
      (append dired-latex-unclean-extensions
              dired-bibtex-unclean-extensions
              dired-texinfo-unclean-extensions))
;; Dired-omit-mode (comes with dired-x)
;; (setq dired-omit-mode t) ; guess this is not important
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; Evil mode
(require 'evil)
(evil-mode 1)
;; QoL
(define-key evil-motion-state-map (kbd "C-a") 'View-scroll-half-page-backward)
(define-key evil-motion-state-map (kbd "C-d") 'View-scroll-half-page-forward)
(define-key evil-motion-state-map (kbd "C-j") (kbd "5 gj"))
(define-key evil-motion-state-map (kbd "C-k") (kbd "5 gk"))
(define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "H") 'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map (kbd "L") 'evil-end-of-visual-line)
(define-key evil-motion-state-map (kbd "TAB") 'evil-jump-item)
(define-key evil-normal-state-map (kbd "M-d M-d") (kbd "\"_dd"))
(define-key evil-visual-state-map (kbd "M-d") (kbd "\"_d"))
;; Jumplist
;; (define-key evil-motion-state-map (kbd "C-u") 'evil-jump-forward)
;; Buffer Management
(define-key evil-normal-state-map [?\d] 'evil-switch-to-windows-last-buffer) ;del(backspace) to C-6
;; Window Management
(define-key evil-motion-state-map (kbd "C-w C-q") 'evil-window-delete)
(define-key evil-motion-state-map (kbd "C-w C-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "C-w C-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "C-w C-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "C-w C-l") 'evil-window-right)
;; Insert Mode
(define-key evil-insert-state-map (kbd "C-a") 'back-to-indentation)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-b") 'backward-char)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-f") 'forward-char)
(define-key evil-insert-state-map (kbd "M-f") 'forward-word)
(define-key evil-insert-state-map (kbd "M-b") 'backward-word)
(define-key evil-insert-state-map (kbd "M-d") 'kill-word)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
;; C-y Yank
(define-key evil-insert-state-map (kbd "C-y") 'yank)
(define-key evil-insert-state-map (kbd "C-k") (delete-region (point) (line-end-position)))
;; Misc
(defun evil-unimpaired/insert-space-above (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))
(defun evil-unimpaired/insert-space-below (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))
(define-key evil-normal-state-map (kbd "[ SPC") 'evil-unimpaired/insert-space-above)
(define-key evil-normal-state-map (kbd "] SPC") 'evil-unimpaired/insert-space-below)
(define-key evil-insert-state-map (kbd "H-v") (kbd "C-r +"))
(define-key evil-normal-state-map (kbd "H-v") (kbd "] SPC j \"+p"))
(defun my-save-if-bufferfilename (&rest args)
  (when (buffer-file-name)
    (save-buffer)))
(add-hook 'evil-insert-state-exit-hook 'my-save-if-bufferfilename) ;save on exiting insert mode
;; M-x list-colors-display
(setq evil-emacs-state-cursor '("red" (bar . 2))) ;set evil-emacs-mode cursor to a red bar
(setq evil-normal-state-cursor '("forest green")) ; box
(setq evil-insert-state-cursor '("black" (bar . 2))) ; bar

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "H-w") 'ace-window))
;; Avy is installed as an ace-window dependency
(global-set-key (kbd "H-d") 'avy-goto-char-2)
(define-key evil-normal-state-map (kbd "s") 'evil-avy-goto-char-2-below)
(define-key evil-normal-state-map (kbd "S") 'evil-avy-goto-char-2-above)

;; (use-package key-chord
;;   :config
;;   (key-chord-mode 1))
;; (key-chord-define evil-insert-state-map (kbd "jk") 'evil-normal-state)

(use-package magit
  :ensure t)

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list '("○" "⁘" "‣" "•" "◦" "◦"))
  :hook (org-mode . org-bullets-mode))
  ;; :config
  ;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;; ○•◦‣◇⁘

(use-package mixed-pitch
  :ensure t
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode)
  (org-mode . mixed-pitch-mode))

(use-package adaptive-wrap
  :ensure t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package smex
  :ensure t) ; smex gives ivy history

(require 'writeroom-mode)
;; (writeroom-set-fullscreen 'maximized)
;; (writeroom-set-vertical-scroll-bars 1)
(setq writeroom-width 0.65)

(require 'vdiff)
(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)

;; Ivy mode (A superior Ido)
(ivy-mode 1)
(setq ivy-use-virtual-buffers nil) ; set true if you want to see recent files and bookmarks
(setq ivy-count-format "(%d/%d) ")
(setq ivy-ignore-buffers '("\\` " "\\`\\*"))
(setq enable-recursive-minibuffers t)
;; abo-abo's custom swiper config
;; (global-set-key (kbd "C-s") 'ora-swiper)
(global-set-key "\C-s" 'ora-swiper)
(defun ora-swiper ()
  (interactive)
  (if (and (buffer-file-name)
           (not (ignore-errors
                  (file-remote-p (buffer-file-name))))
           (if (eq major-mode 'org-mode)
               (> (buffer-size) 60000)
             (> (buffer-size) 300000)))
      (progn
        (save-buffer)
        (counsel-grep))
    (swiper--ivy (swiper--candidates))))
;; Ivy-based interface to standard commands
;; (global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-e") 'counsel-find-file)
(global-set-key (kbd "<f2> f") 'counsel-describe-function)
(global-set-key (kbd "<f2> v") 'counsel-describe-variable)
(global-set-key (kbd "<f2> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "M-`") 'counsel-apropos)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c f") 'counsel-recentf)
;; Counsel Kill-Ring
(global-set-key (kbd "M-y") 'counsel-yank-pop)
;; Hide . & .. in counsel-find-file
;; (setq ivy-extra-directories ("../" "./"))
;; Ivy Minibuffer keybinds IMK
(defun ivy--cd-home ()
  (interactive)
  (ivy--cd "~/"))
(define-key ivy-minibuffer-map (kbd "M-h M-h") 'ivy--cd-home)
(defun ivy--cd-emacsd ()
  (interactive)
  (ivy--cd "~/.emacs.d/"))
(define-key ivy-minibuffer-map (kbd "M-e M-m M-a") 'ivy--cd-emacsd)
(defun ivy--cd-dropbox ()
  (interactive)
  (ivy--cd "~/Dropbox/"))
(define-key ivy-minibuffer-map (kbd "M-d M-b M-x") 'ivy--cd-dropbox)
(defun ivy--cd-dropbox-documents ()
  (interactive)
  (ivy--cd "~/Dropbox/Documents/"))
(define-key ivy-minibuffer-map (kbd "M-d M-d M-c") 'ivy--cd-dropbox-documents)
(defun ivy--cd-documents ()
  (interactive)
  (ivy--cd "~/Documents/"))
(define-key ivy-minibuffer-map (kbd "M-d M-o M-c") 'ivy--cd-documents)
(defun ivy--cd-desktop ()
  (interactive)
  (ivy--cd "~/Desktop/"))
(define-key ivy-minibuffer-map (kbd "M-d M-k") 'ivy--cd-desktop)
(defun ivy--cd-downloads ()
  (interactive)
  (ivy--cd "~/Downloads/"))
(define-key ivy-minibuffer-map (kbd "M-d M-w") 'ivy--cd-downloads)
(defun ivy--cd-org-ee2024 ()
  (interactive)
  (ivy--cd "~/OrgFiles/OrgNotes2024/"))
(define-key ivy-minibuffer-map (kbd "M-e M-4") 'ivy--cd-org-ee2024)
(defun ivy--cd-org-ee2027 ()
  (interactive)
  (ivy--cd "~/OrgFiles/OrgNotes2027/"))
(define-key ivy-minibuffer-map (kbd "M-e M-7") 'ivy--cd-org-ee2027)
(defun ivy--cd-org-pc1432 ()
  (interactive)
  (ivy--cd "~/OrgFiles/OrgNotes1432/"))
(define-key ivy-minibuffer-map (kbd "M-1 M-4") 'ivy--cd-org-pc1432)
(defun ivy--cd-org-st2334 ()
  (interactive)
  (ivy--cd "~/OrgFiles/OrgNotes2334/"))
(define-key ivy-minibuffer-map (kbd "M-s M-t") 'ivy--cd-org-st2334)
;; C-RET also runs RET
;; (define-key ivy-minibuffer-map (kbd "C-return") 'ivy-done)
;; (define-key ivy-minibuffer-map (kbd "C-return") 'ivy-done)
;; (global-set-key (kbd "C-return") 'ivy-done)

;; TRAMP
(setq tramp-default-method "ssh")

;; feebleline
(setq feebleline-use-legacy-settings t)
;; (window-divider-mode t)
;; (setq window-divider-default-bottom-width 1)
;; (setq window-divider-default-places (quote bottom-only))
;; (setq feebleline-mode-line-text
;;       '(("%6s"      ((format "%s,%s" (format-mode-line "%l") (current-column))))
;;         (" : %s"    ((if (buffer-file-name) (buffer-file-name)
;;                        (buffer-name))) (face feebleline-bufname-face))
;;         ("%s"       ((if (and (buffer-file-name) (buffer-modified-p)) "*" "" ))
;;          (face feebleline-asterisk-face))
;;         (" | %s"    ((feebleline-previous-buffer-name))
;;          (face feebleline-previous-buffer-face))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#454545" "#cd5542" "#6aaf50" "#baba36" "#5180b3" "#ab75c3" "#68a5e9" "#bdbdb3"])
 '(custom-safe-themes
   (quote
    ("c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" default)))
 '(mode-line-format nil)
 '(package-selected-packages
   (quote
    (vdiff-magit writeroom-mode use-package smex org-bullets mixed-pitch magit evil counsel ample-theme adaptive-wrap ace-window)))
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-places (quote bottom-only))
 '(writeroom-global-effects
   (quote
    (writeroom-set-alpha writeroom-set-menu-bar-lines writeroom-set-tool-bar-lines writeroom-set-bottom-divider-width))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:foreground "midnight blue" :height 1.7))))
 '(org-level-1 ((t (:foreground "navy" :height 1.2))))
 '(org-level-2 ((t (:foreground "dark slate blue"))))
 '(org-level-3 ((t (:foreground "black"))))
 '(org-level-4 ((t (:foreground "black" :height 0.97))))
 '(org-level-5 ((t (:foreground "dim gray" :height 0.95))))
 '(org-level-6 ((t (:foreground "dark gray" :height 0.95)))))
