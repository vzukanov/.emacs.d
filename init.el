;;
;;
;; This file contains my Emacs customizations.
;; 
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; LOAD PATH AND FILES ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(when (eval-when-compile (version< emacs-version "27"))
  (package-initialize))

; Add my lisp dir to search path
(add-to-list 'load-path "~/.emacs.d/lisp")

; Load the file containing my functions
(load "~/.emacs.d/my-functions")

; Load customizations file (this file is auto created - do not edit it!)
(load "~/.emacs.d/emacs-custom")

; Path to look for Emacs themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

; Set frame size
(if (window-system) (set-frame-size (selected-frame) 124 40))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; THEMES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'zenburn t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; EXTEND FUNCTIONALITY ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'highlight-symbol-autoloads) ; autoload highlight-symbol functions
 
(require 'transpose-frame) ; manipulating frames in a window

(autoload 'web-mode "web-mode" "web mode" t )
(autoload 'js2-mode "js2-mode" "js2 mode" t )
(autoload 'yaml-mode "yaml-mode" "yaml mode" t)
(autoload 'kotlin-mode "kotlin-mode" "kotlin mode" t)


;; Show the name of the function in the status panel
(which-function-mode 1)

;; Disable menu bar
(menu-bar-mode 0)

;; Disable toolbar
(if (window-system)
    (tool-bar-mode 0))

;; Set centralized backup directory
(setq backup-directory-alist `(("." . "~/.emacs-backups")))

;; Perform backups by copy
(setq backup-by-copying t)

(setq delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t)

;; Show the opening of the parenthesized block if the point is at its closing
;; and the beggining is not visible on screen
;; (show-paren-mode 1)

;; Enable ido-mode
;;(ido-mode 1)

;; Don't use tabs for indentation
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; make scroll-all-mode work with mouse ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mwheel-scroll-all-function-all (func &optional arg)
  (if (and scroll-all-mode arg)
      (save-selected-window
        (walk-windows
         (lambda (win)
           (select-window win)
           (condition-case nil
               (funcall func arg)
             (error nil)))))
    (funcall func arg)))

(defun mwheel-scroll-all-scroll-up-all (&optional arg)
  (mwheel-scroll-all-function-all 'scroll-up arg))

(defun mwheel-scroll-all-scroll-down-all (&optional arg)
  (mwheel-scroll-all-function-all 'scroll-down arg))

(setq mwheel-scroll-up-function 'mwheel-scroll-all-scroll-up-all)
(setq mwheel-scroll-down-function 'mwheel-scroll-all-scroll-down-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; FILE EXTENSIONS ASSOCIATION ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Any files that end in .v, .sv, .dv, .svh, .vh, etc. should be in verilog mode
(add-to-list 'auto-mode-alist '("\\.[s]?v[sh]?\\'" . verilog-mode))

;; Also .rdl and .srdl files in verilog mode
(add-to-list 'auto-mode-alist '("\\.[s]?rdl\\'" . verilog-mode))

;; Also .upf files in tcl  mode
(add-to-list 'auto-mode-alist '("\\.upf\\'" . tcl-mode))

;; yml in Yaml mode
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

;; .kt in Kotlin mode
(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.s?css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HOOKS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (add-hook 'isearch-update-post-hook                    ; execute show-whole-line after each update in isearch-mode
;;           'show-whole-line)  

(add-hook 'activate-mark-hook                          ; deactivate line highlight when region highlighted
          '(lambda () (global-hl-line-mode -1)))

(add-hook 'deactivate-mark-hook                        ; activate back line highlight when region unhighlighted
          '(lambda () (global-hl-line-mode 1)))



(defadvice show-paren-function (after show-matching-paren-offscreen
                                      activate)
  "If the matching paren is offscreen, show the matching line in the 
echo area. Has no effect if the character before point is not of
the syntax class ')'."
  (interactive)
  (let ((matching-text nil))
    ;; Only call `blink-matching-open' if the character before point
    ;; is a close parentheses type character. Otherwise, there's not
    ;; really any point, and `blink-matching-open' would just echo
    ;; "Mismatched parentheses", which gets really annoying.
    (if (char-equal (char-syntax (char-before (point))) ?\))
        (setq matching-text (blink-matching-open)))
    (if (not (null matching-text))
        (message matching-text))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; CUSTOMIZATION OF STANDARD LOOK AND FEEL ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Set font only if in a window (not shell emacs)
(when (window-system) 
    (set-face-attribute 'default nil :font "DejaVu Sans Mono-12")    )

(setq inhibit-startup-message t)                       ; don't show welcome screen
(setq line-number-mode 1)                              ; show line number in status panel
(setq column-number-mode 1)                            ; show column number in status panel
(global-hl-line-mode t)                                ; hilight of current line
(setq mouse-drag-copy-region t)                        ; copy selection to kill-ring
(setq find-file-existing-other-name t)                 ; if you try to load the same file twice, you only get one copy of it

(setq scroll-step            1                         ; keyboard scrolling settings
      scroll-conservatively  10000)

(setq mouse-wheel-scroll-amount '(2 ((control))))      ; mouse wheel scrolling settings: 2 lines at a time, full window with CTRL
(setq mouse-wheel-progressive-speed nil) 

(setq frame-title-format                               ; set simple frame (window) title 
      '((:eval 
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))     

;; Disable sound bell and replace it with line flash
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

;; Swap Command and Option keys on macOS
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; kotlin-mode configuration ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default kotlin-tab-width 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; verilog-mode configuration ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Any files in verilog mode should have their keywords colorized
(add-hook 'verilog-mode-hook '(lambda () (font-lock-mode 1)))

;; Replace TABs with spaces in verilog mode
(add-hook 'verilog-mode-hook '(lambda ()
  (add-hook 'write-file-functions (lambda()
      (untabify (point-min) (point-max))
      nil))))

;; Suppress comments at the end of blocks which are shorter than 20 lines
(setq verilog-minimum-comment-distance 20)

;; Do not add a new line after ';' in verilog mode
(setq verilog-auto-newline nil)                        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; CPerl mode config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Indentation parameters
(add-hook 'cperl-mode-hook
          (lambda () (setq cperl-indent-level 4
                           cperl-close-paren-offset -4
                           cperl-continued-statement-offset 4
                           cperl-indent-parens-as-block t
                           cperl-tab-always-indent t)))

;; Replace TABs with spaces in CPerl mode
(add-hook 'cperl-mode-hook '(lambda ()
  (add-hook 'write-file-functions (lambda()
      (untabify (point-min) (point-max))
      nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Terminal settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq explicit-shell-file-name "/usr/bin/bash") ;; Use Bash as emacs shell

(add-hook 'term-mode-hook 
          '(lambda ()
             ;(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
             (make-local-variable 'mouse-yank-at-point)
             (make-local-variable 'transient-mark-mode)
             (make-local-variable 'global-hl-line-mode)
             (setq mouse-yank-at-point t)
             (setq transient-mark-mode nil)
             (setq global-hl-line-mode nil)
             (auto-fill-mode -1)
             (setq tab-width 8 )
             (set-syntax-table (let ((table (make-syntax-table)))
                                 (modify-syntax-entry ?_ "w" table)
                                 (modify-syntax-entry ?/ "w" table)
                                 (modify-syntax-entry ?. "w" table)
                                 (modify-syntax-entry ?~ "w" table)
                                 table))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; my key bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1) Define a sparse keymap (better than `make-keymap` for custom bindings)
(defvar my-keys-minor-mode-map (make-sparse-keymap)
  "Keymap for `my-keys-minor-mode`.")

;; Helpers for smooth scrolling without moving point
(defun my/scroll-up-4 ()   (interactive) (scroll-up   4))
(defun my/scroll-down-4 () (interactive) (scroll-down 4))

;; 2) Bindings (use `kbd` for readability/portability)
(define-key my-keys-minor-mode-map (kbd "M-g")       #'goto-line)
(define-key my-keys-minor-mode-map (kbd "C-c C-k")   #'duplicate-line)
(define-key my-keys-minor-mode-map (kbd "C-x C-b")   #'buffer-menu)
(define-key my-keys-minor-mode-map (kbd "C-x C-f")   #'find-file-at-point)
(define-key my-keys-minor-mode-map (kbd "C-x t")     #'toggle-truncate-lines)
(define-key my-keys-minor-mode-map (kbd "C-c t")     #'transpose-frame)
(define-key my-keys-minor-mode-map (kbd "C-z")       #'undo)
(define-key my-keys-minor-mode-map (kbd "C-c r")     #'revert-buffer)
(define-key my-keys-minor-mode-map (kbd "<f4>")      #'insert-todo)
(define-key my-keys-minor-mode-map (kbd "C-x r")     #'replace-string)
(define-key my-keys-minor-mode-map (kbd "C-x C-r")   #'replace-regexp)
(define-key my-keys-minor-mode-map (kbd "M-p")       #'my/scroll-down-4)
(define-key my-keys-minor-mode-map (kbd "M-n")       #'my/scroll-up-4)
(define-key my-keys-minor-mode-map (kbd "C-c C-h")   #'highlight-symbol-at-point)
(define-key my-keys-minor-mode-map (kbd "C-c C-n")   #'highlight-symbol-next)
(define-key my-keys-minor-mode-map (kbd "C-c C-p")   #'highlight-symbol-prev)

;; 3) Minor mode (global) with a lighter
(define-minor-mode my-keys-minor-mode
  "Global minor mode so my keybindings override most modes."
  :init-value t
  :global t
  :lighter " my-keys")

;; 4) Give it top priority *without* advice/reordering:
;;    `emulation-mode-map-alists` sits above all minor modes.
(add-to-list 'emulation-mode-map-alists
             `((my-keys-minor-mode . ,my-keys-minor-mode-map)))

;; 5) Disable in minibuffer (so it doesn't shadow minibuffer keys)
(defun my-keys--minibuffer-setup () (my-keys-minor-mode -1))
(defun my-keys--minibuffer-exit ()  (my-keys-minor-mode +1))
(add-hook 'minibuffer-setup-hook #'my-keys--minibuffer-setup)
(add-hook 'minibuffer-exit-hook  #'my-keys--minibuffer-exit)

;; 6) Enable the mode
(my-keys-minor-mode 1)

;; 7) Allow narrow/erase
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer     'disabled nil)
