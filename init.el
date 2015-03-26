
;; NOTE:
;;  from time to time , especially when you add new lisp modules, 
;;  compile everything using this command:
;;  C-u 0 M-x byte-recompile-directory
;;  in order to speed up emacs' responsivity!!!
;;  It will compile all the .el files in the directory and in
;;  all subdirectories below.

;; TODO Vasiliy: 26.03.2015 - add some script for periodical auto compilation
;; TODO Vasiliy: 26.03.2015 - move all the custom functions into separate file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; LOAD PATH AND FILES ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Add my lisp dir to search path
(add-to-list 'load-path "~/.emacs.d/lisp")

; Path to look for Emacs themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

; Load customizations file (this file is auto created - do not edit it!)
(load "~/.emacs.d/emacs-custom.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; THEMES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'zenburn t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; EXTEND FUNCTIONALITY ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'highlight-symbol-autoloads) ; autoload highlight-symbol functions
 
(require 'transpose-frame) ; manipulating frames in a window

;; Load verilog mode only when needed
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )

;; Show the name of the function in the status panel
(which-function-mode 1)

;; Show the opening of the parenthesized block if the point is at its closing
;; and the beggining is not visible on screen
(show-paren-mode 1)

;; Enable ido-mode
;;(ido-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HOOKS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-hook 'isearch-update-post-hook                    ; execute show-whole-line after each update in isearch-mode
          'show-whole-line)  

(add-hook 'activate-mark-hook                          ; deactivate line highlight when region highlighted
          '(lambda () (global-hl-line-mode -1)))

(add-hook 'deactivate-mark-hook                        ; activate back line highlight when region unhighlighted
          '(lambda () (global-hl-line-mode 1)))



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; some function that i'm using here ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-whole-line ()
  "Scroll such that the whole line (which contains the point) will be visible.
This function is useful for interactive search when lines are truncated - it makes
sure that the entire content of the line containing the match will be visible."
  
  ;; If it is the top part which is truncated
  (if (not (pos-visible-in-window-p (line-beginning-position)))
      (let
          ((amount 
            ;; the required number of lines to scroll
            (ceiling (/
                      (- (window-start) 
                         (line-beginning-position))
                      (float (window-body-width))))))
        ;; don't scroll at all if the search result will be scrolled out
        (if (< amount (/ 
                       (- (window-end)
                          (point) )
                       (float (window-body-width))))
            (scroll-down amount)))
    
    ;; Else
    (if (not (pos-visible-in-window-p (line-end-position)))
        (let
            ((amount 
              (min 
               ;; the required number of lines to scroll
               (ceiling (/ 
                         (- 
                          (line-end-position) 
                          (window-end (selected-window) t)) 
                         (float (window-body-width))) )
               ;; however not to scroll out the first line
               (/ (- (line-beginning-position) (window-start)) (window-body-width)))))
          (scroll-up amount)))))

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

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


(defun insert-todo ()
  "Appednd 'TODO username: date - ' at the end of line and set point 
to where this string ends"
  (interactive)
  (end-of-line)
  (insert " " comment-start (save-excursion comment-end))   
  (insert (format " TODO %s: " (getenv "USER")) (format-time-string "%d.%m.%Y") " - "))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Tcl mode config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Also .upf files in tcl  mode
(add-to-list 'auto-mode-alist '("\\.upf\\'" . tcl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; verilog-mode configuration ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Any files that end in .v, .sv, .dv, .svh, .vh, etc. should be in verilog mode
(add-to-list 'auto-mode-alist '("\\.[s]?v[sh]?\\'" . verilog-mode))
;; Also .rdl and .srdl files in verilog mode
(add-to-list 'auto-mode-alist '("\\.[s]?rdl\\'" . verilog-mode))

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


(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map "\M-g"     'goto-line)
(define-key my-keys-minor-mode-map "\C-c\C-k" 'duplicate-line)
(define-key my-keys-minor-mode-map "\C-x\C-b" 'buffer-menu)
(define-key my-keys-minor-mode-map "\C-x\C-f" 'find-file-at-point)
(define-key my-keys-minor-mode-map "\C-xt"    'toggle-truncate-lines)
(define-key my-keys-minor-mode-map "\C-ct"    'transpose-frame)
(define-key my-keys-minor-mode-map "\C-z"     'undo)
(define-key my-keys-minor-mode-map "\C-cr"    'revert-buffer) 

(define-key my-keys-minor-mode-map [f4]       'insert-todo) ; add todo item at the end of line
(define-key my-keys-minor-mode-map "\C-xr"    'replace-string) 
(define-key my-keys-minor-mode-map "\C-x\C-r" 'replace-regexp) ; overwrites find-file-read-only, but I don't use it anyway

(define-key my-keys-minor-mode-map "\M-p"  '(lambda () (interactive) (scroll-down 4)))      ; scroll up, point not moving
(define-key my-keys-minor-mode-map "\M-n"  '(lambda () (interactive) (scroll-up 4)))    ; scroll down, point not moving

(define-key my-keys-minor-mode-map "\C-c\C-h" 'highlight-symbol-at-point)
(define-key my-keys-minor-mode-map "\C-c\C-n" 'highlight-symbol-next)
(define-key my-keys-minor-mode-map "\C-c\C-p" 'highlight-symbol-prev)


(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)


(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority over any other
minor mode."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)


(defun my-minibuffer-setup-hook ()
  "Disable my-keys-minor-mode in minibuffer"
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
