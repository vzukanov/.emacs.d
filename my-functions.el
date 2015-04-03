;;
;;
;; This file contains my own functions that I use when working with Emacs
;;
;;


(defun show-whole-line ()
  "Scroll such that the whole line (which contains the point) will be visible.
This function is useful for interactive search when lines are truncated - it makes
sure that the entire content of the line containing the match will be 
visible (if it is possible given the line's length and buffer's size)"
  
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
  "Duplicate current line (possibly multiple times), leaving point in lower line."
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


(defun insert-todo ()
  "Appednd 'TODO username: date - ' at the end of line and set point 
to where this string ends"
  (interactive)
  (end-of-line)
  (insert " " comment-start (save-excursion comment-end))   
  (insert (format " TODO %s: " (getenv "USER")) (format-time-string "%d.%m.%Y") " - "))


