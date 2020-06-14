(setq elgantt-draw--top-left "╭"
      elgantt-draw--top-right "╮"
      elgantt-draw--bottom-left "╰"
      elgantt-draw--bottom-right "╯"
      elgantt-draw--horizontal-line "─"
      elgantt-draw--vertical-line "│")

(defun elgantt-draw--clear-compositions ()
  (remove-text-properties (point-min) (point-max) '(display t)))

(defun elgantt-draw--delete-composition-char ()
  (let ((length (second
		 (plist-get (text-properties-at (point)) 'composition))))
    (delete-char (or length 1))))

(defun elgantt--draw-on-top (new-char)
  (let ((char (buffer-substring-no-properties (point) (1+ (point))))
	(old-string (org-no-properties (get-text-property (point) 'display))))
    (put-text-property (point) (1+ (point)) 'display
		       (compose-string (concat char new-char old-string)))))


(elgantt-create-display-rule line-draw
  :parser ((elgantt-line-to . ((org-entry-get (point) "ELGANTT-LINE-TO"))))
  :args (elgantt-org-id)
  :disable t
  :body ((when elgantt-line-to)))

(defun xxx ()
  (when-let ((elgantt-line-to (when (car (elgantt-get-prop-at-point :elgantt-line-to))
				(s-split " " (car (elgantt-get-prop-at-point :elgantt-line-to)))))
	     (elgantt-org-id (car (elgantt-get-prop-at-point :elgantt-org-id))))
    (--map 
     (save-excursion
       (let* ((start (cons (current-column) (line-number-at-pos)))
	      (end (cons (progn (elgantt--goto-id it)
				(current-column))
			 (line-number-at-pos)))
	      (sorted (cond ((<= (car start) (car end))
			     (list start end))
			    ((> (car start) (car end))
			     (list end start))))
	      (x-distance (- (caadr sorted)
			     (caar sorted)))
	      (y-distance (- (cdadr sorted)
			     (cdar sorted))))
	 (goto-char (point-min))
	 (forward-line (1- (cdar sorted)))
	 (move-to-column (caar sorted))
	 (cl-loop for x from 1 to (/ x-distance 2)
		  do (progn (unless (= x 1)
			      (elgantt--draw-on-top elgantt-draw--horizontal-line))
			    (forward-char (if (> 0 x-distance) -1 1))))
	 (cl-loop for y from 1 to (abs y-distance)
		  do (progn (if (= y 1)
				(elgantt--draw-on-top (if (> 0 y-distance)
							  elgantt-draw--bottom-right
							elgantt-draw--top-right))
			      (elgantt--draw-on-top elgantt-draw--vertical-line))
			    (elgantt--forward-line (if (> 0 y-distance) -1 1))))
	 (cl-loop for x from 1 to (+ (/ x-distance 2)
				     (% x-distance 2))
		  do (progn (if (= x 1)
				(elgantt--draw-on-top (cond ((> 0 y-distance)
							     elgantt-draw--top-left)
							    ((= 0 y-distance)
							     elgantt-draw--horizontal-line)
							    (t
							     elgantt-draw--bottom-left)))
			      (elgantt--draw-on-top elgantt-draw--horizontal-line))
			    (forward-char (if (> 0 x-distance) -1 1))))))
     elgantt-line-to)))


(cl-loop for x from 0 to -4
	 do (insert (format "%d" x)))



(--map (if it (insert it)) (-list "adsf" nil))
