 ;;;  -*- lexical-binding: t; -*-

(defface elg-interaction::message-bar-face '((t (:background "gray" :foreground "black")))
  "Message bar face.")

(setq elg-interaction::action-list nil)
(setq elg-interaction::selected-cells nil)
(setq elg-interaction::current-action nil)
(setq elg-interaction::message-overlay nil)

(defun elg-interaction::adjust-overlay ()
  (interactive)
  (when elg-interaction::message-overlay
    (let* ((start (window-hscroll))
	   (end (+ start (length (overlay-get elg-interaction::message-overlay
					      'display)))))
      (move-overlay elg-interaction::message-overlay
		    start
		    end))))

(defun elg-interaction::message-overlay (command &optional string)
  (pcase command
    ((or `delete `clear) (progn (ov-clear :elg-interaction-message)
				(setq elg-interaction::message-overlay nil)))
    ((or `set `create) (let* ((message (concat "INTERACTION MODE: " (symbol-name elg-interaction::current-action)
					       "  " string "  "
					       (number-to-string
						(length elg-interaction::selected-cells))
					       " of "
					       (number-to-string
						(elg-interaction::get-prop elg-interaction::current-action
									       :selection-number))))		       
			      (start (save-excursion (move-to-window-line 0) (point)))
			      (end (+ start (length message))))
			 (elg-interaction::message-overlay 'clear)
			 (setq elg-interaction::message-overlay
			       (make-overlay start end))
			 (overlay-put elg-interaction::message-overlay
				      'display message)
			 (overlay-put elg-interaction::message-overlay
				      :elg-interaction-message t)
			 (overlay-put elg-interaction::message-overlay
				      'face
				      'elg-interaction::message-bar-face)))))

(defun elg-interaction::get-message ()
  (let* ((number-selected (1+ (length elg-interaction::selected-cells)))
	 (message-stack (elg-interaction::get-prop elg-interaction::current-action
						       :selection-messages))
	 (places (cl-loop for place in message-stack
			  collect (car place)))
	 (max-selections (elg-interaction::get-prop elg-interaction::current-action
							:selection-number)))
    (cond ((memq 'all places)
	   (alist-get 'all message-stack))
	  ((memq number-selected places)
	   (alist-get number-selected message-stack))
	  ((and (= number-selected max-selections)
		(memq 'last places))
	   (alist-get 'last message-stack))
	  ((and (<= number-selected max-selections)
		(memq 'rest places))
	   (alist-get 'rest message-stack)))))

(defun elg-interaction::start-action ()
  (interactive)
  (setq elg-interaction::selected-cells nil)
  (setq elg-interaction::current-action nil)
  (when-let ((action (intern (completing-read "Select action: "
					      (mapcar (lambda (element)
							(symbol-name (car element)))
						      elg-interaction::action-list) 
					      nil t))))
    (setq elg-interaction::current-action action)
    (elg-interact-mode 1)
    (elg-interaction::message-overlay
     'set
     (elg-interaction::get-message))))


(defun elg-interaction::get-prop (name prop)
  (plist-get (alist-get name elg-interaction::action-list) prop))

(defun elg-interaction::add-cell-to-list ()
  (interactive)
  (if (<= (length elg-interaction::selected-cells) (elg-interaction::get-prop
						    elg-interaction::current-action
						    :selection-number))
      (progn 
	(setq elg-interaction::selected-cells
	      (append elg-interaction::selected-cells
		      `(,(elg--select-entry))))
	(elg-interaction::message-overlay 'set
					  (elg-interaction::get-message)))
    (message "Too many cells selected!")))

(defsubst elg-interaction::terminate ()
  (interactive)
  (elg-interact-mode -1))

(define-minor-mode elg-interact-mode
  "Mode to interact with calendar"
  nil
  "ELG-INTERACT"
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map elg-mode-map)
    (define-key map (kbd "RET") #'elg-interaction::execute-action)
    (define-key map (kbd "q") #'elg-interaction::terminate)
    (define-key map (kbd "SPC") #'elg-interaction::add-cell-to-list)
    map)
  (if elg-interact-mode
      (progn (setq cursor-type 'hollow)
	     (elg-interaction::message-overlay 'delete)
	     (add-hook 'post-command-hook #'elg-interaction::adjust-overlay t t)
	     (setq elg-interaction::selected-cells nil))
    (setq cursor-type 'box)
    (elg-interaction::message-overlay 'delete)
    (setq elg-interaction::selected-cells nil)
    (remove-hook 'post-command-hook #'elg-interaction::adjust-overlay t)
    (setq elg-interaction::current-action nil)))

(cl-defmacro elg--selection-rule (&key name selection-number selection-messages execution-functions args parser)
  ;; There is no need for this to be a macro, except to avoid quoting in the
  ;; call. 
  (when execution-functions
    (let (function-stack)
      (if (or parser args)
	  (progn 
	    (when parser
	      (cl-loop for (prop . val) in (-list parser)
		       do (setf (alist-get (if (s-starts-with-p ":" (symbol-name prop))
					       prop
					     (intern (concat ":" (symbol-name prop))))
					   elg--parsing-functions)
				`(lambda () ,@val))))
	    (cl-loop for (place . command) in execution-functions
		     do (push `(,place . (lambda (return-val)
					   (mapc
					    (lambda (arg-list)
					      (-let ((,(append (cl-loop for arg in args
									collect (elg--add-remove-prop-colon arg t))
							       (cl-loop for (prop . val) in parser
									collect (elg--add-remove-prop-colon prop t)))
						      arg-list))
						,@command))
					    (or 
					     (elg:zip
					      (mapcar #'elg:get-prop-at-point
						      (append (cl-loop for arg in ',args
								       collect (elg--add-remove-prop-colon arg))
							      (cl-loop for (prop . val) in ',parser
								       collect (elg--add-remove-prop-colon prop)))))
					     ;; If the preceding code returns `nil', then the `mapc' function, above,
					     ;; will not run. Since `elg:get-prop-at-point' will usually return nil
					     ;; if on an empty cell, it creates a problem if the user wants to run
					     ;; the command in an empty cell. 
					     ;; To avoid this, if `elg:zip' returns nil, this will create a list of nils to
					     ;; be assigned to the argument list, since nil is not `eq' to (nil),
					     ;; `mapc' will accept the list and run.
					     (make-list (+ (length ',parser) (length ',args)) nil)))))
			      function-stack)))
	(cl-loop for (place . command) in execution-functions
		 do (push `(,place . (lambda (return-val) ,@command)) function-stack)))
      `(setf (alist-get ',name elg-interaction::action-list) (list :execution-functions ',function-stack
								       :selection-number ,(if (= selection-number 0)
											      999
											    selection-number)
								       :selection-messages ',selection-messages)))))

(defun elg-interaction::execute-action ()
  (interactive)
  (let ((function-stack (reverse (plist-get
				  (alist-get elg-interaction::current-action
					     elg-interaction::action-list)
				  :execution-functions)))
	return-val)
    (cl-loop for (place . function) in function-stack
	     do (pcase place
		  ((pred numberp) (progn (elg--goto-id (plist-get (nth (1- place) elg-interaction::selected-cells) :ID))
					 (setq return-val (funcall function return-val))))
		  (`all (mapc (lambda (cell)
				(elg--goto-id (plist-get cell :ID))
				(setq return-val (funcall function return-val))))
			elg-interaction::selected-cells)
		  (`rest (mapc (lambda (cell)
				 (elg--goto-id (plist-get elg-interaction::selected-cells :ID))
				 (funcall-function return-val))
			       (cdr elg-interaction::selected-cells)))
		  (`all-but-last (mapc (lambda (cell)
					 (elg--goto-id (plist-get cell :ID))
					 (setq return-val (funcall function return-val))))
				 (butlast elg-interaction::selected-cells))
		  (`last (progn (elg--goto-id (plist-get (last elg-interaction::selected-cells) :ID))
				(setq return-val (funcall function return-val))))))
    (elg--update-display-all-cells)
    (elg-interact-mode -1)))

(elg--selection-rule :name colorize
		     :execution-functions ((2 . ((elg-with-point-at-orig-entry nil
						     (org-set-property "ELG-COLOR" (s-trim (helm-colors)))
						   (org-id-get-create))))
					   (1 . ((elg-with-point-at-orig-entry nil
						     (org-set-property "ELG-LINKED-TO" return-val)
						   (org-set-property "ELG-COLOR" (s-trim (helm-colors)))))))
		     :selection-messages ((1 . "Select cell and color")
					  (2 . "Select cell and color"))
		     :selection-number 2)




;; (elg--selection-rule :name set-anchor
;; 		     :parser ((:elg-dependents . ((when-let ((dependents (cdar (org-entry-properties (point)
;; 												     "ELG-DEPENDENTS"))))
;; 						    (s-split " " dependents)))))
;; 		     :execution-functions ((2 . ((elg:with-point-at-orig-entry nil
;; 									       (org-id-get-create))))
;; 					   (1 . ((elg:with-point-at-orig-entry nil
;; 									       (let ((current-heading-id (org-id-get-create)))
;; 										 (org-set-property "ELG-DEPENDENTS"
;; 												   (format "%s"
;; 													   (substring 
;; 													    (if (elg--mem-s= return-val elg-dependents)
;; 														elg-dependents
;; 													      (push return-val elg-dependents))
;; 													    1 -1)))))))
;; 					   (2 . ((elg:with-point-at-orig-entry nil
;; 									       (org-set-property "ELG-ANCHOR" return-val)))))
;; 		     :selection-messages ((1 . "Select the anchor.")
;; 					  (rest . "Select the dependents."))
;; 		     :selection-number 0)
