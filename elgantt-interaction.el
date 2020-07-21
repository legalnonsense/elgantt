 ;;;  -*- lexical-binding: t; -*-

(defface elgantt-interaction::message-bar-face '((t (:background "gray" :foreground "black")))
  "Message bar face.")

(define-key elgantt-mode-map (kbd "a") #'elgantt-interaction::start-action)

(setq elgantt-interaction::action-list nil)
(setq elgantt-interaction::selected-cells nil)
(setq elgantt-interaction::current-action nil)
(setq elgantt-interaction::message-overlay nil)

(defun elgantt-interaction::adjust-overlay ()
  (interactive)
  (when elgantt-interaction::message-overlay
    (let* ((start (window-hscroll))
	   (end (+ start (length (overlay-get elgantt-interaction::message-overlay
					      'display)))))
      (move-overlay elgantt-interaction::message-overlay
		    start
		    end))))

(defun elgantt-interaction::message-overlay (command &optional string)
  (pcase command
    ((or `delete `clear) (progn (ov-clear :elgantt-interaction-message)
				(setq elgantt-interaction::message-overlay nil)))
    ((or `set `create) (let* ((message (concat "INTERACTION MODE: " (symbol-name elgantt-interaction::current-action)
					       "  " string "  "
					       (number-to-string
						(length elgantt-interaction::selected-cells))
					       " of "
					       (number-to-string
						(elgantt-interaction::get-prop elgantt-interaction::current-action
									       :selection-number))))		       
			      (start (save-excursion (move-to-window-line 0) (point)))
			      (end (+ start (length message))))
			 (elgantt-interaction::message-overlay 'clear)
			 (setq elgantt-interaction::message-overlay
			       (make-overlay start end))
			 (overlay-put elgantt-interaction::message-overlay
				      'display message)
			 (overlay-put elgantt-interaction::message-overlay
				      :elgantt-interaction-message t)
			 (overlay-put elgantt-interaction::message-overlay
				      'face
				      'elgantt-interaction::message-bar-face)))))

(defun elgantt-interaction::get-message ()
  (let* ((number-selected (1+ (length elgantt-interaction::selected-cells)))
	 (message-stack (elgantt-interaction::get-prop elgantt-interaction::current-action
						       :selection-messages))
	 (places (cl-loop for place in message-stack
			  collect (car place)))
	 (max-selections (elgantt-interaction::get-prop elgantt-interaction::current-action
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

(defun elgantt-interaction::start-action ()
  (interactive)
  (setq elgantt-interaction::selected-cells nil)
  (setq elgantt-interaction::current-action nil)
  (when-let ((action (intern (completing-read "Select action: "
					      (mapcar (lambda (element)
							(symbol-name (car element)))
						      elgantt-interaction::action-list) 
					      nil t))))
    (setq elgantt-interaction::current-action action)
    (elgantt-interact-mode 1)
    (elgantt-interaction::message-overlay
     'set
     (elgantt-interaction::get-message))))


(defun elgantt-interaction::get-prop (name prop)
  (plist-get (alist-get name elgantt-interaction::action-list) prop))

(defun elgantt-interaction::add-cell-to-list ()
  (interactive)
  (if (<= (length elgantt-interaction::selected-cells) (elgantt-interaction::get-prop
						    elgantt-interaction::current-action
						    :selection-number))
      (progn 
	(setq elgantt-interaction::selected-cells
	      (append elgantt-interaction::selected-cells
		      `(,(elgantt--select-entry))))
	(elgantt-interaction::message-overlay 'set
					  (elgantt-interaction::get-message)))
    (message "Too many cells selected!")))

(defsubst elgantt-interaction::terminate ()
  (interactive)
  (elgantt-interact-mode -1))

(define-minor-mode elgantt-interact-mode
  "Mode to interact with calendar"
  nil
  "ELGANTT-INTERACT"
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map elgantt-mode-map)
    (define-key map (kbd "RET") #'elgantt-interaction::execute-action)
    (define-key map (kbd "q") #'elgantt-interaction::terminate)
    (define-key map (kbd "SPC") #'elgantt-interaction::add-cell-to-list)
    map)
  (if elgantt-interact-mode
      (progn (setq cursor-type 'hollow)
	     (elgantt-interaction::message-overlay 'delete)
	     (add-hook 'post-command-hook #'elgantt-interaction::adjust-overlay t t)
	     (setq elgantt-interaction::selected-cells nil))
    (setq cursor-type 'box)
    (elgantt-interaction::message-overlay 'delete)
    (setq elgantt-interaction::selected-cells nil)
    (remove-hook 'post-command-hook #'elgantt-interaction::adjust-overlay t)
    (setq elgantt-interaction::current-action nil)))

(cl-defmacro elgantt--selection-rule (&key name selection-number selection-messages execution-functions args parser)
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
					   elgantt--parsing-functions)
				`(lambda () ,@val))))
	    (cl-loop for (place . command) in execution-functions
		     do (push `(,place . (lambda (return-val)
					   (mapc
					    (lambda (arg-list)
					      (-let ((,(append (cl-loop for arg in args
									collect (elgantt--add-remove-prop-colon arg t))
							       (cl-loop for (prop . val) in parser
									collect (elgantt--add-remove-prop-colon prop t)))
						      arg-list))
						,@command))
					    (or 
					     (elg:zip
					      (mapcar #'elg:get-prop-at-point
						      (append (cl-loop for arg in ',args
								       collect (elgantt--add-remove-prop-colon arg))
							      (cl-loop for (prop . val) in ',parser
								       collect (elgantt--add-remove-prop-colon prop)))))
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
      `(setf (alist-get ',name elgantt-interaction::action-list) (list :execution-functions ',function-stack
								   :selection-number ,(if (= selection-number 0)
											  999
											selection-number)
								   :selection-messages ',selection-messages)))))

(defun elgantt-interaction::execute-action ()
  (interactive)
  (let ((function-stack (reverse (plist-get
				  (alist-get elgantt-interaction::current-action
					     elgantt-interaction::action-list)
				  :execution-functions)))
	return-val)
    (cl-loop for (place . function) in function-stack
	     do (pcase place
		  ((pred numberp) (progn (elgantt--goto-id (plist-get (nth (1- place) elgantt-interaction::selected-cells) :elgantt-org-id))
					 (setq return-val (funcall function return-val))))
		  (`all (mapc (lambda (cell)
				(elgantt--goto-id (plist-get cell :elgantt-org-id))
				(setq return-val (funcall function return-val))))
			elgantt-interaction::selected-cells)
		  (`rest (mapc (lambda (cell)
				 (elgantt--goto-id (plist-get elgantt-interaction::selected-cells :elgantt-org-id))
				 (funcall-function return-val))
			       (cdr elgantt-interaction::selected-cells)))
		  (`all-but-last (mapc (lambda (cell)
					 (elgantt--goto-id (plist-get cell :elgantt-org-id))
					 (setq return-val (funcall function return-val))))
				 (butlast elgantt-interaction::selected-cells))
		  (`last (progn (elgantt--goto-id (plist-get (last elgantt-interaction::selected-cells) :elgantt-org-id))
				(setq return-val (funcall function return-val))))))
    (elgantt-interact-mode -1)
    (elgantt-open)))





(provide 'elgantt-interaction)
