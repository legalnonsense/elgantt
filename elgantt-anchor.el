;; Anchor/dependent creation

(defun elgantt-org-create-anchor ()
  "Add :elgantt-anchor and :elgantt-dependents. For use in the underlying org file."
  ;; Prompt user for the anchor heading. Add an `org-id' to the 
  ;; anchor heading if necessary. Add the property `ELGANTT-ANCHOR'
  ;; to the current heading, which is the `org-id' of the anchor.
  ;; Add `ELGANTT-DEPENDENTS' to the anchor heading, which is a list
  ;; of ids which are anchored to the heading.
  (interactive)
  (let* ((current-heading-id (org-id-get-create))
	 (anchor-heading-id (progn (org-goto)
				   (org-id-get-create)))
	 (elgantt-dependents (elgantt--org-get-dependents)))
    (org-id-goto anchor-heading-id)
    (org-set-property "ELGANTT-DEPENDENTS"
		      (s-join " "
			      (if (member current-heading-id elgantt-dependents)
				  elgantt-dependents
				(push current-heading-id elgantt-dependents))))
    (org-id-goto current-heading-id)
    (org-set-property "ELGANTT-ANCHOR" anchor-heading-id)))

(defun elgantt--org-get-dependents ()
  "Return a list of dependent deadlines from an org buffer."
  (when-let ((anchors (cdar (org-entry-properties (point) "ELGANTT-DEPENDENTS"))))
    (s-split " " anchors)))

(defun elgantt--get-dependents (&optional props)
  "Get a list of dependents from the cell at point.
  If PROPS, get the dependent from those properties. If
  PROPS is `all', then get all dependents at point if there
  are multiple entries in the cell." 
  (when-let ((prop (or (when (eq props 'all)
			 (elgantt--select-entry 'all))
		       (when props
			 (list props))
		       (list (elgantt--select-entry))))
	     (dependents (mapcar (lambda (p)
				   (plist-get p :ELGANTT-DEPENDENTS))
				 prop)))
    (unless (eq (-non-nil dependents) nil)
      (s-split " " (cl-loop for dep in (-flatten dependents)
			    concat dep)))))

(defun elgantt--highlight-dependents ()
  (interactive)
  (elgantt--highlight-dependent-dates 'elgantt-dependent-highlight-face))

(defun elgantt--highlight-dependent-dates (face &optional props)
  "Apply FACE to all dependant dates of the current date at point."
  (save-excursion 
    (if-let ((dependents (elgantt--get-dependents props)))
	(progn (elgantt--create-overlay (point) (1+ (point)) 'face face
				    :elgantt-dependent-highlight t)
	       (mapc (lambda (dependent-id)
		       (elgantt--goto-id dependent-id)
		       (elgantt--create-overlay (point) (1+ (point)) 'face face
					    :elgantt-dependent-highlight t))
		     dependents))
      (elgantt--clear-elgantt-overlays))))

(defun elgantt--move-date-and-dependents (&optional backward props)
  "Move the current date and all anchored dates (and their dependents) forward by one days
  If BACKWARD is non-nil, move backward. PROPS is a plist of cell data; otherwise,
  use the cell at point and prompt the user if there are multiple entries in the cell."
  (interactive)
  (when-let* ((props (if backward
			 (elgantt--shift-date -1 props)
		       (elgantt--shift-date 1 props)))
	      (dependent-ids (elgantt--get-dependents props)))
    (mapc (lambda (dependent-id)
	    (save-excursion
	      (elgantt--goto-id dependent-id)
	      (let ((new-props (-first (lambda (x)
					 (-contains? x dependent-id))
				       (elgantt-get-prop-at-point))))
		(if backward
		    (elgantt--move-date-and-dependents 'backward new-props)
		  (elgantt--move-date-and-dependents nil new-props)))))
	  dependent-ids)))

(defun elgantt-move-date-and-dependents-forward ()
  (interactive)
  (elgantt--move-date-and-dependents))

(defun elgantt-move-date-and-dependents-backward ()
  (interactive)
  (elgantt--move-date-and-dependents 'backward))
