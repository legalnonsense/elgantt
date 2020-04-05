;;;  -*- lexical-binding: t; -*-
(require 'assoc)
(require 'cl-lib)
(require 'color)
(require 'org)
(require 's)
(require 'dash)
(require 'ts)

(defun elgantt-parse::convert-date-string (date-string)
  (ts-format "%Y-%m-%d" (ts-parse-org date-string)))

(defun elgantt-parse::parse-this-headline ()
  "Return a property list with all properties available.
Function should be called with POINT at the first headline.
PROPS are additional text properties to append."
  (let ((xxx (append
	      (list :org-buffer (buffer-name))
	      (cadr (org-element-at-point))
	      (elgantt-parse::get 'elgantt-header)
	      (elgantt-parse::get 'elgantt-data))))
    (when (plist-get xxx :elgantt-date)
      (elgantt-cal::insert-entry))))


(setq elgantt:header-type 'root)
(defun elgantt::get-props (&rest props)
  "This is meant as a utility for getting information about an org headline at point.
  Its return value for any inquiry is in the form of (:property value). All return
  values are set as the text properties of the calendar cell. All property names
  are prefixed with `elgantt' to avoid collision with other properties. 
  Accepts any of the following arguments: `root', `timestamp', 
`timestamp-ia', `category', `hashtag', `timestamp-range', `timestamp-ia-range',
`deadline', `todo', `alltags', `elgantt-data', `scheduled', `file', `headline', 
`elgantt-header'."
  (let* ((category (cdr (assoc "CATEGORY" (org-entry-properties (point) "CATEGORY"))))
	 (prop-list
	  `((category . (lambda () (cons 'category ,category)))
	    (root . (lambda () (list :root
				     (save-excursion 
				       (while (org-up-heading-safe))
				       (cdar (org-entry-properties (point) "ITEM"))))))
	    (todo . (lambda () (list :todo 
				     (cdr (car (org-entry-properties (point) "TODO"))))))
	    (file . (lambda () (list :file 
				     (cdr (car (org-entry-properties (point) "FILE"))))))
	    (headline . (lambda () (list :headline
					 (cdar (org-entry-properties (point) "ITEM")))))
	    (timestamp . (lambda ()
			   (list :timestamp
				 (when-let ((timestamp (cdar (org-entry-properties (point) "TIMESTAMP"))))
				   (cond ((string= timestamp
						   ,category)
					  nil)
					 ((s-match "--" timestamp)
					  nil)
					 (t
					  (elgantt-parse::convert-date-string timestamp)))))))
	    (timestamp-ia . (lambda ()
			      (list :timestamp-ia
				    (when-let ((timestamp-ia (cdar (org-entry-properties (point) "TIMESTAMP_IA"))))
				      (cond ((string= timestamp-ia
						      ,category)
					     nil)
					    ((s-match "--" timestamp-ia)
					     nil)
					    (t
					     (elgantt-parse::convert-date-string timestamp-ia)))))))
	    (timestamp-range . (lambda ()
				 (list :timestamp-range
				       (when-let ((range (cadr (org-entry-properties (point) "TIMESTAMP"))))
					 (cond ((string= range ,category)
						nil)
					       ((not (s-match "--" range))
						nil)
					       (t
						(let ((dates (s-split "--" range)))
						  (list (elgantt-parse::convert-date-string (car dates)) (elgantt-parse::convert-date-string (cadr dates))))))))))
	    (timestamp-ia-range . (lambda () (list :timestamp-ia-range
						   (when-let ((range (cadr (org-entry-properties (point) "TIMESTAMP_IA"))))
						     (cond ((string= range ,category)
							    nil)
							   ((not (s-match "--" range))
							    nil)
							   (t
							    (let ((dates (s-split "--" range)))
							      (list (elgantt-parse::convert-date-string (car dates)) (elgantt-parse::convert-date-string (cadr dates))))))))))
	    (deadline . (lambda () (list :deadline 
					 (when (cdr (car (org-entry-properties (point) "DEADLINE")))
					   (if (string= (cdr (car (org-entry-properties (point) "DEADLINE"))) ,category)
					       nil
					     (elgantt-parse::convert-date-string (cdr (car (org-entry-properties (point) "DEADLINE")))))))))
	    (hashtag . (lambda ()
			 (list :hashtag
			       (when-let ((tag-string (cdar (org-entry-properties (point) "ALLTAGS"))))
				 (-first (lambda (tagstring) (s-starts-with-p "#" tagstring))
					 (s-split ":" tag-string))))))
	    (scheduled . (lambda () (list :scheduled
					  (when (cdr (car (org-entry-properties (point) "SCHEDULED")))
					    (if (string= (cdr (car (org-entry-properties (point) "SCHEDULED"))) ,category)
						nil
					      (elgantt-parse::convert-date-string (cdr (car (org-entry-properties (point) "SCHEDULED")))))))))
	    (alltags . (lambda () (list :alltags
					(let ((org-trust-scanner-tags t))
					  (when-let ((tag-string (cdar (org-entry-properties (point) "ALLTAGS"))))
					    (s-split ":" tag-string t))))))
	    (elgantt-header . (lambda () (pcase elgantt:header-type
					   ('root (list :elgantt-header (cadr (funcall (alist-get 'root prop-list)))))
					   ('hashtag (list :elgantt-header (cadr (funcall (alist-get 'hashtag prop-list)))))
					   ('category (list :elgantt-header ,category))
					   (_ (error "Invalid header type.")))))
	    (elgantt-data . (lambda () (let ((label nil)
					     (start-or-end-or-range nil)
					     (date nil)
					     (type nil)
					     (header nil))
					 ;; (when (cdar (org-entry-properties (point) "ALLTAGS"))
					 ;; 	 (dolist (tag (s-split ":" (cdar (org-entry-properties (point) "ALLTAGS"))))
					 ;; 	   (when (or (s-ends-with-p "_start" tag) (s-ends-with-p "_end" tag) (s-ends-with-p "_block" tag))
					 ;; 	     (setq label (car (s-split "_" tag)))
					 ;; 	     (setq start-or-end-or-range (cadr (s-split "_" tag)))
					 ;; 	     (if (string= start-or-end-or-range "block")
					 ;; 		 (cond ((elgantt-parse::get 'timestamp-range)
					 ;; 			(setq date (cadr (elgantt-parse::get 'timestamp-range))))
					 ;; 		       ((elgantt-parse::get 'timestamp-ia-range)
					 ;; 			(setq date (cadr (elgantt-parse::get 'timestamp-ia-range)))))
					 (cond ((cadr (funcall (alist-get 'deadline prop-list)))
						(setq type 'deadline)
						(setq date (cadr (elgantt-parse::get 'deadline))))
					       ((cadr (funcall (alist-get 'timestamp prop-list)))
						(setq type 'timestamp)
						(setq date (cadr (elgantt-parse::get 'timestamp))))
					       ((cadr (funcall (alist-get 'timestamp-ia prop-list)))
						(setq type 'timestamp-ia)
						(setq date (cadr (elgantt-parse::get 'timestamp-ia))))
					       ((cadr (elgantt-parse::get 'scheduled))
						(setq type 'scheduled)
						(setq date (cadr (elgantt-parse::get 'scheduled)))))
					 (list :elgantt-start-end-range start-or-end-or-range :elgantt-label label :elgantt-date date :elgantt-type type)))))))
    (cl-flet ((get (p) (funcall (alist-get p prop-list))))
      (cl-loop for prop in (or props
			       (mapcar #'car prop-list))
	 collect (get prop)))))






(defun elgantt-parse::get-years (&optional date-type)
  "Get the date range of all time values in all agenda files. 
Optional DATE-TYPE is any value (or list of values) accepted by `org-re-timestamp':
        all: all timestamps
     active: only active timestamps (<...>)
   inactive: only inactive timestamps ([...])
  scheduled: only scheduled timestamps
   deadline: only deadline timestamps
     closed: only closed time-stamps
If it is not provided, the default is ('active inactive deadline)."
  (save-excursion
    (let ((years '()))
      (--each (-list elgantt:agenda-files)
	(with-temp-buffer
	  (insert-file-contents it)
	  (goto-char (point-min))
	  (--each (or (-list date-type)
		      '(all))
	    (goto-char (point-min))
	    (while (re-search-forward (org-re-timestamp it) nil t)
	      (push (substring (car (s-split "-" (match-string 0))) 1) years)))))
      (delete-dups years)
      (sort
       (mapcar (lambda (it)
		 (string-to-number it))
	       years)
       '<))))



;; (ert-deftest check-parsing ()
;;   "Test the parsing function"
