
;; 1. check if entry has a date
;; 2. check to see if it has a heading already
;; 3. a. if it has a heading, goto the beginning of that line
;;    b. otherwise, insert a new heading
;; 4. insert the entry by changing the text properties at point

(defsubst elgantt-parse::convert-date-string-to-ts (date-string)
  (ts-parse-org date-string))

(defun elgantt-parse::parse-this-headline (&optional props)
  (-flatten-n 1
	      (mapcar #'elgantt-parse::get (or (-list props)
					       '(root
						 timestamp
						 timestamp-ia
						 timestamp-range
						 timestamp-ia-range
						 deadline
						 scheduled
						 file
						 todo
						 hashtag
						 category
						 elgantt-data
						 alltags)))))

(defun elgantt-parse::get (prop)
  "Accepts any of the following arguments: `root', `timestamp', 
`timestamp-ia', `category', `hashtag', `timestamp-range', `timestamp-ia-range',
`deadline', `todo', `alltags', `elgantt-data', `scheduled' and `file'. 
PROP may also be a function to get heading information, 
which is claled with POINT at the first point of the org headline."
  (pcase prop
    ((pred functionp) (funcall prop))
    ('root
     (list :root
	   (save-excursion 
	     (while (org-up-heading-safe))
	     (cdar (org-entry-properties (point) "ITEM")))))
    ('todo
     (list :todo 
	   (cdr (car (org-entry-properties (point) "TODO")))))
    ('file
     (list :file 
	   (cdr (car (org-entry-properties (point) "FILE")))))
    ('timestamp
     (list :timestamp
	   (when-let ((timestamp (cdar (org-entry-properties (point) "TIMESTAMP"))))
	     (cond ((string= timestamp
			     (elgantt--get-property-from-org-point))
		    nil)
		   ((s-match "--" timestamp)
		    nil)
		   (t
		    (elgantt-parse::convert-date-string-to-ts timestamp))))))
    ('timestamp-ia
     (list :timestamp-ia
	   (when-let ((timestamp-ia (cdar (org-entry-properties (point) "TIMESTAMP_IA"))))
	     (cond ((string= timestamp-ia
			     (elgantt--get-property-from-org-point))
		    nil)
		   ((s-match "--" timestamp-ia)
		    nil)
		   (t
		    (elgantt-parse::convert-date-string-to-ts timestamp-ia))))))
    ('timestamp-range
     (list :timestamp-range
	   (when-let ((range (cdar (org-entry-properties (point) "TIMESTAMP"))))
	     (cond ((string= range (cadr (elgantt-parse::get 'category)))
		    nil)
		   ((not (s-match "--" range))
		    nil)
		   (t
		    (let ((dates (s-split "--" range)))
		      (list (elgantt-parse::convert-date-string-to-ts (car dates)) (elgantt-parse::convert-date-string-to-ts (cadr dates)))))))))
    ('timestamp-ia-range
     (list :timestamp-ia-range
	   (when-let ((range (cdar (org-entry-properties (point) "TIMESTAMP_IA"))))
	     (cond ((string= range (cadr (elgantt-parse::get 'category)))
		    nil)
		   ((not (s-match "--" range))
		    nil)
		   (t
		    (let ((dates (s-split "--" range)))
		      (list (elgantt-parse::convert-date-string-to-ts (car dates)) (elgantt-parse::convert-date-string-to-ts (cadr dates)))))))))
    ('category
     (list :category 
	   (cdar (org-entry-properties (point) "CATEGORY"))))
    ('deadline
     (list :deadline 
	   (when (cdr (car (org-entry-properties (point) "DEADLINE")))
	     (if (string= (cdr (car (org-entry-properties (point) "DEADLINE"))) (cadr (elgantt-parse::get 'category)))
		 nil
	       (elgantt-parse::convert-date-string-to-ts (cdr (car (org-entry-properties (point) "DEADLINE"))))))))
    ('hashtag
     (list :hashtag
	   (when-let ((tag-string (cdar (org-entry-properties (point) "ALLTAGS"))))
	     (-first (lambda (tagstring) (s-starts-with-p "#" tagstring))
		     (s-split ":" tag-string)))))
    ('scheduled
     (list :scheduled
	   (cdar (org-entry-properties (point) "SCHEDULED"))))
    ('alltags
     (list :alltags
	   (when-let ((tag-string (cdar (org-entry-properties (point) "ALLTAGS"))))
	     (s-split ":" tag-string t))))
    ('elgantt-data
     (let ((label nil)
	   (start-or-end-or-range nil)
	   (date nil))
       (when (cdar (org-entry-properties (point) "ALLTAGS"))
	 (dolist (tag (s-split ":" (cdar (org-entry-properties (point) "ALLTAGS"))))
	   (when (or (s-ends-with-p "_start" tag) (s-ends-with-p "_end" tag) (s-ends-with-p "_block" tag))
	     (setq label (car (s-split "_" tag)))
	     (setq start-or-end-or-range (cadr (s-split "_" tag)))
	     (if (string= start-or-end-or-range "block")
		 (cond ((elgantt-parse::get 'timestamp-range)
			(setq date (elgantt-parse::get 'timestamp-range)))
		       ((elgantt-parse::get 'timestamp-ia-range)
			(setq date (elgantt-parse::get 'timestamp-ia-range))))
	       (cond ((elgantt-parse::get 'deadline)
		      (setq date (cadr (elgantt-parse::get 'deadline))))
		     ((elgantt-parse::get 'timestamp)
		      (setq date (cadr (elgantt-parse::get 'timestamp))))
		     ((elgantt-parse::get 'timestamp-ia)
		      (setq date (cadr (elgantt-parse::get 'timestamp-ia))))))))
	 `(:elgantt-type ,start-or-end-or-range :elgantt-label ,label :elgantt-date ,date))))
    (_ (user-error "\"%s\" is not a valid argument." prop))))

