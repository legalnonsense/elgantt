
(defsubst elgantt-parse::convert-date-string-to-ts (date-string)
  (ts-parse-org date-string))

(defun elgantt-parse::parse-this-headline (&optional props)
  "Return a property list with all properties available."
  (-flatten-n 1
	      (mapcar #'elgantt-parse::get (or (-list props)
					       '(root
						 headline
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

(defun elgantt-parse::get (prop &rest args)
  "Accepts any of the following arguments: `root', `timestamp', 
`timestamp-ia', `category', `hashtag', `timestamp-range', `timestamp-ia-range',
`deadline', `todo', `alltags', `elgantt-data', `scheduled', `file', and `headline'. 
PROP may also be a function to get heading information, 
which is called with POINT at the first point of the org headline with ARGS."
  (pcase prop
    ((pred functionp) (funcall prop args))
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
    ('headline
     (list :headline
	   (cdar (org-entry-properties (point) "ITEM"))))
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

;; This needs to be fixed to accept the value of
;; `elgantt:agenda-files'. Right now, it defaults to
;; `org-agenda-files'. 
(defun elgantt-parse::get-years (&optional date-type)
  "Get the date range of all time values in all agenda files.
Optional DATE-TYPE is any value (or list of values) accepted by `org-re-timestamp':
        all: all timestamps
     active: only active timestamps (<...>)
   inactive: only inactive timestamps ([...])
  scheduled: only scheduled timestamps
   deadline: only deadline timestamps
     closed: only closed time-stamps
If it is not provided, the default is ('active inactive deadline)"
  (save-excursion 
    (let ((first-year 9999)
	  (last-year 0))
      (--each (org-agenda-files)
	(with-current-buffer (find-buffer-visiting it)
	  (goto-char (point-min))
	  (--each (or (-list date-type)
		      '(active inactive deadline))
	    (while (re-search-forward (org-re-timestamp it) nil t)
	      (let ((current-year (string-to-number (ts-format "%Y" (ts-parse-org (match-string 0))))))
		(cond ((> current-year last-year)
		       (setq last-year current-year))
		      ((< current-year first-year)
		       (setq first-year current-year))))))))
      (when (or (= first-year 9999)
		(= last-year 0))
	(error "Error in elgantt-parse::get-years. Years not parsed properly."))
      (-iterate '1+ first-year (1+ (- last-year first-year))))))

(defun elgantt-parse::get-years* (&optional date-type)
  "Get the date range of all time values in all agenda files.
Optional DATE-TYPE is any value (or list of values) accepted by `org-re-timestamp':
        all: all timestamps
     active: only active timestamps (<...>)
   inactive: only inactive timestamps ([...])
  scheduled: only scheduled timestamps
   deadline: only deadline timestamps
     closed: only closed time-stamps
If it is not provided, the default is ('active inactive deadline)"
  (save-excursion
    (let ((years '()))
      (--each (org-agenda-files)
	(with-current-buffer (find-buffer-visiting it)
	  (--each (or (-list date-type)
		      '(active inactive deadline))
	    (while (re-search-forward (org-re-timestamp it) nil t)
	      (cl-pushnew (string-to-number (ts-format "%Y" (ts-parse-org (match-string 0)))) years)))))
      (sort years '<))))
