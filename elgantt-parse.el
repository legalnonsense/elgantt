;;;  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'color)
(require 'org)
(require 's)
(require 'dash)
(require 'ts)

(defun elgantt-parse::convert-date-string (date-string)
  (when date-string 
    (ts-format "%Y-%m-%d" (ts-parse-org date-string))))

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
      xxx)))


(defun elgantt-parse::get (prop &rest args)
  "Accepts any of the following arguments: `root', `timestamp', 
`timestamp-ia', `category', `hashtag', `timestamp-range', `timestamp-ia-range',
`deadline', `todo', `alltags', `elgantt-data', `scheduled', `file', `headline', 
`elgantt-header', PROP may also be a function to get heading information, 
which is called with POINT at the first point of the org headline with ARGS."
  (pcase prop
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
			     (cadr (elgantt-parse::get 'category)))
		    nil)
		   ((s-match "--" timestamp)
		    nil)
		   (t
		    (elgantt-parse::convert-date-string timestamp))))))
    ('timestamp-ia
     (list :timestamp-ia
	   (when-let ((timestamp-ia (cdar (org-entry-properties (point) "TIMESTAMP_IA"))))
	     (cond ((string= timestamp-ia
			     (cadr (elgantt-parse::get 'category)))
		    nil)
		   ((s-match "--" timestamp-ia)
		    nil)
		   (t
		    (elgantt-parse::convert-date-string timestamp-ia))))))
    ('timestamp-range
     (list :timestamp-range
	   (when-let ((range (cadr (org-entry-properties (point) "TIMESTAMP"))))
	     (cond ((string= range (cadr (elgantt-parse::get 'category)))
		    nil)
		   ((not (s-match "--" range))
		    nil)
		   (t
		    (let ((dates (s-split "--" range)))
		      (list (elgantt-parse::convert-date-string (car dates)) (elgantt-parse::convert-date-string (cadr dates)))))))))
    ('timestamp-ia-range
     (list :timestamp-ia-range
	   (when-let ((range (cadr (org-entry-properties (point) "TIMESTAMP_IA"))))
	     (cond ((string= range (cadr (elgantt-parse::get 'category)))
		    nil)
		   ((not (s-match "--" range))
		    nil)
		   (t
		    (let ((dates (s-split "--" range)))
		      (list (elgantt-parse::convert-date-string (car dates)) (elgantt-parse::convert-date-string (cadr dates)))))))))
    ('category
     (list :category 
	   (cdr (assoc "CATEGORY" (org-entry-properties (point) "CATEGORY")))))
    ('deadline
     (list :deadline 
	   (when (cdr (car (org-entry-properties (point) "DEADLINE")))
	     (if (string= (cdr (car (org-entry-properties (point) "DEADLINE"))) (cadr (elgantt-parse::get 'category)))
		 nil
	       (elgantt-parse::convert-date-string (cdr (car (org-entry-properties (point) "DEADLINE"))))))))
    ('hashtag
     (list :hashtag
	   (when-let ((tag-string (cdar (org-entry-properties (point) "ALLTAGS"))))
	     (-first (lambda (tagstring) (s-starts-with-p "#" tagstring))
		     (s-split ":" tag-string)))))
    ('scheduled
     (list :scheduled
	   (when (cdr (car (org-entry-properties (point) "SCHEDULED")))
	     (if (string= (cdr (car (org-entry-properties (point) "SCHEDULED"))) (cadr (elgantt-parse::get 'category)))
		 nil
	       (elgantt-parse::convert-date-string (cdr (car (org-entry-properties (point) "SCHEDULED"))))))))
    ('alltags
     (list :alltags
	   (let ((org-trust-scanner-tags t))
	     (when-let ((tag-string (cdar (org-entry-properties (point) "ALLTAGS"))))
	       (s-split ":" tag-string t)))))
    ('elgantt-header
     (pcase elgantt-header-type
       ('root
	(list :elgantt-header (cadr (elgantt-parse::get 'root))))
       ('hashtag
	(list :elgantt-header (cadr (elgantt-parse::get 'hashtag))))
       ('category 
	(list :elgantt-header (cadr (elgantt-parse::get 'category))))
       (_ (error "Invalid header type."))))
    ('elgantt-data
     (let ((label nil)
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
       (cond ((cadr (elgantt-parse::get 'deadline))
	      (setq type 'deadline)
	      (setq date (cadr (elgantt-parse::get 'deadline))))
	     ((cadr (elgantt-parse::get 'timestamp))
	      (setq type 'timestamp)
	      (setq date (cadr (elgantt-parse::get 'timestamp))))
	     ((cadr (elgantt-parse::get 'timestamp-ia))
	      (setq type 'timestamp-ia)
	      (setq date (cadr (elgantt-parse::get 'timestamp-ia)))))
       ;; ((cadr (elgantt-parse::get 'scheduled))
       ;;  (setq type 'scheduled)
       ;;  (setq date (cadr (elgantt-parse::get 'scheduled)))))
       (list :elgantt-start-end-range start-or-end-or-range :elgantt-label label :elgantt-date date :elgantt-type type)))
    ((pred functionp) (funcall prop args))
    (_ (user-error "\"%s\" is not a valid argument." prop))))


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
