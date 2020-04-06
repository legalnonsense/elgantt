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

;; (defun elgantt-parse::parse-this-headline ()
;;   "Return a property list with all properties available.
;; Function should be called with POINT at the first headline.
;; PROPS are additional text properties to append."
;;   (let ((xxx (append
;; 	      (list :org-buffer (buffer-name))
;; 	      (cadr (org-element-at-point))
;; 	      (elgantt-parse::get 'elgantt-header)
;; 	      (elgantt-parse::get 'elgantt-data))))
;;     (when (plist-get xxx :elg-date)
;; (elgantt-cal::insert-entry))))


(setq elgantt:header-type 'root)


(defun elgantt-parse::parse-this-headline ()
  "This is meant as a utility for getting information about an org headline at point.
  Its return value for any inquiry is in the form of (:property value). All return
  values are set as the text properties of the calendar cell. All property names
  are prefixed with `elgantt' to avoid collision with other properties. 
  Accepts any of the following arguments: `root', `timestamp', 
`timestamp-ia', `category', `hashtag', `timestamp-range', `timestamp-ia-range',
`deadline', `todo', `alltags', `elgantt-data', `scheduled', `file', `headline', 
`elgantt-header'."
  (let* ((category (cdr (assoc "CATEGORY" (org-entry-properties (point) "CATEGORY"))))
	 ;; For some reason, certain properties retrieved using `org-entry-properties' return
	 ;; the `category' of an entry if the value is nil. For example, if there is no timestamp
	 ;; in an entry, it will return the category. Thus, certain property values must be check
	 ;; against the entry's category to determine whether the value is nil. Since category
	 ;; is repeatedly used, it is stored first. 
	 (prop-list (append
		     (list :elg-category category)
		     (list :elg-root
			   (save-excursion 
			     (while (org-up-heading-safe))
			     (cdar (org-entry-properties (point) "ITEM"))))
		     (list :elg-todo 
			   (cdr (car (org-entry-properties (point) "TODO"))))
		     (list :elg-file 
			   (cdr (car (org-entry-properties (point) "FILE"))))
		     (list :elg-headline
			   (cdar (org-entry-properties (point) "ITEM")))
		     (list :elg-timestamp
			   (when-let ((timestamp (cdar (org-entry-properties (point) "TIMESTAMP"))))
			     (cond ((string= timestamp
					     category)
				    nil)
				   ((s-match "--" timestamp)
				    nil)
				   (t
				    (elgantt-parse::convert-date-string timestamp)))))
		     (list :elg-timestamp-ia
			   (when-let ((timestamp-ia (cdar (org-entry-properties (point) "TIMESTAMP_IA"))))
			     (cond ((string= timestamp-ia
					     category)
				    nil)
				   ((s-match "--" timestamp-ia)
				    nil)
				   (t
				    (elgantt-parse::convert-date-string timestamp-ia)))))
		     (list :elg-timestamp-range
			   (when-let ((range (cadr (org-entry-properties (point) "TIMESTAMP"))))
			     (cond ((string= range category)
				    nil)
				   ((not (s-match "--" range))
				    nil)
				   (t
				    (let ((dates (s-split "--" range)))
				      (list (elgantt-parse::convert-date-string (car dates))
					    (elgantt-parse::convert-date-string (cadr dates))))))))
		     (list :elg-timestamp-ia-range
			   (when-let ((range (cadr (org-entry-properties (point) "TIMESTAMP_IA"))))
			     (cond ((string= range category)
				    nil)
				   ((not (s-match "--" range))
				    nil)
				   (t
				    (let ((dates (s-split "--" range)))
				      (list (elgantt-parse::convert-date-string (car dates))
					    (elgantt-parse::convert-date-string (cadr dates))))))))
		     (list :elg-deadline 
			   (when (cdr (car (org-entry-properties (point) "DEADLINE")))
			     (if (string= (cdr (car (org-entry-properties (point) "DEADLINE"))) category)
				 nil
			       (elgantt-parse::convert-date-string (cdr (car (org-entry-properties (point) "DEADLINE")))))))
		     (list :elg-hashtag
			   (when-let ((tag-string (cdar (org-entry-properties (point) "ALLTAGS"))))
			     (-first (lambda (tagstring) (s-starts-with-p "#" tagstring))
				     (s-split ":" tag-string))))
		     (list :elg-scheduled
			   (when (cdr (car (org-entry-properties (point) "SCHEDULED")))
			     (if (string= (cdr (car (org-entry-properties (point) "SCHEDULED"))) category)
				 nil
			       (elgantt-parse::convert-date-string (cdr (car (org-entry-properties (point) "SCHEDULED")))))))
		     (list :elg-alltags
			   (when-let ((tag-string (cdar (org-entry-properties (point) "ALLTAGS"))))
			     (mapcar #'org-no-properties (s-split ":" tag-string t))))
		     (list :elg-header
			   (pcase elgantt:header-type
			     ('root 
			      (save-excursion 
				(while (org-up-heading-safe))
				(cdar (org-entry-properties (point) "ITEM"))))
			     ('hashtag 
			      (when-let ((tag-string (cdar (org-entry-properties (point) "ALLTAGS"))))
				(-first (lambda (tagstring) (s-starts-with-p "#" tagstring))
					(s-split ":" tag-string))))
			     ('category  category)
			     (_ (error "Invalid header type.")))))))
    (setq prop-list (append 
		     (cond ((plist-get prop-list :elg-deadline)
			    (list :elg-date (plist-get prop-list :elg-deadline)
				  :elg-type 'deadline))
			   ((plist-get prop-list :elg-timestamp)
			    (list :elg-date (plist-get prop-list :elg-timestamp)
				  :elg-type 'timestamp))
			   ((plist-get prop-list :elg-timestamp-ia)
			    (list :elg-date (plist-get prop-list :elg-timestamp-ia)
				  :elg-type 'timestamp-ia))
			   ((plist-get prop-list :elg-scheduled)
			    (list :elg-date (plist-get prop-list :elg-scheduled)
				  :elg-type 'scheduled)))
		     prop-list))
    (when (plist-get prop-list :elg-date)
      prop-list)))





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
