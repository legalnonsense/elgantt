
;; 1. check if entry has a date
;; 2. check to see if it has a heading already
;; 3. a. if it has a heading, goto the beginning of that line
;;    b. otherwise, insert a new heading
;; 4. insert the entry by changing the text properties at point

(defun elgantt-parse::get (prop)
  "Accepts the following arguments: `root', `timestamp', 
`timestamp-ia', `category', `hashtag', `timestamp-range', `timestamp-ia-range',
`deadline'. PROP may also be a function to get heading information, 
which is claled with POINT at the first point of the org headline."
  (pcase prop
    ((pred functionp) (funcall prop))
    ('root
     (save-excursion 
       (while (org-up-heading-safe))
       (cdar (org-entry-properties (point) "ITEM"))))
    ('timestamp
     (cond ((string= (cdar (org-entry-properties (point) "TIMESTAMP")) (elgantt--get-property-from-org-point)) nil)
	   ((not (s-match "--" (cdar (org-entry-properties (point) "TIMESTAMP")))) nil)
	   (t
	    (let ((dates (s-split "--" (cdar (org-entry-properties (point) "TIMESTAMP")))))
	      (list (elgantt--normalize-date-string (car dates)) (elgantt--normalize-date-string (cadr dates)))))))
    ('timestamp-range
     (when (cdar (org-entry-properties (point) "TIMESTAMP"))
       (cond ((string= (cdar (org-entry-properties (point) "TIMESTAMP")) (elgantt--get-property-from-org-point)) nil)
	     ((not (s-match "--" (cdar (org-entry-properties (point) "TIMESTAMP")))) nil)
	     (t
	      (let ((dates (s-split "--" (cdar (org-entry-properties (point) "TIMESTAMP_IA")))))
		(list (elgantt--normalize-date-string (car dates)) (elgantt--normalize-date-string (cadr dates))))))))
    ('timestamp-ia
     (when (cdr (car (org-entry-properties (point) "TIMESTAMP_IA")))
       (cond ((string= (cdr (car (org-entry-properties (point) "TIMESTAMP_IA"))) (elgantt--get-property-from-org-point)) nil)
	     ((s-match "--" (cdar (org-entry-properties (point) "TIMESTAMP_IA"))) nil)
	     (t
	      (elgantt--normalize-date-string (cdr (car (org-entry-properties (point) "TIMESTAMP_IA"))))))))       
    ('timestamp-ia-range
     (when (cdar (org-entry-properties (point) "TIMESTAMP_IA"))
       (cond ((string= (cdar (org-entry-properties (point) "TIMESTAMP_IA")) (elgantt--get-property-from-org-point)) nil)
	     ((not (s-match "--" (cdar (org-entry-properties (point) "TIMESTAMP_IA")))) nil)
	     (t
	      (let ((dates (s-split "--" (cdar (org-entry-properties (point) "TIMESTAMP_IA")))))
		(list (elgantt--normalize-date-string (car dates)) (elgantt--normalize-date-string (cadr dates))))))))
    ('category
     (cdar (org-entry-properties (point) "CATEGORY")))
    ('deadline
     (when (cdr (car (org-entry-properties (point) "DEADLINE")))
       (if (string= (cdr (car (org-entry-properties (point) "DEADLINE"))) (elgantt--get-property-from-org-point))
	   nil
	 (elgantt--normalize-date-string (cdr (car (org-entry-properties (point) "DEADLINE")))))))
    ('hashtag
     (-first (lambda (tagstring) (s-starts-with-p "#" tagstring))
	     (s-split ":"
		      (cdar (org-entry-properties (point) "ALLTAGS")))))
    (_ (user-error "%s is not a valid argument."))))




