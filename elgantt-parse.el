
;; 1. check if entry has a date
;; 2. check to see if it has a heading already
;; 3. a. if it has a heading, goto the beginning of that line
;;    b. otherwise, insert a new heading
;; 4. insert the entry by changing the text properties at point

(defun jrf/elgantt--get-root-heading ()
  (save-excursion 
    (while (org-up-heading-safe))
    (cdar (org-entry-properties (point) "ITEM"))))

(defun gantt--get-inactive-timestamp ()
  (cdar (org-entry-properties (point) "TIMESTAMP_IA")))

(defun gantt--get-active-timestamp ()
  (cdar (org-entry-properties (point) "TIMESTAMP")))
	   
(defun gantt--get-category ()
  (cdar (org-entry-properties (point) "CATEGORY")))

(defun gantt--get-hashtag ()
  (catch 'hashtag
    (dolist (tag (s-split ":" 
			  (org-no-properties 
			   (cdar (org-entry-properties (point) "ALLTAGS")))))
      (when (s-starts-with-p "#" tag)
	(throw 'hashtag (substring-no-properties tag))))))






