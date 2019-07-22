
;; May 8 - added support for getting active and inactive time ranges to the parsing functions and map-data
;; DONE: added functions to parse time ranges, putting active and inactive time ranges into a list '("YYYY-MM
;; DONE: added support for inserting the calendar point into the map-data
;; DONE: figure out why the dates are off, see, e.g., Robnsin Nov, 2019 and Dec. 2019

;;May 9 
;; DONE: figure out how to highlight the current fucking date (the problem is with inserting a character that has a background that can be changed
;; DONE: add support for drawing date blocks for active time ranges; these should be supported by a "xxx_block" tag format

;;May 10
;; IN_PROGRESS: add support for moving around the buffer (may require learning to major mode!)
;; todo: add support for making tentative dates half-color
;; todo: horizontal highlighting 
;; todo: support for hiding any with blank lines 

;; June 16
;; Have not looked at this in awhile and had toe get rid of some recent changes to get rid of an error I could not find 

(define-derived-mode elgantt-mode special-mode "El Gantt"
					;(define-key elgantt-mode-map (kbd "f") 'elgantt/keys/move-forward)
					;(define-key elgantt-mode-map (kbd "p") 'elgantt/keys/move-up)
					;(define-key elgantt-mode-map (kbd "n") 'elgantt/keys/move-down)
  ;;  (define-key elgantt-mode-map (kbd "b") 'backward-word)
  (define-key elgantt-mode-map (kbd "r") 'elgantt-open)
  (define-key elgantt-mode-map (kbd "SPC") 'elgantt/display/open-org-file-at-point)
  (define-key elgantt-mode-map (kbd "f")   'elgantt/display/move-selection-bar-forward)
  (define-key elgantt-mode-map (kbd "b")   'elgantt/display/move-selection-bar-backward)
  (define-key elgantt-mode-map (kbd "RET") 'elgantt--open-org-agenda-at-date))
					;  (define-key elgantt-mode-map (kbd "n") 'elgantt--next-line)


(defvar elgantt/map-data nil)

(defun elgantt/display/highlight-weekends-in-line-number (line-number-string years)
  (let ((months [31 28 31 30 31 30 31 31 30 31 30 31]))
    (dotimes (y (length years))
      (if (elgantt/date/leap-year-p (nth y years))
	  (setq months [31 29 31 30 31 30 31 31 30 31 30 31])
	(setq months [31 28 31 30 31 30 31 31 30 31 30 31]))
      (dotimes (m 12)
	(dotimes (d (aref months m))
	  (when (or (= (org-day-of-week (1+ d) (1+ m) (nth y years)) 5)
		    (= (org-day-of-week (1+ d) (1+ m) (nth y years)) 6))
	    (put-text-property (elgantt/display/convert-date-to-column-number (elgantt/date/normalize-date-string (format "%d-%d-%d" (nth y years) (1+ m) (1+ d))))
			       (1+ (elgantt/display/convert-date-to-column-number (elgantt/date/normalize-date-string (format "%d-%d-%d" (nth y years) (1+ m) (1+ d)))))
			       ;; 'font-lock-face `(:background ,(color-lighten-name (plist-get (get-text-property (elgantt/display/convert-date-to-column-number
			       ;; 										   (elgantt/date/normalize-date-string (format "%d-%d-%d" (nth y years) (1+ m) (1+ d))))
			       ;; 										  'face line-number-string)
			       ;; 							       :background) -25))))))))
			       'font-lock-face `(:background "#ececec") line-number-string))))))
  line-number-string)


(defun elgantt-open ()
  "Display Gantt chart for an Orgmode outline"
  (interactive)
  (require 'color)
  (require 'org)
  (require 's)

  (switch-to-buffer "El Gantt Calendar")
  (let ((inhibit-read-only t))
    (erase-buffer))

  (setq elgantt/variables/exclusions '("Habits" "Personal" "Business" "taskmaster" "Unsorted" "Computer" "Business"))
  (setq elgantt/map-data nil)
  (setq elgantt/use-hashtag nil)
  (setq elgantt/display/variables/normal-year-number-line   "|1234567890123456789012345678901|1234567890123456789012345678|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901")
  (setq elgantt/display/variables/leap-year-number-line     "|1234567890123456789012345678901|12345678901234567890123456789|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901")
  (setq elgantt/display/variables/normal-year-calendar-line "| January xxxx                  | February xxxx              | March xxxx                    | April xxxx                   | May xxxx                      | June xxxx                    | July xxxx                     | August xxxx                   | September xxxx               | October xxxx                  | November xxxx                | December xxxx                 ")

  (setq elgantt/display/variables/leap-year-calendar-line "| January xxxx                  | February xxxx               | March xxxx                    | April xxxx                   | May xxxx                      | June xxxx                    | July xxxx                     | August xxxx                   | September xxxx               | October xxxx                  | November xxxx                | December xxxx                 ")
  (setq elgantt/display/variables/normal-year-blank-line  "|                               |                            |                               |                              |                               |                              |                               |                               |                              |                               |                              |                               ")
  (setq elgantt/display/variables/leap-year-blank-line    "|                               |                             |                               |                              |                               |                              |                               |                               |                              |                               |                              |                               ")
    
  (setq elgantt/display/variables/current-selection-bar-color "Orange")
  (setq elgantt/display/variables/block-default-start "#696969")
  (setq elgantt/display/variables/block-default-end "#ff4500")

  (setq elgantt/display/variables/color-of-current-selectionbar "#880088")
  (setq elgantt/display/variables/current-selection-column nil)
  (setq elgantt/display/variables/tentative-block-lighten-percent 25)
  (setq elgantt/variables/color-alist '((discovery . "#4444ff")
					(brief . "#ff8c00")
					(waiting . "#cdcd00")))
  (setq elgantt/variables/error-log "")
  (setq elgantt/variables/default-background-color "#bebebe")
  (setq elgantt/variables/deadline-character "D")
  (setq elgantt/display/old-backgrounds '())
  (put-text-property 0 1 'font-lock-face `(:background ,(face-attribute 'default :background)) elgantt/variables/deadline-character)
  (setq elgantt/variables/event-character "x")
  (put-text-property 0 1 'font-lock-face `(:background ,(face-attribute 'default :background)) elgantt/variables/event-character)
  (elgantt-draw)
  (elgantt-mode)
  (toggle-truncate-lines 1)
  (setq cursor-type 'box)
  (goto-char (point-min))
  (forward-line 2)
  (forward-char (elgantt/display/convert-date-to-column-number (format-time-string "%Y-%m-%d")))
  (add-hook 'post-command-hook 'elgantt/display/show-echo-message nil t)
  (add-hook 'post-command-hook 'elgantt/display/vertical-highlight nil t)
;  (add-hook 'post-command-hook 'elgantt/display/horizontal-highlight nil t)
  (delete-other-windows))

(defun elgantt-draw ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Parsing org files...")
    (elgantt/parse-org-files)
    (erase-buffer)
    (insert (elgantt/display/draw-month-line))
    (insert "\n")
    (insert (elgantt/display/draw-number-line))
    (insert "\n")
    (setq elgantt/variables/number-of-lines 1)

    (dolist (header (elgantt/data/get-headers))
      (insert (elgantt/display/draw-string-for-header header))
      (insert "\n"))
    (elgantt/display/highlight-current-day)))


(defun elgantt/show-parsed-data ()
  (interactive)
  (with-output-to-temp-buffer "El Gantt Parsed Data"
    (dolist (header elgantt/map-data)
      (prin1 (car header))
      (terpri)
      (dolist (property (cdr header))
	(prin1 "    ")
	(prin1 property)
	(terpri))))
  (switch-to-buffer-other-window "El Gantt Parsed Data")
  (toggle-truncate-lines 1))

(defun elgantt/parse-org-files ()
  "returns an list of alists. the cdr of each alist is a plist of the properties of the heading
   access the elgantt/map-data data structure using the elgantt/data functions
   if use-hashtag is non-nil, then it will use tags that start with # to create the
   elgantt/map-data"
  (interactive)
  ;; NOTE: all of these parsing functions assume a global variable elgantt/map-data which is an empty list
  ;; NOTE: I know this is sloppy, and need to fix it. Not a priority right now. 
  (setq elgantt/map-data '())
  (org-map-entries (lambda ()
		     ;; get the header by interning the category
		     (let ((header (intern (elgantt/parse/get-category (point)))))
		       ;; when using hashtags, find the tag with the hashtag, remove the hashtag, and intern it
		       (when elgantt/use-hashtag
			 (if (elgantt/parse/get-hashtag (point))
			     (setq header (intern (elgantt/parse/get-hashtag (point))))
			   (setq header nil)))
		       (when (not (member (symbol-name header) elgantt/variables/exclusions))
			 ;; if there is no hashtag, then skip the entry bc it will not appear in the calendar
			 (when header
			   (elgantt/data/add-properties header `(:category                  ,(elgantt/parse/get-category (point))
											    :hashtag                   ,(elgantt/parse/get-hashtag (point))
											    :tag-string                ,(elgantt/parse/get-alltags-as-string (point))
											    :file                      ,(elgantt/parse/get-file (point))
											    :timestamp_active          ,(elgantt/parse/get-active-timestamp (point))
											    :timestamp_inactive        ,(elgantt/parse/get-inactive-timestamp (point))
											    :timestamp_range_active    ,(elgantt/parse/get-active-time-range)
											    :timestamp_range_inactive  ,(elgantt/parse/get-inactive-time-range)	
											    :deadline                  ,(elgantt/parse/get-deadline (point))
											    :headline_text             ,(elgantt/parse/get-headline-text (point))
											    :todo                      ,(elgantt/parse/get-todo (point))
											    :start-or-end-or-range     ,(nth 0 (elgantt/parse/get-start-or-end-or-range (point)))
											    :calendar-label            ,(nth 1 (elgantt/parse/get-start-or-end-or-range (point)))
											    :calendar-date             ,(nth 2 (elgantt/parse/get-start-or-end-or-range (point)))
											    :calendar-point nil        ; done, this fills in later 
											    :calendar-column nil       ; done, fills below
											    :calendar-blocks nil       ; to be filled later
											    :org-point ,(point)))))))

		   ;; forget what nil does; 'agenda means all agenda files"; 'archive means to skip archives
		   ;; the file is changed for testing purposes
;		   nil '("/home/jeff/Dropbox/DropsyncFiles/elgantt/testing.org"))
  		   nil 'agenda 'archive)

  ;;calculate the offset that will be used for the calendar 
  (setq elgantt/variables/header-column-offset (+ (elgantt/data/get-longest-header-length) 1))

  ;; add the data for what column in the calendar corresponds to each date; if not displayed then nil 
  (dolist (property (elgantt/data/get-all-properties))
    (when (stringp (plist-get property :calendar-date))
      (plist-put property :calendar-column (elgantt/display/convert-date-to-column-number (plist-get property :calendar-date)))))

  ;;  return elgantt/map-data, even thoug it is already saved
  elgantt/map-data)


      ;;;;;;;;;;;;;;;;;;;;;;;;; HELPER FUNCTIONS

(defun elgantt/display/convert-date-to-column-number (date)				    
  "Assumes a YYYY-MM-DD date, returns the column number including the name offset column"
  (let ((spaces 0))
    (subseq (elgantt/date/get-the-range-of-years)
	    0 (position (string-to-number (substring date 0 4)) (elgantt/date/get-the-range-of-years)))
    ;; add the preceding years
    (dolist (year
	     (subseq (elgantt/date/get-the-range-of-years)
		     0 (position (string-to-number (substring date 0 4)) (elgantt/date/get-the-range-of-years))))
      (if (elgantt/date/leap-year-p year)
	  (setq spaces (+ spaces 366 12))
	(setq spaces (+ spaces 365 12))))
    ;; add the current year
    (+ spaces (elgantt/display/convert-date-to-column-in-current-year date) elgantt/variables/header-column-offset)))


(defun elgantt/data/get-longest-header-length ()
  (let ((x 0))
    (dolist (header elgantt/map-data)
      (when (> (length (symbol-name (car header))) x)
	(setq x (length (symbol-name (car header))))))
    x))

(defun elgantt/data/add-properties (header properties)
  "check to see if header is in list, if not, add it
                   then add properties"
  (when (not (elgantt/data/check-if-header-exists header))
    (elgantt/data/add-header-to-list header))
  (elgantt/data/add-property-list-to-header header properties))

(defun elgantt/data/add-header-to-list (header)
  "add a header to the list
                     adds in the form of '(,header . "")
                     (for now, it appears initializing with "" is necessary to create a blank alist"
  (add-to-list 'elgantt/map-data `(,header . "")))

(defun elgantt/data/check-if-header-exists (header)
  "check to see if the new header is a member of the elgantt/map-data
                     pass a quoted name e.g., 'Jones; return t or nil"
  (if (assoc header elgantt/map-data)
      't
    nil))

(defun elgantt/data/check-if-header-has-elements (header)
  "check if a header has elements"
  (if (eq (alist-get header elgantt/map-data) "")
      nil
    't))

(defun elgantt/data/get-property-list-for-header (header)
  (if (eq (alist-get header elgantt/map-data) "")
      nil
    (alist-get header elgantt/map-data)))

(defun elgantt/data/check-for-specific-property (header property)
  ""
  nil)

(defun elgantt/data/add-property-list-to-header (header property-list)
  "adds information from an org heading to the appropriate header"
  (setf (alist-get header elgantt/map-data) (append (alist-get header elgantt/map-data) (list property-list))))

(defun elgantt/parse/get-hashtag-from-tag-string (tag-string)
  "accept a string :x:y:#z and return the tag that begins with a hash, otherwise nil only returns the first hashtag encountered"
  (catch 'hashtag 
    (dolist (tag (s-split ":" tag-string))
      (when (s-starts-with-p "#" tag)
	(throw 'hashtag (substring-no-properties tag))))))

(defun elgantt/parse/generate-plist-for-heading (point &optional hashtag)
  "accept a point in an org file, within the context of org-map-entries, 
               and return an element of an alist in the form of '(header . '(plist of propertiets))")

(defun elgantt/parse/get-category (point)
  (cdr (car (org-entry-properties (point) "CATEGORY"))))

(defun elgantt/parse/get-alltags-as-string (point)
  (if (symbolp (cdr (car (org-entry-properties (point) "ALLTAGS"))))
      (symbol-name (cdr (car (org-entry-properties (point) "ALLTAGS"))))
    (substring-no-properties (cdr (car (org-entry-properties (point) "ALLTAGS"))))))

(defun elgantt/parse/get-file (point)
  (cdr (car (org-entry-properties (point) "FILE"))))

(defun elgantt/parse/get-hashtag (point)
  (if (elgantt/parse/get-hashtag-from-tag-string (elgantt/parse/get-alltags-as-string (point)))
      (substring (elgantt/parse/get-hashtag-from-tag-string (elgantt/parse/get-alltags-as-string (point))) 1)
    nil))

(defun elgantt/parse/get-todo (point)
  (cdr (car (org-entry-properties (point) "TODO"))))

(defun elgantt/parse/get-deadline (point)
  (when (cdr (car (org-entry-properties (point) "DEADLINE")))
    (if (string= (cdr (car (org-entry-properties (point) "DEADLINE"))) (elgantt/parse/get-category (point)))
	nil
      (elgantt/date/normalize-date-string (cdr (car (org-entry-properties (point) "DEADLINE")))))))

;;newssss
(defun elgantt/parse/get-active-time-range ()
  (when (cdar (org-entry-properties (point) "TIMESTAMP"))
    (cond ((string= (cdar (org-entry-properties (point) "TIMESTAMP")) (elgantt/parse/get-category (point))) nil)
	  ((not (s-match "--" (cdar (org-entry-properties (point) "TIMESTAMP")))) nil)
	  (t
	   (let ((dates (s-split "--" (cdar (org-entry-properties (point) "TIMESTAMP")))))
	     (list (elgantt/date/normalize-date-string (car dates)) (elgantt/date/normalize-date-string (cadr dates))))))))
;;new
(defun elgantt/parse/get-inactive-time-range ()
  (when (cdar (org-entry-properties (point) "TIMESTAMP_IA"))
    (cond ((string= (cdar (org-entry-properties (point) "TIMESTAMP_IA")) (elgantt/parse/get-category (point))) nil)
	  ((not (s-match "--" (cdar (org-entry-properties (point) "TIMESTAMP_IA")))) nil)
	  (t
	   (let ((dates (s-split "--" (cdar (org-entry-properties (point) "TIMESTAMP_IA")))))
	     (list (elgantt/date/normalize-date-string (car dates)) (elgantt/date/normalize-date-string (cadr dates))))))))

(defun elgantt/parse/get-active-timestamp (point)
  (when (cdr (car (org-entry-properties (point) "TIMESTAMP")))
    (if (string= (cdr (car (org-entry-properties (point) "TIMESTAMP"))) (elgantt/parse/get-category (point)))
	nil
      (elgantt/date/normalize-date-string (cdr (car (org-entry-properties (point) "TIMESTAMP")))))))

(defun elgantt/parse/get-inactive-timestamp (point)
  (when (cdr (car (org-entry-properties (point) "TIMESTAMP_IA")))
    (if (string= (cdr (car (org-entry-properties (point) "TIMESTAMP_IA"))) (elgantt/parse/get-category (point)))
	nil
      (elgantt/date/normalize-date-string (cdr (car (org-entry-properties (point) "TIMESTAMP_IA")))))))

(defun elgantt/parse/get-headline-text (point)
  (cdr (car (org-entry-properties (point) "ITEM"))))

(defun elgantt/parse/get-start-or-end-or-range (point)
  "returns a list '(label start-or-end-or-range date) each value is a string"
  (let ((label nil)
	(start-or-end-or-range nil)
	(date nil))
    (dolist (tag (s-split ":" (elgantt/parse/get-alltags-as-string (point))))
      (when (or (s-ends-with-p "_start" tag) (s-ends-with-p "_end" tag) (s-ends-with-p "_block" tag))
	(setq label (car (s-split "_" tag)))
	(setq start-or-end-or-range (cadr (s-split "_" tag)))
	(if (string= (cadr (s-split "_" tag)) "block")
	    (setq date (elgantt/parse/get-active-time-range))
	  (progn 
	    (cond ((elgantt/parse/get-deadline (point)) (setq date (elgantt/parse/get-deadline (point))))
		  ((elgantt/parse/get-active-timestamp (point)) (setq date (elgantt/parse/get-active-timestamp (point)))))))))
    `(,start-or-end-or-range ,label ,date)))

(defun elgantt/data/get-headers ()
  (let ((headers '()))
    (dolist (header elgantt/map-data)
      (push (car header) headers))
    headers))

(defun elgantt/data/get-all-properties ()
  "returns a list of all properties"
  (let ((properties-list '()))
    (dolist (header elgantt/map-data)
      (setq header (car header))
      (dolist (properties (alist-get header elgantt/map-data))
	(push properties properties-list)))
    properties-list))

(defun elgantt/date/get-the-range-of-years ()
  (let ((years '()))
    (dolist (property (elgantt/data/get-all-properties))
      (when (plist-get property :deadline)
	(when (not (memq (string-to-number (substring (plist-get property :deadline) 0 4)) years))
	  (push (string-to-number (substring (plist-get property :deadline) 0 4)) years)))
      (when (plist-get property :timestamp_active)
	(when (not (memq (string-to-number (substring (plist-get property :timestamp_active) 0 4)) years))
	  (push (string-to-number (substring (plist-get property :timestamp_active) 0 4)) years)))
      (when (plist-get property :timestamp_range_active)
	(when (not (memq (string-to-number (substring (cadr (plist-get property :timestamp_range_active)) 0 4)) years))
	  (push (string-to-number (substring (cadr (plist-get property :timestamp_range_active)) 0 4)) years))))
    (sort years '<)))

(defun elgantt/date/get-the-oldest-date ()
  "Gets the oldest date in the entire file of any deadline or active timestamp"
  (let ((last-date "1970-01-01"))
    (dolist (property (elgantt/data/get-all-properties))
      (when (plist-get property :deadline)
	(setq last-date (elgantt/date/return-later-date last-date (plist-get property :deadline))))
      (when (plist-get property :timestamp_active)
	(setq last-date (elgantt/date/return-later-date last-date (plist-get property :timestamp_active)))))
    last-date))

(defun elgantt/date/normalize-date-string (date)
  (when (or (string= (substring date 0 1) "<") (string= (substring date 0 1) "["))
    (setq date (substring date 1)))
  (let ((new-date ""))
    (dolist (element (s-split "-"  date))
      (if (< (string-to-number element) 10)
	  (setq new-date (concat new-date "0" (number-to-string (string-to-number element))))
	(setq new-date (concat new-date element))))
    (concat (substring new-date 0 4) "-" (substring new-date 4 6) "-" (substring new-date 6 8))))

(defun elgantt/date/return-later-date (date1 date2)
  "return the later of two dates, assuming YYYY-MM-DD format"
  (car (sort (list date1 date2) 'string>)))

(defun elgantt/display/draw-string-for-header (header &optional show-labels-in-bars)
  "this draws the string that will appear in the calendar for each header and returns it"
  (setq elgantt/variables/line-length (length (elgantt/display/draw-number-line)))
  (setq elgantt/variables/number-of-lines (1+ elgantt/variables/number-of-lines))
  (let ((date-line (elgantt/display/generate-blank-dateline header)))
    ;;put a background that matches the default background so that it is easier to manipulate later 

    (if (= 0 (% elgantt/variables/number-of-lines 2))
	(put-text-property 0 (length date-line) 'font-lock-face `(:background ,(color-lighten-name (face-attribute 'default :background) -25)) date-line)
      (put-text-property 0 (length date-line) 'font-lock-face `(:background ,(face-attribute 'default :background)) date-line))

    ;(elgantt/display/highlight-weekends-in-line-number date-line (elgantt/date/get-the-range-of-years))

    
    (dolist (properties (elgantt/data/get-property-list-for-header header))
      (when (plist-get properties :deadline)
	(setq date-line 
	      (elgantt/display/insert-char-at-index date-line
						    (elgantt/display/convert-date-to-column-number
						     (plist-get properties :deadline))
						    elgantt/variables/deadline-character))
	;;this appears to be working
	(plist-put properties :calendar-point (+ (* elgantt/variables/number-of-lines elgantt/variables/line-length)
	 					 (elgantt/display/convert-date-to-column-number (plist-get properties :deadline))
						 elgantt/variables/number-of-lines 1)))
      
      (when (plist-get properties :timestamp_active)
	(setq date-line 
	      (elgantt/display/insert-char-at-index date-line
						    (elgantt/display/convert-date-to-column-number
						     (plist-get properties :timestamp_active))
						    elgantt/variables/event-character))

	(plist-put properties :calendar-point (+ (* elgantt/variables/number-of-lines elgantt/variables/line-length)
	 					 (elgantt/display/convert-date-to-column-number (plist-get properties :timestamp_active))
						 elgantt/variables/number-of-lines 1))))

    (dolist (bloke (elgantt/data/build-block-list header))
      (let ((color-start elgantt/display/variables/block-default-start)
	    (color-end "#ff1c1c"))
	(when (alist-get (car bloke) elgantt/variables/color-alist)
    	  (setq color-end (alist-get (car bloke) elgantt/variables/color-alist)))
    	(elgantt/display/colors/change-gradient-of-substring color-start color-end date-line
    							     (elgantt/display/convert-date-to-column-number (cadr bloke))
    							     (+ (elgantt/display/convert-date-to-column-number (cadr (cdr bloke))) 1))))
    date-line))

;;colorizers
(defun elgantt/display/get-background-of-point (point)
  "give it a point in the buffer, and it returns the background color of it"
  (plist-get (get-text-property point 'face) :background))

(defun elgantt/display/get-background-of-string (s index)
  "give it a point in the buffer, and it returns the background color of it"
  (plist-get (get-text-property index 'face s) :background))

(defun elgantt/display/change-brightness-of-background-color-of-point (point change)
  "if there is a background font lock color, this will change its brightness"
  (put-text-property point (1+ point) 'font-lock-face
		     `(:background ,(color-lighten-name
				     (plist-get (get-text-property point 'face) :background) change))))



(defun elgantt/display/colors/change-brightness-of-string (color change string)
  "returns a string with the background color changed, assuming the background is changed"
  (put-text-property 0 (length string) 'font-lock-face `(:background ,(color-lighten-name color change)) string)
  string)




(defun elgantt/display/colors/change-gradient-of-substring (start-color end-color string &optional start end)
  "START-COLOR and END-COLOR are #xxxxxx
   STRING is the string 
   START and END are the indices of the string; END is inclusive"
  (when (not start)
    (setq start 0))
  (when (not end)
    (setq end (length string)))
  (setq start-color `(,(string-to-number (substring start-color 1 3) 16)
		      ,(string-to-number (substring start-color 3 5) 16)
		      ,(string-to-number (substring start-color 5 7) 16)))
  (setq end-color `(,(string-to-number (substring end-color 1 3) 16)
		    ,(string-to-number (substring end-color 3 5) 16)
		    ,(string-to-number (substring end-color 5 7) 16)))
  (while (< start end)
    (dolist (color (color-gradient start-color end-color (- end start)))
      (let ((hex ""))
	(dolist (c color)
	  (if (= (length (format "%x" c)) 1)
	      (setq hex (concat hex (format "0%x" c)))
	    (setq hex (concat hex (format "%x" c)))))
	(setq hex (concat "#" hex ))
	(put-text-property start (+ 1 start) 'font-lock-face `(:background ,hex) string))
      (setq start (+ 1 start))))
  string)



(defun elgantt/display/colors/draw-gradient (start-color end-color steps)
  "accepts hex colors \"#ffffff\" and returns a string of spaces"
  (setq start-color `(,(string-to-number (substring start-color 1 3) 16)
		      ,(string-to-number (substring start-color 3 5) 16)
		      ,(string-to-number (substring start-color 5 7) 16)))
  (setq end-color `(,(string-to-number (substring end-color 1 3) 16)
		    ,(string-to-number (substring end-color 3 5) 16)
		    ,(string-to-number (substring end-color 5 7) 16)))
  (let ((color-string ""))
    (dolist (color (color-gradient start-color end-color steps))
      (let ((str " ")
	    (hex ""))
	(setq hex "")
	(dolist (c color)
	  (if (= (length (format "%x" c)) 1)
	      (setq hex (concat hex (format "0%x" c)))
	    (setq hex (concat hex (format "%x" c)))))
	(setq hex (concat "#" hex ))
	(put-text-property 0 (length str) 'font-lock-face `(:background ,hex) str)
	(setq color-string (concat color-string str ))))
    color-string))


;;old function 
;; (defun elgantt/data/build-block-list (header &optional active-time-ranges)
;;   (setq active-time-ranges t)
;;   (let ((block-list '()))
;;     (dolist (properties (elgantt/data/get-property-list-for-header header))
;;       (when (or (string= (plist-get properties :start-or-end-or-range) "start") (string= (plist-get properties :start-or-end-or-range) "end"))
;; 	(when (not (alist-get (intern (plist-get properties :calendar-label)) block-list))
;; 	  (setf (alist-get (intern (plist-get properties :calendar-label)) block-list) '()))

;; 	(if (not (alist-get (intern (plist-get properties :calendar-label)) block-list))
;; 	    (setf (alist-get (intern (plist-get properties :calendar-label)) block-list) (plist-get properties :calendar-date))
;; 	  (setf (alist-get (intern (plist-get properties :calendar-label)) block-list)
;; 		(sort (list (alist-get (intern (plist-get properties :calendar-label)) block-list) (plist-get properties :calendar-date)) 'string<))))

;;       (when active-time-ranges
;;       	(when (plist-get properties :timestamp_range_active)
;;       	  (setf (alist-get (intern (plist-get properties :calendar-label)) block-list) `(,(car  (plist-get properties :timestamp_range_active))
;; 											 ,(cadr (plist-get properties :timestamp_range_active)))))))
;;     block-list))

;; new function 
(defun elgantt/data/build-block-list (header &optional active-time-ranges inactive-time-ranges)
  (setq active-time-ranges t)
  (let ((block-list '()))
    (dolist (properties (elgantt/data/get-property-list-for-header header))
      (when (or (string= (plist-get properties :start-or-end-or-range) "start") (string= (plist-get properties :start-or-end-or-range) "end"))
	(when (not (alist-get (intern (plist-get properties :calendar-label)) block-list))
	  (setf (alist-get (intern (plist-get properties :calendar-label)) block-list) '()))

	(if (not (alist-get (intern (plist-get properties :calendar-label)) block-list))
	    (setf (alist-get (intern (plist-get properties :calendar-label)) block-list) (plist-get properties :calendar-date))
	  (setf (alist-get (intern (plist-get properties :calendar-label)) block-list)
		(sort (list (alist-get (intern (plist-get properties :calendar-label)) block-list) (plist-get properties :calendar-date)) 'string<))))

      (when active-time-ranges
      	(when (plist-get properties :timestamp_range_active)
      	  (setf (alist-get (intern (plist-get properties :calendar-label)) block-list) `(,(car  (plist-get properties :timestamp_range_active))
											 ,(cadr (plist-get properties :timestamp_range_active))))))

      (when inactive-time-ranges
      	(when (plist-get properties :timestamp_range_inactive)
      	  (setf (alist-get (intern (plist-get properties :calendar-label)) block-list) `(,(car  (plist-get properties :timestamp_range_inactive))
											 ,(cadr (plist-get properties :timestamp_range_inactive)))))))
    block-list))


      ;;;;;;;;;;;;;;;HELPER FUNCTIONS

(defun elgantt/display/draw-number-line ()
  (let ((number-line ""))
    (dolist (year (elgantt/date/get-the-range-of-years))
      (if (elgantt/date/leap-year-p year)
	  (setq number-line (concat number-line elgantt/display/variables/leap-year-number-line))
	(setq number-line (concat number-line elgantt/display/variables/normal-year-number-line))))
    (elgantt/display/highlight-weekends-in-line-number (concat (make-string elgantt/variables/header-column-offset ? ) number-line) (elgantt/date/get-the-range-of-years))))

;(insert (elgantt/display/highlight-weekends-in-line-number (make-string 2000 ?\ ) '(2019 2020 2021)))

;; ;;old
;; (defun elgantt/display/draw-month-line ()
;;   (let ((calendar-line ""))
;;     (dolist (year (elgantt/date/get-the-range-of-years))
;;       (if (elgantt/date/leap-year-p year)
;;           (setq calendar-line (concat calendar-line elgantt/display/variables/leap-year-calendar-line))
;;         (setq calendar-line (concat calendar-line elgantt/display/variables/normal-year-calendar-line))))
;;     (concat (make-string elgantt/variables/header-column-offset ? ) calendar-line)))

;;testing
(defun elgantt/display/draw-month-line ()
  (let ((calendar-line ""))
    (dolist (year (elgantt/date/get-the-range-of-years))
      (if (elgantt/date/leap-year-p year)
	  (setq calendar-line (concat calendar-line 
				      (replace-regexp-in-string "xxxx" (number-to-string year) 
								elgantt/display/variables/leap-year-calendar-line)))
	(setq calendar-line (concat calendar-line
				    (replace-regexp-in-string "xxxx" (number-to-string year) 
							      elgantt/display/variables/normal-year-calendar-line)))))
    (concat (make-string elgantt/variables/header-column-offset ? ) calendar-line)))

;; helper function to create a blank dateline
(defun elgantt/display/generate-blank-dateline (header)
  "creates a blank dateline of appropriate length (spanning all years and offset by the name column size)"
  (let ((date-line ""))
    (dolist (year (elgantt/date/get-the-range-of-years))
      (if (elgantt/date/leap-year-p year)
	  (setq date-line (concat date-line elgantt/display/variables/leap-year-blank-line))
	(setq date-line (concat date-line elgantt/display/variables/normal-year-blank-line))))
    (concat (symbol-name header) (make-string (- elgantt/variables/header-column-offset (length (symbol-name header))) ? ) date-line)))


;; (defun elgantt/display/parse-org-data (header)
;;   (dolist (props (elgantt/data/get-property-list-for-header header))))

(defun elgantt/display/parse/convert-encoded-date-to-YYYY-MM-DD (date)
  (let ((new-date (decode-time date)))
    (concat (number-to-string (nth 5 new-date)) "-" (number-to-string (nth 4 new-date)) "-" (number-to-string (nth 3 new-date)))))

;;depricated; use insert-string-st-index
(defun elgantt/display/insert-char-at-index (string index char)
  "insert a character at index (int) of given string)
                                               return the new string"
  (concat (subseq string 0 index) char (subseq string (+ index 1))))

(defun elgantt/display/insert-string-at-index (source-string index replacement)
  (concat (substring source-string 0 index) replacement (substring source-string (length replacement))))


;; GETTING INFORMATION FROM THE CALENDAR AT POINT

(defun elgantt/display/show-echo-message ()
  (interactive)
  (message "%s -- %s    %s"
	   (elgantt/display/get-header-at-point)
	   (elgantt/display/get-date-at-point)
	   (elgantt/display/get-task-at-point)))

(defun elgantt/display/get-task-at-point ()
  (interactive)
  (let ((task ""))
    (when (alist-get (intern (elgantt/display/get-header-at-point)) elgantt/map-data)
      (dolist (property (alist-get (intern (elgantt/display/get-header-at-point)) elgantt/map-data))
	(when (plist-get property :calendar-point)
	  (when (= (plist-get property :calendar-point) (point))
	    (setq task (plist-get property :headline_text))))))
    task))

(defun elgantt/display/get-date-at-point (&optional column)
  (when (not column)
    (setq column (current-column)))
  (let ((current-point (point))
	(date ""))
    (save-excursion
      (if (re-search-backward "|" nil t)
	  (progn 
	    (setq date (number-to-string (- current-point (match-beginning 0))))
	    (with-no-warnings (goto-line 0))
	    (move-to-column column)
	    (if (re-search-backward "|" nil t)
		(progn
		  (re-search-forward "[[:alpha:]]+" nil t)
		  (setq date (concat (match-string 0) " " date))
		  (if (re-search-forward "[[:digit:]]+" nil t)
		      (setq date (concat date ", " (match-string 0)))
		    (setq date "")))
	      (setq date "")))
	(setq date "")))
    date))

(defun elgantt/display/get-header-at-point ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (if (re-search-forward "[^|]+" nil t)
	(setq header (s-trim (match-string 0)))
      (setq header ""))))

(defun elgantt/display/open-org-file-at-point ()
  (interactive)
  (when (alist-get (intern (elgantt/display/get-header-at-point)) elgantt/map-data)
    (dolist (property (alist-get (intern (elgantt/display/get-header-at-point)) elgantt/map-data))
      (when (plist-get property :calendar-point)
	(when (= (plist-get property :calendar-point) (point))
	  (elgantt--navigate-to-org-file (car (last (s-split "/" (plist-get property :file)))) (plist-get property :org-point)))))))

;; NAVIGATING TO OTHER FILES
(defun elgantt--navigate-to-org-file (FILE POINT)
  "this will navigate to a location in an org file when
                supplied with the file name (string) and point (number)"
  (switch-to-buffer-other-window FILE)
  (org-shifttab)
  (goto-char POINT)
  (outline-show-children)
  (outline-show-entry)
  (beginning-of-visual-line))

(defun elgantt--open-org-agenda-at-date ()
  (interactive)
  (let ((date
	 `(,(nth 4 (parse-time-string (elgantt/display/get-date-at-point)))
	   ,(nth 3 (parse-time-string (elgantt/display/get-date-at-point)))
	   ,(nth 5 (parse-time-string (elgantt/display/get-date-at-point))))))
    (org-agenda-list nil (calendar-absolute-from-gregorian date) 'day))
  (other-window 1))


;;;helper to get days per year
(defun elgantt/display/days-in-year (year)
  (if (elgantt/date/leap-year-p year)
      366
    365))

        ;;;;; helpers for converting dates to columns
(defun elgantt/display/convert-date-string-to-day-number-in-year (date)
  "accept a date in the format YYYY-MM-DD and return an int of #day of the year"
  (time-to-day-in-year (encode-time 0 0 0 (string-to-number (substring date 8 10))
				    (string-to-number (substring date 5 7))
				    (string-to-number (substring date 0 4)))))

(defun elgantt/display/convert-date-to-column-in-current-year (date)
  "accepts a date YYYY-MM-DD and returns the position on the horizontal calendar (int)
                       this works on leap years"
  (+ (elgantt/display/convert-date-string-to-day-number-in-year date)
     (- (string-to-number (substring date 5 7)) 1)))


(defun elgantt/date/get-total-days ()
  (let ((total-days 0))
    (dolist (year (elgantt/date/get-the-range-of-years))
      (if (elgantt/date/leap-year-p year)
	  (setq total-days (+ total-days 366))
	(setq total-days (+ total-days 365))))
    total-days))

(defun elgantt/display/parse/days-between-dates (d1 d2)
  "return the number of days between two dates
                                   accepts: two dates in the format '(month day year) [i.e., d1 and d2]
                                   returns an int of the number of days"
  (let* ((days (- (calendar-absolute-from-gregorian d1)
		  (calendar-absolute-from-gregorian d2)))
	 (days (1+ (if (> days 0) days (- days)))))
    days))

(defun elgantt/date/leap-year-p (year)
  (if (= (% year 4) 0)
      t
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEALING WITH THE BUFFER / COLORIZERS

(defun elgantt/display/highlight-current-day ()
  (interactive)
  (save-excursion 
    (goto-char (point-min))
    (let ((date-line (+ (elgantt/display/convert-date-to-column-number (format-time-string "%Y-%m-%d"))))
	  (x 1)
	  (total-lines (elgantt/display/count-lines)))
      (while (< x total-lines)
	(move-beginning-of-line 1)
	(forward-char date-line)
	(elgantt/display/change-brightness-of-background-color-of-point (point) -15)
	(forward-line)
	(setq x (1+ x))))
    (goto-char (point-min))))

(defun elgantt/display/get-background-of-point ()
  "give it a point in the buffer, and it returns the background color of it"
  (interactive)
  (message (plist-get (get-text-property (point) 'face) :background))
  (plist-get (get-text-property (point) 'face) :background))

(defun elgantt/display/change-brightness-of-background-color-of-point (point change)
  "if there is a background font lock color, this will change its brightness"
  (when (get-text-property point 'face)
    (put-text-property point (1+ point) 'font-lock-face
		       `(:background ,(color-lighten-name
				       (plist-get (get-text-property point 'face) :background) change)))))

(defun elgantt/display/colors/change-brightness-of-string (color change string)
  "returns a string with the background color changed"
  (put-text-property 0 (length string) 'font-lock-face `(:background ,(color-lighten-name color change)) string))

(defun elgantt/display/colors/draw-gradient (start-color end-color steps)
  "accepts hex colors \"#ffffff\""
  (setq start-color `(,(string-to-number (substring start-color 1 3) 16)
		      ,(string-to-number (substring start-color 3 5) 16)
		      ,(string-to-number (substring start-color 5 7) 16)))
  (setq end-color `(,(string-to-number (substring end-color 1 3) 16)
		    ,(string-to-number (substring end-color 3 5) 16)
		    ,(string-to-number (substring end-color 5 7) 16)))
  (let ((color-string ""))
    (dolist (color (color-gradient start-color end-color steps))
      (let ((str " ")
	    (hex ""))
	(setq hex "")
	(dolist (c color)
	  (if (= (length (format "%x" c)) 1)
	      (setq hex (concat hex (format "0%x" c)))
	    (setq hex (concat hex (format "%x" c)))))
	(setq hex (concat "#" hex ))
	(put-text-property 0 (length str) 'font-lock-face `(:background ,hex) str)
	(setq color-string (concat color-string str ))))
    color-string))



(defun elgantt/display/count-lines ()
  "Return number of lines between START and END.
  This is usually the number of newlines between them,
  but can be one more if START is not equal to END
  and the greater of them is not at the start of a line.

  Taken from Xah Lee and modified. 
  http://ergoemacs.org/emacs/emacs_line_number_problem.html

  Isn't there an easier way to do this!?"
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) (point-max))
      (goto-char (point-min))
      (if (eq selective-display t)
	  (save-match-data
	    (let ((done 0))
	      (while (re-search-forward "[\n\C-m]" nil t 40)
		(setq done (+ 40 done)))
	      (while (re-search-forward "[\n\C-m]" nil t 1)
		(setq done (+ 1 done)))
	      (goto-char (point-max))
	      (if (and (/= start end)
		       (not (bolp)))
		  (1+ done)
		done)))
	(1+ (- (buffer-size) (forward-line (buffer-size))))))))

(defun elgantt/keys/move-forward ()
  (interactive)
  (forward-word)
  (backward-char)
  (elgantt/display/get-echo-message))

(defun elgantt/keys/move-backward ()
  (backward-word))

(defun elgantt/keys/move-up ()
  (interactive)
  (elgantt/keys/move-line -1))

(defun elgantt/keys/move-down ()
  (interactive)
  (elgantt/keys/move-line 1))


(defun elgantt/keys/move-line (direction)
  "direction == 1 means move down
   direction == -1 means move up"
  (interactive)
  (forward-line direction)
  (when (not (thing-at-point 'word))
    (cond ((< (re-search-forward "[[:alpha:]]") (re-search-backward "[[:alpha:]]"))
	   (elgantt/keys/move-forward))
	  ((> (re-search-forward "[[:alpha:]]") (re-search-backward "[[:alpha:]]"))
	   (elgantt/keys/move-backward)))))





(setq elgantt/display/old-horizontal-highlight '())

(defun elgantt/display/horizontal-highlight ()
  (interactive)
  (let ((inhibit-read-only t))
    (dolist (p elgantt/display/old-horizontal-highlight)
      (when (cadr p)
	(put-text-property (car p) (1+ (car p)) 'font-lock-face `(:background ,(cadr p))))))
  (setq elgantt/display/old-horizontal-highlight '())

  (let ((start)
	(end)
	(inhibit-read-only t))
    (save-excursion
      (move-beginning-of-line nil)
      (setq start (point))
      (move-end-of-line nil)
      (setq end (current-column))
      (dotimes (x end)
	(add-to-list 'elgantt/display/old-horizontal-highlight `(,(+ start x) ,(plist-get (get-text-property (+ x start) 'font-lock-face) :background)))
	(elgantt/display/change-brightness-of-background-color-of-point (+ start x) -25)))))


 
    
(defun elgantt/display/vertical-highlight (&optional column)
  "insert a vertical highlight bar at column, and remove the previous selection bar"
  (interactive)
  (when (not column)
    (setq column (current-column)))

  (let ((inhibit-read-only t))
    (dolist (p elgantt/display/old-backgrounds)
      (when (cadr p)
	(put-text-property (car p) (1+ (car p)) 'font-lock-face `(:background ,(cadr p))))))
  (setq elgantt/display/old-background-props '())

  (save-excursion
    (goto-char (point-min))
    (let ((x 1)
	  (inhibit-read-only t))
      (while (< x (elgantt/display/count-lines))
	(move-beginning-of-line 1)
	(forward-char column)
	(add-to-list 'elgantt/display/old-backgrounds `(,(point) ,(plist-get (get-text-property (point) 'font-lock-face) :background)))
	(elgantt/display/change-brightness-of-background-color-of-point (point) -35)
	(forward-line)
	(setq x (1+ x))))))

;; (defun elgantt/display/move-selection-bar-forward ()
;;   "need to add a check to make sure not at the end of the screen"
;;   (interactive)
;;   (elgantt/display/vertical-highlight (+ 1 (current-column)))
;;   (forward-char))

(defun elgantt/display/move-selection-bar-forward ()
  (interactive)
  (when (<= (current-column) elgantt/variables/header-column-offset)
    (forward-char elgantt/variables/header-column-offset))
  (forward-char)
  (goto-char (1- (re-search-forward "[^| ]" nil t)))
  (elgantt/display/vertical-highlight (current-column)))

(defun elgantt/display/move-selection-bar-backward ()
  (interactive)
  (goto-char (re-search-backward "[^| ]" nil t))
  (when (< (current-column) elgantt/variables/header-column-offset)
    (move-beginning-of-line nil)
    (forward-char elgantt/variables/header-column-offset))                               
  (elgantt/display/vertical-highlight (current-column)))    
    


;; (defun elgantt/display/move-selection-bar-backward ()
;;   "need to add a check to make sure not at the end of the screen"
;;   (interactive)
;;   (elgantt/display/vertical-highlight (- (current-column) 1))
;;   (backward-char))


(provide 'elgantt)
