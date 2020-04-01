;;;  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'color)
(require 'org)
(require 's)
(require 'dash)
(require 'ts)

(setq elgantt-cal-deadline-character "▲")
;;      "Character used for deadlines in the calendar.")

(setq elgantt-cal-active-timestamp-character "●")
;;      "Character used for active timestamps in the calendar")

(setq elgantt-cal-inactive-timestamp-character "i")
;;      "Character used for inactive timestamps in the calendar")

(setq elgantt-cal-scheduled-character "s")
;;"Character used for active timestamps in the calendar")

(defcustom elgantt:skip-files 'archive
  "Accepts the following values from `org-map-entries'):
`archive'    skip trees with the archive tag
`comment'    skip trees with the COMMENT keyword
`function' or Emacs Lisp form:
           will be used as value for org-agenda-skip-function, so
           whenever the function returns a position, FUNC will not be
           called for that entry and search will continue from the
           position returned")

(defcustom elgantt:header-type 'root
  "Define how to gather the headers"
  :options '(root hashtag category))


(setq elgantt:header-type 'root)
(setq elgantt-cal:leap-year-month-line   "| January xxxx                  | February xxxx               | March xxxx                    | April xxxx                   | May xxxx                      | June xxxx                    | July xxxx                     | August xxxx                   | September xxxx               | October xxxx                  | November xxxx                | December xxxx                 ")
(setq elgantt-cal:leap-year-date-line    "|1234567890123456789012345678901|12345678901234567890123456789|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901")
(setq elgantt-cal:leap-year-blank-line   "|                               |                             |                               |                              |                               |                              |                               |                               |                              |                               |                              |                               ")
(setq elgantt-cal:normal-year-month-line "| January xxxx                  | February xxxx              | March xxxx                    | April xxxx                   | May xxxx                      | June xxxx                    | July xxxx                     | August xxxx                   | September xxxx               | October xxxx                  | November xxxx                | December xxxx                 ")
(setq elgantt-cal:normal-year-date-line  "|1234567890123456789012345678901|1234567890123456789012345678|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901")
(setq elgantt-cal:normal-year-blank-line "|                               |                            |                               |                              |                               |                              |                               |                               |                              |                               |                              |                               ")

(defface elgantt:horizontal-line-face
    '((t :background "white" :foreground "white" :height .1))
  "Horizontal line face"
  :group 'elgantt)

(face-spec-set 'elgantt:horizontal-line-face
	       '((t :background "white" :height .2 :width normal)))
(face-spec-set 'elgantt:vertical-line-face
	       '((t :background "white" :foreground "white" :height .1)))

(defface elgantt:vertical-line-face
    '((t :background "white" :foreground "white" :height .1))
  "Vertical line face"
  :group 'elgantt)

(face-spec-set 'elgantt:vertical-line-face
	       '((t :background "white" :foreground "white" :height .1)))

(face-spec-set 'elgantt:vertical-line-face '((t :background "white" :foreground "white" :width ultra-expanded :height .3)))

(setq elgantt:vertical-line-char "|")
(put-text-property 0 1 'face 'elgantt:vertical-line-face elgantt:vertical-line-char)

(setq elgantt:horizontal-bar " ")

;; This was more macro practice than anything...
(defmacro elgantt-cal::add-vertical-line-props (lines)
  (let ((body (cl-loop for line in lines
		       collect `(setq ,line (s-replace "|" ,elgantt:vertical-line-char ,line)))))
    `(progn ,@body)))

(elgantt-cal::add-vertical-line-props (elgantt-cal:leap-year-month-line
				       elgantt-cal:leap-year-date-line
				       elgantt-cal:leap-year-blank-line
				       elgantt-cal:normal-year-month-line
				       elgantt-cal:normal-year-date-line
				       elgantt-cal:normal-year-blank-line))




(defcustom elgantt:agenda-files (org-agenda-files)
  "Source files. Default: `org-agenda-files'.")

1(defcustom elgantt-cal:header-column-offset 20
   "Width of the header column")

(defvar elgantt-cal::deadline-warning-days org-deadline-warning-days
  "Warning days to show in calendar.")

(defvar elgantt-cal::date-range (elgantt-parse::get-years)
  "Range of years to be calendared. Default: `elgantt-parse::get-years'")

(defun elgantt-cal::get-char (type)
  "Get the character to insert."
  (pcase type
    ('deadline elgantt-cal-deadline-character)
    ('timestamp elgantt-cal-active-timestamp-character)
    ('timestamp-ia elgantt-cal-inactive-timestamp-character)
    ('scheduled elgantt-cal-scheduled-character)))

;; This should be re-written
(defun elgantt-cal::convert-date-to-column-number (timestamp)
  "Accepts a date in the form of \"YYYY-MM-DD\""
  (let ((spaces 0)
	(date timestamp))
    (cl-subseq elgantt-cal::date-range
	       0 (cl-position (string-to-number (substring date 0 4)) elgantt-cal::date-range))
    ;; add the preceding years
    (dolist (year
	      (cl-subseq elgantt-cal::date-range
			 0 (cl-position (string-to-number (substring date 0 4)) elgantt-cal::date-range)))
      (if (elgantt-cal::leap-year-p year)
	  (setq spaces (+ spaces 366 12))
	(setq spaces (+ spaces 365 12))))
    ;; add the current year
    (+ spaces (elgantt-cal::convert-date-to-column-in-current-year date) elgantt-cal:header-column-offset)))

(defun elgantt-cal::convert-date-string-to-day-number-in-year (date)
  "accept a date in the format YYYY-MM-DD and return an int of #day of the year"
  (time-to-day-in-year (encode-time 0 0 0 (string-to-number (substring date 8 10))
				    (string-to-number (substring date 5 7))
				    (string-to-number (substring date 0 4)))))

(defun elgantt-cal::convert-date-to-column-in-current-year (date)
  "accepts a date YYYY-MM-DD and returns the position on the horizontal calendar (int)
                       this works on leap years"
  (+ (elgantt-cal::convert-date-string-to-day-number-in-year date)
     (- (string-to-number (substring date 5 7)) 1)))

(defun elgantt-cal::get-header-create (header)
  "Put point at HEADER, creating it if necessary."
  (goto-char (point-min))
  (let ((header (s-truncate elgantt-cal:header-column-offset header)))
    (if (search-forward header nil t)
	(beginning-of-line)
      (progn 
	(elgantt-cal::insert-new-header-line header)
	(beginning-of-line)))))

(defsubst elgantt-cal::get-days-in-year (year)
  (if (elgantt::leap-year-p year) 366 365))

(defsubst elgantt-cal::leap-year-p (year) 
  (= (% year 4) 0))

;;CURRENT
;; props should not be a list 
(defun elgantt-cal::insert-entry (&rest props)
  "PROPS is a plist which must include, at minimum, the following properties:
`elgantt-header', `elgantt-date', `elgantt-type',
`elgantt-label', `elgantt-start-or-end-or-range'"
  (when-let ((props (car props)))
    (unless (equal props '(()))
      (elgantt-cal::get-header-create (plist-get props :elgantt-header))
      (forward-char (elgantt-cal::convert-date-to-column-number (plist-get props :elgantt-date)))
      (delete-char 1)
      (let ((char (elgantt-cal::get-char (plist-get props :elgantt-type))))
	(set-text-properties 0 1 props char)
	(insert char)))))

(defun elgantt-cal::insert-new-header-line (header)
  (goto-char (point-max))
  (insert "\n"
	  (substring 
	   (concat header (make-string elgantt-cal:header-column-offset ? ))
	   0 elgantt-cal:header-column-offset))
  (cl-loop for year in (elgantt-parse::get-years)
     do (if (elgantt-cal::leap-year-p year)
	    (insert elgantt-cal:leap-year-blank-line)
	  (insert elgantt-cal:normal-year-blank-line))))

(defun elgantt-cal::draw-month-line ()
  (let ((calendar-line ""))
    (dolist (year (elgantt-parse::get-years))
      (if (elgantt-cal::leap-year-p year)
	  (setq calendar-line (concat calendar-line 
				      (replace-regexp-in-string "xxxx" (number-to-string year) 
								elgantt-cal:leap-year-month-line)))
	(setq calendar-line (concat calendar-line
				    (replace-regexp-in-string "xxxx" (number-to-string year) 
							      elgantt-cal:normal-year-month-line)))))
    (insert 
     (concat (make-string elgantt-cal:header-column-offset ? ) calendar-line))))

(defun elgantt-cal::draw-number-line ()
  (let ((number-line ""))
    (dolist (year (elgantt-parse::get-years))
      (if (elgantt-cal::leap-year-p year)
	  (setq number-line (concat number-line elgantt-cal:leap-year-date-line))
	(setq number-line (concat number-line elgantt-cal:normal-year-date-line))))
    (insert 
     (concat (make-string elgantt-cal:header-column-offset ? ) number-line))))

(defun elgantt-cal::draw-horizontal-line ()
  (let* ((length
	  (+ (cl-loop for year in elgantt-cal::date-range
		sum (if (elgantt-cal::leap-year-p year)
			(+ 366 12)
		      (+ 365 12)))
	     elgantt-cal:header-column-offset))
	 (string (make-string length ? )))
    (put-text-property 0 length
		       'face
		       'elgantt:horizontal-line-face
		       string)
    (insert string)))

(defun elgantt:get-data ()
  (interactive)
  (jrf/pp
   (-non-nil
    (org-map-entries #'elgantt-parse::parse-this-headline
		     nil
		     (org-agenda-files)
		     elgantt:skip-files))))

(defun elgnatt:set-vertical-bar-face ()
  (goto-char (point-min))
  (while (re-search-forward elgantt:vertical-line-char nil t)
    (put-text-property (match-beginning 0)
		       (match-end 0)
		       'face
		       'elgantt-vertical-line-face)))


(defun elgantt-cal:get-date-at-point (&optional column)
  (if (not (char-equal (char-after) ?|))
      (progn
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
			    (progn
			      (setq date (concat date ", " (match-string 0)))
			      (let ((day (org-day-of-week (nth 3 (parse-time-string date))
							  (nth 4 (parse-time-string date))
							  (nth 5 (parse-time-string date))))
				    (text ""))
				(cond ((= day 0) (setq text "Monday, "))
				      ((= day 1) (setq text "Tuesday, "))
				      ((= day 2) (setq text "Wednesday, "))
				      ((= day 3) (setq text "Thursday, "))
				      ((= day 4) (setq text "Friday, "))
				      ((= day 5) (setq text "Saturday, "))
				      ((= day 6) (setq text "Sunday, ")))
				(setq date (concat text date))))
			  (setq date "")))
		    (setq date "")))
	      (setq date "")))
	  date))
    ""))

(defun elgantt--get-background-of-point (point)
  "give it a point in the buffer, and it returns the background color of it"
  (plist-get (get-text-property point 'face) :background))

(defun elgantt--change-brightness-of-background-at-point (point change)
  "if there is a background font lock color, this will change its brightness"
  (put-text-property point (1+ point) 'font-lock-face
		     `(:background ,(color-lighten-name
				     (plist-get (get-text-property point 'face) :background) change))))

(defun elgantt-cal::set-gradient (header start-date end-date start-color end-color)
  "   HEADER is a string, which will be automatically truncated as needed.
START-COLOR and END-COLOR are hex colors formatted as a string: \"#xxxxxx\".
START-DATE and END-DATE are strings: \"YYYY-MM-DD\""
  (goto-char (point-min))
  (let ((header (s-truncate elgantt-cal:header-column-offset header)))
    (if (search-forward header nil t)
	(progn
	  (beginning-of-line)
	  (let* ((start-color `(,(string-to-number (substring start-color 1 3) 16)
				,(string-to-number (substring start-color 3 5) 16)
				,(string-to-number (substring start-color 5 7) 16)))
		 (end-color `(,(string-to-number (substring end-color 1 3) 16)
			      ,(string-to-number (substring end-color 3 5) 16)
			      ,(string-to-number (substring end-color 5 7) 16)))
		 (start-col (elgantt-cal::convert-date-to-column-number start-date))
		 (end-col (elgantt-cal::convert-date-to-column-number end-date))
		 (start (save-excursion (forward-char start-col) (point)))
		 (end (save-excursion (forward-char end-col) (point)))
		 (hex ""))
	    (beginning-of-line)
	    (dolist (color (color-gradient start-color end-color (1+ (- end start))))
	      (setq hex "")
	      (dolist (c color)
		(if (= (length (format "%x" c)) 1)
		    (setq hex (concat hex (format "0%x" c)))
		  (setq hex (concat hex (format "%x" c)))))
	      (setq hex (concat "#" hex ))
	      (put-text-property start (+ 1 start) 'font-lock-face `(:background ,hex))
	      (setq start (+ 1 start)))))
      (error "Error in elgantt-cal:change-gradient. Header not found."))))

(defun elgantt-cal:get-prop-at-point (&optional property)
  "Get the text PROPERTY at point, if specified. 
Otherwise, get a plist of all properties."
  (let ((properties (text-properties-at (point))))
    (if property
	(plist-get properties property)
      properties)))

(defun elgantt-cal:navigate-to-org-file ()
  "this will navigate to a location in an org file when
                supplied with the file name (string) and point (number)"
  (let ((buffer (elgantt-cal:get-prop-at-point :org-buffer))
	(marker (elgantt-cal:get-prop-at-point :begin)))
    (switch-to-buffer-other-window buffer)
    (org-shifttab)
    (goto-char marker)
    (outline-show-children)
    (outline-show-entry)
    (beginning-of-line)))

(defmacro elgantt-cal:with-point-at-orig-entry (&rest body)
  "Execute BODY with point at location given by the `:begin' property.
Buffer is determined from the `:org-buffer' property." 
  (declare (indent 2))
  `(let ((marker (get-text-property (point) :begin))
	 (marker-buffer (get-text-property (point) :org-buffer)))
     (with-current-buffer marker-buffer
       (save-excursion
	 (goto-char marker)
	 ,@body))))

;; (defmacro elgantt-cal::create-shift-date-funcs (directions units)
;;   "Creates functions which adjust the date at point
;; DIRECTIONS and UNITS are arguments ultimately used in `org-timestamp-change'.
;; See that documentation. The functions are named:
;; `elgantt-cal::shift-UNIT-DIRECTION'"
;;   (let ((funcs (cl-loop for direction in (-list directions)
;; 		  append (cl-loop for unit in (-list units)
;; 			    collect `(defun ,(intern (concat "elgantt-cal::shift-"
;; 							     (symbol-name unit)
;; 							     "-"
;; 							     (symbol-name direction))) ()
;; 				       ,(concat "Shift date at point " (symbol-name direction)
;; 						" by one " (symbol-name unit))
;; 				       (elgantt-cal:with-point-at-orig-entry
;; 					   (when (re-search-forward (org-re-timestamp 'all))
;; 					     (org-timestamp-change 1 ',unit ',direction))))))))
;;     `(progn ,@funcs)))

;; (elgantt-cal::create-shift-date-funcs (up down) (day month year))

(defun elgantt-cal::shift-date (n direction unit)
  (elgantt-cal:with-point-at-orig-entry
      (when (re-search-forward (org-re-timestamp 'all))
	(org-timestamp-change n unit direction))))

(defun elgantt-cal:update-this-cell ()
  "Gets data for a specific cell by looking for any headings
which occur on on the operative DATE which also contain
the same CATEGORY, HASHTAG, or ROOT."
  (let* ((date (elgantt-cal:get-date-at-point))
	 (type (pcase elgantt:header-type
		 ('root 'ancestors)
		 ('category 'category)
		 ('hashtag 'tags-inherited)))
	 (header (elgantt-cal:get-prop-at-point :elgantt-header))
	 (item (pcase type
		 ('category header)
		 ('hashtag header)
		 ('ancestors `(regexp ,header)))))
    (org-ql-select elgantt:agenda-files
	`(and (ts (:on ,date))
	  (,type ,item)))))

(org-ql-select elgantt:agenda-files
    '(ts :on "2020-03-31"))

(org-ql-select elgantt:agenda-files
    '(ts-a :on "2020-03-31"))

(org-ql-query
  :from (org-agenda-files)
  :where '(ts :on "2020-03-31"))



;;;###autoload 
(defun elgantt:open ()
  (switch-to-buffer "*El Gantt Calendar*")
  (setq elgantt-cal::date-range (elgantt-parse::get-years))
  (toggle-truncate-lines 1)
  (horizontal-scroll-bar-mode 1)
  (erase-buffer)
  (elgantt-cal::draw-month-line)
  (insert "\n")
  (elgantt-cal::draw-number-line)
  ;;(insert "\n")
  ;;  (elgantt-cal::draw-horizontal-line)
  (mapc #'elgantt-cal::insert-entry
	(org-map-entries #'elgantt-parse::parse-this-headline
			 nil
			 (org-agenda-files)
			 'archive))
  (goto-char (point-min)))
