;;;  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'color)
(require 'org)
(require 's)
(require 'dash)
(require 'ts)

(defcustom elgantt-cal-deadline-character "▲"
  "Character used for deadlines in the calendar.")

(defcustom elgantt-cal-active-timestamp-character "●"
  "Character used for active timestamps in the calendar")

(defcustom elgantt-cal-inactive-timestamp-character " "
  "Character used for inactive timestamps in the calendar")

(defcustom elgantt-cal-scheduled-character "s"
  "Character used for active timestamps in the calendar")

(defcustom elgantt-header-type 'category
  "Define how to gather the headers"
  :options '(root hashtag category))
(setq elgantt-header-type 'root)





(defvar elgantt-cal:normal-year-date-line  "|1234567890123456789012345678901|1234567890123456789012345678|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901")
(defvar elgantt-cal:normal-year-month-line "| January xxxx                  | February xxxx              | March xxxx                    | April xxxx                   | May xxxx                      | June xxxx                    | July xxxx                     | August xxxx                   | September xxxx               | October xxxx                  | November xxxx                | December xxxx                 ")
(defvar elgantt-cal:normal-year-blank-line "|                               |                            |                               |                              |                               |                              |                               |                               |                              |                               |                              |                               ")
(defvar elgantt-cal:leap-year-date-line    "|1234567890123456789012345678901|12345678901234567890123456789|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901")
(defvar elgantt-cal:leap-year-month-line   "| January xxxx                  | February xxxx               | March xxxx                    | April xxxx                   | May xxxx                      | June xxxx                    | July xxxx                     | August xxxx                   | September xxxx               | October xxxx                  | November xxxx                | December xxxx                 ")
(defvar elgantt-cal:leap-year-blank-line   "|                               |                             |                               |                              |                               |                              |                               |                               |                              |                               |                              |                               ")

(defcustom elgantt:agenda-files (org-agenda-files)
  "A single agenda file or list of agenda files to use to generate the calendar.
Default: `org-agenda-files'")

(defcustom elgantt-cal:header-column-offset 20
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
(defun elgantt-cal::convert-date-to-column-number (ts)				    
  "Assumes a TS date, returns the column number including the name offset column"
  (let ((spaces 0)
	(date (ts-format "%Y-%m-%d" ts)))
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
    (+ spaces (elgantt--convert-date-to-column-in-current-year date) elgantt--header-column-offset)))

(defun elgantt-cal::get-header-create (header)
  "Put point at HEADER, creating it if necessary."
  (goto-char (point-min))
  (if (search-forward header nil t)
      (beginning-of-line)
    (elgantt-cal::insert-new-header-line header)
    (beginning-of-line)))

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
  (elgantt-cal::get-header-create (plist-get props :header))
  (goto-char (elgantt-cal::convert-date-to-column-number (plist-get props :elgantt-date)))
  (delete-char 1)
  (insert 
   (propertize (elgantt-cal::get-char (plist-get props :type)) props)))

(defun elgantt-cal::insert-new-header-line (header)
  (unless (search-forward header nil t)
    (goto-char (point-max))
    (insert "\n"
	    (substring 
	     (concat header (make-string elgantt-cal:header-column-offset ? ))
	     0 elgantt-cal:header-column-offset))
    (cl-loop for year in (-list elgantt-cal::date-range)
	     do (if (elgantt-cal::leap-year-p year)
		    (insert elgantt-cal:leap-year-blank-line)
		  (insert elgantt-cal:normal-year-blank-line)))))


