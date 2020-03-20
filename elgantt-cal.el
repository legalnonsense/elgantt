(require 'cl-lib)
(require 'color)
(require 'org)
(require 's)
(require 'ts)

(defcustom elgantt-cal-deadline-character "▲"
  "Character used for deadlines in the calendar.")

(defcustom elgantt-cal-active-timestamp-character "●"
  "Character used for active timestamps in the calendar")

(defvar elgantt-cal:normal-year-date-line  "|1234567890123456789012345678901|1234567890123456789012345678|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901")
(defvar elgantt-cal:normal-year-month-line "| January xxxx                  | February xxxx              | March xxxx                    | April xxxx                   | May xxxx                      | June xxxx                    | July xxxx                     | August xxxx                   | September xxxx               | October xxxx                  | November xxxx                | December xxxx                 ")
(defvar elgantt-cal:normal-year-blank-line "|                               |                            |                               |                              |                               |                              |                               |                               |                              |                               |                              |                               ")
(defvar elgantt-cal:leap-year-date-line    "|1234567890123456789012345678901|12345678901234567890123456789|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901")
(defvar elgantt-cal:leap-year-month-line   "| January xxxx                  | February xxxx               | March xxxx                    | April xxxx                   | May xxxx                      | June xxxx                    | July xxxx                     | August xxxx                   | September xxxx               | October xxxx                  | November xxxx                | December xxxx                 ")
(defvar elgantt-cal:leap-year-blank-line   "|                               |                             |                               |                              |                               |                              |                               |                               |                              |                               |                              |                               ")

(defcustom elgantt-cal:header-column-offset 20
  "Width of the header column")

(defvar elgantt-cal::years-shown nil
  "A list of the number of years shown on the calendar")

(defun elgantt-cal--convert-date-to-column-number (date)				    
  "Assumes a YYYY-MM-DD date, returns the column number including the name offset column"
  (let ((spaces 0))
    (cl-subseq (elgantt--get-range-of-years)
	       0 (cl-position (string-to-number (substring date 0 4)) (elgantt--get-range-of-years)))
    ;; add the preceding years
    (dolist (year
	     (cl-subseq (elgantt--get-range-of-years)
			0 (cl-position (string-to-number (substring date 0 4)) (elgantt--get-range-of-years))))
      (if (elgantt--leap-year-p year)
	  (setq spaces (+ spaces 366 12))
	(setq spaces (+ spaces 365 12))))
    ;; add the current year
    (+ spaces (elgantt--convert-date-to-column-in-current-year date) elgantt--header-column-offset)))

(defun elgantt-cal:check-if-header-exists (header)
  (goto-char (point-min))
  (search-forward header nil t))

(defsubst elgantt-cal:get-days-in-year (year)
  (if (elgantt--leap-year-p year) 366 365))

(defsubst elgantt-cal:leap-year-p (year) 
  (= (% year 4) 0))

(defun elgantt-cal:insert-new-header-line (header years)
  (unless (search-forward header nil t)
    (goto-char (point-max))
    (insert "\n")
    (cl-loop for year in (-list years)
	     do (if (elgantt-cal:leap-year-p year)
		    (insert elgantt-cal:leap-year-blank-line)
		  (insert elgantt-cal:normal-year-blank-line)))))











