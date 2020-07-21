;;; elgant.el --- Generate  Gantt Charts from Orgmode files  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jeff Filipovits

;; Author: Jeff Filipovits <jrfilipovits@gmail.com>
;; Url: https://github.com/legalnonsense/elgantt
;; Version: 0.0.2
;; Package-Requires: ((emacs "26.1") (org "9.0") (s "1.12.0")
;;                    (ts "0.2") (org-ql "0.5-pre") (dash "2.16.0"))
;; Keywords: Org, agenda, calendar, outlines, gantt

;; This file is not part of GNU Emacs.

;;; Commentary:

;; El Gantt generates a text-based Gantt Chart/Calendar from orgmode files. 
;; El Gannt relies on the use of tags to designate how to generate the charts.
;; The goal is to for you to be able to customize your chart without altering
;; the way you use org mode. In other words, El Gantt allows you to customize
;; your charts while staying out of the way. The chart/calendar generated is
;; integrated with orgmode and can jump to the point of an org file and open
;; an agenda for each day of the chart. See the README. 

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;; Installation

;; Install these required packages:

;; + s
;; + org-ql
;; + dash
;; + ts

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'elgantt)

;;;; Usage

;; Run this command: 

;; `elgantt-open': Open a Gantt Calendar from your agenda files

;;;; Tips

;; + You can customize settings in the `elgantt' group.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'color)
(require 'org)
(require 'org-clock)
(require 'org-id) 

(require 'org-ql)
(require 's)
(require 'dash)
(require 'ts)

;;;; Customization
(defgroup elgantt-org nil
  "Options about gantt-org."
  :tag "Elgantt"
  :group 'org
  :group 'elgantt)

(defcustom elgantt-scroll-to-current-month-at-startup t
  "Scroll the calendar to the current month at startup.")

(defcustom elgantt-even-numbered-line-change 5
  "Percent to darken or lighten even numbered lines in the calendar. If using a dark 
background default face, lighten by this percent. If using a light background 
default face, darken by this percent.")

(defcustom elgantt-timestamps-to-display '(deadline
					   timestamp
					   timestamp-range
					   scheduled
					   timestamp-ia
					   timestamp-range-ia)
  "List of the types of timestamps to display in the calendar. Order matters. If an entry has two types of 
  timestamps, then the first found will be used to determine where it appears in the calendar.
  The following types are accepted: deadline timestamp timestamp-ia scheduled timestamp-range timestamp-range-ia.")

(defcustom elgantt-deadline-character "▲"
  "Character used for deadlines in the calendar.
Default: ▲")

(defcustom elgantt-use-inherited-hashtags nil
  "If using 'hashtag for `elgantt-header-type' , use inherited tags.")

(defcustom elgantt-level-prefix-char ?-
  "Use this prefix character to show header depth when the outline is unfolded.")

(defcustom elgantt-active-timestamp-character "●"
  "Character used for active timestamps in the calendar.
Default: ●")
(defcustom elgantt-inactive-timestamp-character "⊚"
  "Character used for inactive timestamps in the calendar. 
Default: ⊚")
(defcustom elgantt-scheduled-character "⬟"
  "Character used for active timestamps in the calendar.
Default: ⬟")
(defcustom elgantt-multiple-entry-character "☰"
  "Character used for cells which have multiple entries.
Default: ☰")
(defcustom elgantt-timestamp-range-start-character "▶"
  "Character shown at the beginning of a timerange.
Default: ▶")
(defcustom elgantt-timestamp-range-end-character "◀"
  "Character shown at the end of a timerange.
Default: ◀")
(defcustom elgantt-timestamp-range-ia-start-character "▷"
  "Character shown at the beginning of a timerange.
Default: ▷")
(defcustom elgantt-timestamp-range-ia-end-character "◁"
  "Character shown at the end of a timerange.
Default: ◁")

(defcustom elgantt-insert-blank-line-between-top-level-header t
  "Do you want to put a blank line between the top level headers?")

(defcustom elgantt-draw-overarching-headers nil
  "Draw a line bracketing the start and end dates for the children of 
and top-level headers, assuming there is no date already associated with the header.")

(defcustom elgantt-agenda-files (org-agenda-files)
  "A file, list of files, or function returning a list of files
Default: `org-agenda-files'.")

(defcustom elgantt-skip-archives t
  "Skip archived entries if non-nil.
Default: t")

(defcustom elgantt-start-date (concat (format-time-string "%Y-") "01-01")
  "Beginning date for the calendar as a string YYYY-MM-DD. 
Nothing before this date will be parsed or display. 
Default: the current year.")

(defcustom elgantt-header-column-offset 20
  "Width of the header column.
Default: 20.")

(defcustom elgantt-startup-folded nil
  "Whether subheadings are folded at startup.")

(defcustom elgantt-header-type 'outline
  "Define how to gather the headers. Can be: category, hashtag, root, outline,
or a function that returns the desired header. Default: 'outline.")

(defcustom elgantt-header-line-format
  '(:eval
    (let ((string (s-pad-right (window-total-width) " "
			       (concat (when (elgantt-get-date-at-point)
					 (s-pad-right 30 " " (elgantt-get-date-at-point)))
				       (when (elgantt-get-header-at-point)
					 (s-pad-right 30 " " (elgantt-get-header-at-point)))
				       (when-let ((headlines (elgantt-get-prop-at-point :elgantt-headline)))
					 (if (> (length headlines) 1)
					     (cl-loop for headline in headlines
						      concat (concat headline " / "))
					   (concat (car headlines))))))))
      (put-text-property 0 (length string) 'face 'elgantt-header-line-face string)
      string))
  "Display information about the cell at point in the header line. See `header-line-format'.")

(defcustom elgantt-exclusions nil
  "Exclude headings mathing these strings or regexps
in this list from display in the calendar.
Default: nil")

(defcustom elgantt-insert-header-even-if-no-timestamp nil
  "Insert a header even if there is no timestamp. 
Parent headings will still be inserted if a child has a timestamp, even
if this is set to nil.")

;; The names refer to the corners of a square. 
(defcustom elgantt-draw--top-left "╭"
  "Line drawing char: top left.")
(defcustom elgantt-draw--top-right "╮"
  "Line drawing char: top right.")
(defcustom elgantt-draw--bottom-left "╰"
  "Line drawing char: bottom left.")
(defcustom elgantt-draw--bottom-right "╯"
  "Line drawing char: bottom right.")
(defcustom elgantt-draw--horizontal-line "─"
  "Line drawing char: horizontal line.")
(defcustom elgantt-draw--vertical-line "│"
  "Line drawing char: vertical line.")
(defcustom elgantt-draw--top-half-vertical "╵"
  "Line drawing char: top half vertical line.")
(defcustom elgantt-draw--bottom-half-vertical "╷"
  "Line drawing char: bottom half vertical line.")

;;;; Faces

(defface elgantt-vertical-line-face
  '((t (:height .1)))
  "Vertical line face")

(defface elgantt-header-line-face '((t (:inherit default)))
  "Header line face.")

(defface elgantt-odd-numbered-line-face '((t (:inherit default)))
  "Face applied to odd numbered lines in the calendar.")

(defface elgantt-even-numbered-line-face '((t (:inherit default)))
  "Face applied to even numbered lines in the calendar. Do not set this face.")

;;;; Constants
(defconst elgantt-leap-year-month-line
  (concat  "| January xxxx                  "
	   "| February xxxx               "
	   "| March xxxx                    "
	   "| April xxxx                   "
	   "| May xxxx                      "
	   "| June xxxx                    "
	   "| July xxxx                     "
	   "| August xxxx                   "
	   "| September xxxx               "
	   "| October xxxx                  "
	   "| November xxxx                "
	   "| December xxxx                 "))

(defconst elgantt-leap-year-date-line
  (concat "|1234567890123456789012345678901"
	  "|12345678901234567890123456789"
	  "|1234567890123456789012345678901"
	  "|123456789012345678901234567890"
	  "|1234567890123456789012345678901"
	  "|123456789012345678901234567890"
	  "|1234567890123456789012345678901"
	  "|1234567890123456789012345678901"
	  "|123456789012345678901234567890"
	  "|1234567890123456789012345678901"
	  "|123456789012345678901234567890"
	  "|1234567890123456789012345678901"))

(defconst elgantt-leap-year-blank-line
  (concat "|                               "
	  "|                             "
	  "|                               "
	  "|                              "
	  "|                               "
	  "|                              "
	  "|                               "
	  "|                               "
	  "|                              "
	  "|                               "
	  "|                              "
	  "|                               "))


(defconst elgantt-normal-year-month-line
  (concat  "| January xxxx                  "
	   "| February xxxx              "
	   "| March xxxx                    "
	   "| April xxxx                   "
	   "| May xxxx                      "
	   "| June xxxx                    "
	   "| July xxxx                     "
	   "| August xxxx                   "
	   "| September xxxx               "
	   "| October xxxx                  "
	   "| November xxxx                "
	   "| December xxxx                 "))

(defconst elgantt-normal-year-date-line
  (concat "|1234567890123456789012345678901"
	  "|1234567890123456789012345678"
	  "|1234567890123456789012345678901"
	  "|123456789012345678901234567890"
	  "|1234567890123456789012345678901"
	  "|123456789012345678901234567890"
	  "|1234567890123456789012345678901"
	  "|1234567890123456789012345678901"
	  "|123456789012345678901234567890"
	  "|1234567890123456789012345678901"
	  "|123456789012345678901234567890"
	  "|1234567890123456789012345678901"))

(defconst elgantt-normal-year-blank-line
  (concat "|                               "
	  "|                            "
	  "|                               "
	  "|                              "
	  "|                               "
	  "|                              "
	  "|                               "
	  "|                               "
	  "|                              "
	  "|                               "
	  "|                              "
	  "|                               "))

(defconst elgantt-cell-entry-re
  (concat "["
	  elgantt-deadline-character
	  elgantt-active-timestamp-character
	  elgantt-inactive-timestamp-character
	  elgantt-scheduled-character
	  elgantt-multiple-entry-character
	  elgantt-timestamp-range-end-character
	  elgantt-timestamp-range-start-character
	  elgantt-timestamp-range-ia-end-character
	  elgantt-timestamp-range-ia-start-character
	  "]")
  "List of display characters for use as a regexp for finding entries.")

(defconst elgantt-vertical-line-char "|"
  "Character used to draw lines between dates.")

(defmacro elgantt--add-vertical-line-props (lines)
  "Place vertical line text properties."
  (let ((body (cl-loop for line in lines
                       collect `(setq ,line (s-replace "|" ,elgantt-vertical-line-char ,line)))))
    `(progn ,@body)))

;;;; Private variables
(defvar elgantt--parsing-functions nil
  "List of functions for parsing org files.")

(defvar elgantt--display-rules nil
  "List of functions for drawing overlays in the buffer based on underlying
text properties.")

(defvar elgantt--date-range nil
  "Range of years present in the agenda files.")

(defvar elgantt--vertical-bar-overlay-list nil
  "List of overlays for the vertical selection bar.")

(defvar elgantt--post-command-hooks nil
  "Post command hooks added by user.")

;;;; Functions

(defun elgantt--change-symbol-name (symbol &optional prefix suffix
					   substring-start substring-end)
  "SYMBOL is any symbol name. PREFIX and SUFFIX are a string to be
prepended or appended to the symbol name and returned as a new 
symbol."
  (intern (concat prefix
		  (substring (symbol-name symbol)
			     substring-start substring-end)
		  suffix)))

(defun elgantt--add-remove-prop-colon (prop &optional remove)
  "PROP is a symbol with or without a colon prefix. 
Returns a symbol with a colon prefix. If REMOVE is t, 
then returns a symbol without a colon prefix."
  (if remove
      (if (s-starts-with-p ":" (symbol-name prop))
	  (intern (substring (symbol-name prop) 1))			
	prop)
    (if (s-starts-with-p ":" (symbol-name prop))
	prop			
      (intern (concat ":" (symbol-name prop))))))

(defun elgantt--plist-pair-p (plist key val &optional predicate)
  "Return t if PLIST has the KEY and VAL pair. Tests using `equal'.
Optional PREDICATE provides a function which performs equality test."
  (when-let ((stored-val (plist-get plist key)))
    (cond ((not predicate)
	   (equal stored-val val))
	  ((functionp predicate)
	   (funcall predicate stored-val val)))))

(defun elgantt--zip (args)
  "Zips multiple lists together. Example:
(elgantt--zip '((1 5 9) (2 6 10) (3 7 11) (4 8 12)))
=> '((1 2 3 4) (5 6 7 8) (9 10 11 12)).
All lists must be the same length."
  (if (catch 'match ; Check if lists are all the same length
	(dotimes (x (1- (length args)))
	  (when (/= (length (nth x args))
		    (length (nth (1+ x) args)))
	    (throw 'match nil)))
	(throw 'match t))
      (let (zip subzip)
	(dotimes (_ (length (car args)))
	  (setq subzip nil)
	  (dotimes (x (length args))
	    (push (pop (nth x args)) subzip))
	  (push (reverse subzip) zip))
	(reverse zip))
    (user-error "Lists are not all the same length.")))

(defsubst elgantt--get-days-in-year (year)
  "Return the number of days in YEAR." 
  (if (elgantt--leap-year-p year) 366 365))

(defun elgantt--leap-year-p (year)
  "Return t if YEAR is a leap year. Otherwise, nil."
  (cond ((/= (% year 4) 0) nil)
	((/= (% year 100) 0) t)
	((/= (% year 400) 0) nil)
	(t t)))

(defun elgantt--sort-dates (dates)
  "Put a list of DATES in order."
  (sort (-non-nil dates) #'string<))

(defun elgantt--date-compare-p (pred date1 date2)
  "Apply PRED to date1 and date2. 
PRED is one of =, <, <=, >=, >, or /=."
  (pcase pred
    (`/= (not (string= date1 date2)))
    (`> (string> date1 date2))
    (`< (string< date1 date2))
    (`= (string= date1 date2))
    (`<= (or (string< date1 date2)
	     (string= date1 date2)))
    (`>= (or (string> date1 date2)
	     (string= date1 date2)))
    (_ (error "Invalid predicate."))))

(defun elgantt--date-earlier-p (date1 date2)
  "Return t if date1 is before date2."
  (string= date1
	   (car (elgantt--sort-dates (list date1 date2)))))

(defun elgantt--convert-date-string (date)
  "Converts an org date string to YYYY-MM-DD."
  (->> date
       (ts-parse-org)
       (ts-format "%Y-%m-%d")))

(defun elgantt--convert-date-to-column-number (date)
  "Accepts a date in the form of YYYY-MM-DD and returns
the column of that date."
  (let ((spaces 0))
    (cl-subseq elgantt--date-range
	       0 (cl-position (string-to-number (substring date 0 4))
			      elgantt--date-range))
    (dolist (year
	     (cl-subseq elgantt--date-range
			0 (cl-position (string-to-number (substring date 0 4))
				       elgantt--date-range)))
      (if (elgantt--leap-year-p year)
	  (setq spaces (+ spaces 366 12))
	(setq spaces (+ spaces 365 12))))
    (+ spaces
       (elgantt--convert-date-to-column-in-current-year date)
       elgantt-header-column-offset)))

(defun elgantt--convert-date-to-day-number-in-year (date)
  "accept a date in the format YYYY-MM-DD and return an int of day number of the year"
  (time-to-day-in-year (encode-time 0 0 0 (string-to-number (substring date 8 10))
				    (string-to-number (substring date 5 7))
				    (string-to-number (substring date 0 4)))))

(defun elgantt--convert-date-to-column-in-current-year (date)
  "accepts a date YYYY-MM-DD and returns the position on the horizontal calendar (int)
this works on leap years"
  (+ (elgantt--convert-date-to-day-number-in-year date)
     (- (string-to-number (substring date 5 7)) 1)))

(defun elgantt--date-calc (date offset &optional unit)
  "DATE is a string \"YYYY-MM-DD\"
OFFSET is a positive or negative integer representing
the number of days. UNIT should be day, month, year."
  (->> date
       (ts-parse)
       (ts-adjust (or unit 'day) offset)
       (ts-format "%Y-%m-%d")))

(defun elgantt--create-overlay (&optional begin end properties)
  "Create an overlay from BEGIN to END with PROPERTIES. If BEGIN is
nil, then create the overlay at point. If END is nil, then create
the overlay at BEGIN. Returns the new overlay."
  (let ((overlay (make-overlay (or begin (point))
			       (or end (1+ (point)))))
	(len (length properties))
	(i 0))
    (overlay-put overlay :elgantt t)
    (while (< i len)
      (overlay-put overlay
		   (nth i properties) (nth (setq i (1+ i)) properties))
      (setq i (1+ i)))
    overlay))

(defun elgantt--parser ()
  "Runs at each org heading and returns a plist of 
relevant properties to be inserted into the calendar buffer."
  (-let* (((&alist "CATEGORY" elgantt-category
		   "ITEM" elgantt-headline
		   "FILE" elgantt-file
		   "TIMESTAMP" elgantt-timestamp
		   "TIMESTAMP_IA" elgantt-timestamp-ia
		   "DEADLINE" elgantt-deadline
		   "SCHEDULED" elgantt-scheduled
		   "TODO" elgantt-todo
		   "ALLTAGS" elgantt-alltags
		   "ELGANTT-DEPENDENTS" elgantt-dependents
		   "ELGANTT-ANCHOR" elgantt-anchor)
	   (org-entry-properties))
	  ;; Return a new property list to be
	  ;; assigned to the cell. The first set
	  ;; match proerties from `org-entry-properties'.
	  (props (list :elgantt-category (or (org-entry-get-with-inheritance "CATEGORY")
					     elgantt-category)
		       :elgantt-headline elgantt-headline
		       :elgantt-level (if (eq elgantt-header-type 'outline)
					  (org-outline-level)
					1)
		       :elgantt-file elgantt-file
		       :elgantt-deadline (when elgantt-deadline
					   (elgantt--convert-date-string elgantt-deadline))
		       :elgantt-scheduled (when elgantt-scheduled
					    (elgantt--convert-date-string elgantt-scheduled))
		       :elgantt-todo elgantt-todo
		       :elgantt-marker (point-marker)
		       ;; Don't get the timestamps if they are ranges.
		       :elgantt-timestamp (when (and elgantt-timestamp
						     (not (s-match "--" elgantt-timestamp)))
					    (elgantt--convert-date-string elgantt-timestamp))
		       :elgantt-timestamp-ia (when (and elgantt-timestamp-ia
							(not (s-match "--" elgantt-timestamp-ia)))
					       (elgantt--convert-date-string elgantt-timestamp-ia))
		       ;; Don't get the ranges if they are single dates.
		       :elgantt-timestamp-range (when elgantt-timestamp
						  (if (not (s-match "--" elgantt-timestamp))
						      nil
						    (let ((dates (s-split "--" elgantt-timestamp)))
						      (list (elgantt--convert-date-string
							     (car dates))
							    (elgantt--convert-date-string
							     (cadr dates))))))
		       :elgantt-timestamp-range-ia (when elgantt-timestamp-ia
						     (if (not (s-match "--" elgantt-timestamp-ia))
							 nil
						       (let ((dates (s-split "--"
									     elgantt-timestamp-ia)))
							 (list (elgantt--convert-date-string
								(car dates))
							       (elgantt--convert-date-string
								(cadr dates))))))
		       ;; Clean up the tags
		       :elgantt-hashtag (--first (s-starts-with-p "#" it)
						 (org-get-tags nil
							       (not elgantt-use-inherited-hashtags)))
		       :elgantt-alltags (when-let ((tag-string elgantt-alltags))
					  (mapcar #'org-no-properties (s-split ":" tag-string t)))
		       :elgantt-folded elgantt-startup-folded
		       :elgantt-header (pcase elgantt-header-type
					 ('root (save-excursion 
						  (while (org-up-heading-safe))
						  (cdar (org-entry-properties (point) "ITEM"))))
					 ('hashtag (when-let ((hashtag
							       (--first (s-starts-with-p "#" it)
									(org-get-tags nil
										      (not elgantt-use-inherited-hashtags)))))
						     (substring hashtag 1)))
					 ('outline elgantt-headline)
					 ('category (or (org-entry-get-with-inheritance "CATEGORY")
							elgantt-category))
					 ('parent (save-excursion
						    (when (org-up-heading-safe)
						      (cdar (org-entry-properties (point) "ITEM")))))
					 ((pred functionp) (funcall elgantt-header-type))
					 (_ (error "Invalid header type.")))
		       :elgantt-parent (pcase elgantt-header-type
					 ('root (save-excursion 
						  (while (org-up-heading-safe))
						  (cdar (org-entry-properties (point) "ITEM"))))
					 ('hashtag
					  (when-let ((hashtag
						      (--first (s-starts-with-p "#" it)
							       (org-get-tags nil
									     (not elgantt-use-inherited-hashtags)))))
					    (substring hashtag 1)))
					 ('outline elgantt-headline)
					 ('category (or (org-entry-get-with-inheritance "CATEGORY")
							elgantt-category))
					 ('parent (save-excursion
						    (when (org-up-heading-safe)
						      (cdar (org-entry-properties (point) "ITEM")))))
					 ((pred functionp) (funcall elgantt-header-type))
					 (_ (error "Invalid header type.")))
		       :elgantt-parent-level (save-excursion (while (org-up-heading-safe))
							     (org-current-level))
		       :elgantt-org-buffer (current-buffer)
		       :elgantt-org-parent (save-excursion (while (org-up-heading-safe))
							   (cons (cdar (org-entry-properties (point) "ITEM"))
								 (org-id-get-create)))
		       :elgantt-outline-path (save-excursion (cl-loop while (org-up-heading-safe)
								      collect (cons (org-no-properties
										     (org-get-heading))
										    (org-id-get-create)))))))
    ;; If the header is in `elgantt-exclusions', then don't add it.
    ;; If :elgantt-header is nil, don't add it
    (unless (or (member (plist-get props :elgantt-header) elgantt-exclusions)
		(not (plist-get props :elgantt-header)))
      (setq props (append props
			  ;; Set the date if it contains a date type in `elgantt-timestamps-to-display'
			  `(:elgantt-date ,(plist-get props
						      (elgantt--change-symbol-name
						       (--first (plist-get props
									   (elgantt--change-symbol-name it ":elgantt-"))
								elgantt-timestamps-to-display)
						       ":elgantt-")))
			  `(:elgantt-date-type ,(--first (plist-get props
								    (elgantt--change-symbol-name it ":elgantt-"))
							 elgantt-timestamps-to-display))))
      (if
	  (or (plist-get props :elgantt-date)
	      elgantt-insert-header-even-if-no-timestamp)
	  (append props
		  `(:elgantt-org-id ,(org-id-get-create))
		  ;; Append properites from `org-element-at-point' in
		  ;; case anyone wants to use them.
		  (cadr (org-element-at-point))
		  ;; Run all custom parsing functions and append
		  ;; those values
		  (-flatten-n 1
			      (cl-loop for (prop . function) in elgantt--parsing-functions
				       collect `(,prop ,(funcall function)))))))))

(defun elgantt--iterate ()
  "Iterate over all entries in `elgantt-agenda-files'."
  (if elgantt-insert-header-even-if-no-timestamp
      ;; Seems wasteful to write two separate queries, but
      ;; I do not know how to format a single org-ql query
      ;; to include or exclude timestamps depending on the value of 
      ;; the horribly named `elgantt-insert-header-even-if-no-timestamp'
      (mapc #'elgantt--insert-entry
	    (-non-nil
	     (org-ql-select elgantt-agenda-files
	       `(not (tags ,(when elgantt-skip-archives
			      org-archive-tag)))
	       :action #'elgantt--parser)))
    (mapc #'elgantt--insert-entry
	  (-non-nil
	   (org-ql-select elgantt-agenda-files
	     `(and (ts :from ,elgantt-start-date)
		   (not (tags ,(when elgantt-skip-archives
				 org-archive-tag))))
	     :action #'elgantt--parser)))))

(defun elgantt--on-vertical-line-p ()
  "Is the cursor on a vertical line?"
  (looking-at "|"))

(defun elgantt--select-entry (&optional prop-or-all)
  "Prompt the user to select from multiple entries.
If PROP is `all', then skip the prompt and return the
list of all props at point. (i.e., the same thing as
				   `elgantt-get-props-at-point')"
  (when-let ((prop-list (elgantt-get-prop-at-point)))
    (cond ((eq prop-or-all 'all)
	   ;; If user wants all entries, return them
	   prop-list)
	  ((= (length prop-list) 1)
	   ;; If there is only one entry, return it,
	   ;; as an unnested list (hence the use of car).
	   ;; If there are two entries with 'all, it
	   ;;  will return a nested list; if there 
	   ;; is one entry, the list is not nested. 
	   ;; TODO: figure out why I wrote it this way.
	   (car prop-list))
	  (t
	   ;; Otherwise, there are more than one entry
	   ;; and the user only wants one of them.
	   ;; Prompt the user to select which one. 
	   ;; TODO: turn this into an elgantt-selection-function that
	   ;; can be customized by the user
	   (let ((selection (completing-read "Select entry: "
					     (elgantt-get-prop-at-point :elgantt-headline)
					     nil
					     'require-match)))
	     (-first (lambda (x) (-contains? x selection)) prop-list))))))

(defun elgantt-get-header-at-point ()
  "Gets the header of the cell's current position.
Returns nil if not on a header line."
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) 'elgantt-header)))

(defun elgantt-get-date-at-point (&optional column)
  "Get the date at point in YYYY-MM-DD format. NOTE: this gets this date 
from the location in the calendar, and does not rely on the text properties. 
It works on empty cells, and does not rely on the :elgantt-date property." 
  ;; HACK: It works, but...
  (let ((deactivate-mark t)) 
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
		    (goto-char (point-min))
		    (move-to-column column)
		    (if (re-search-backward "|" nil t)
			(progn
			  (re-search-forward "[[:alpha:]]+" nil t)
			  (setq date (concat (match-string 0) " " date))
			  (if (re-search-forward "[[:digit:]]+" nil t)
			      (progn
				(setq date (concat date " " (match-string 0)))
				(let ((day (org-day-of-week (nth 3 (parse-time-string date))
							    (nth 4 (parse-time-string date))
							    (nth 5 (parse-time-string date)))))
				  (setq date (concat date)))
				(setq date (ts-format "%Y-%m-%d" (ts-parse date))))
			    (setq date "")))
		      (setq date "")))
		(setq date "")))
	    date))
      "")))

(defun elgantt-get-prop-at-point (&optional prop)
  "Returns the :elgantt text property value. 
This will be a list of plists with the values stored by
`elgantt--parser'. 

If PROP is specified, return the value of that property in a list.
If there is more than one entry in the cell, the list will contain
the value of each entry."
  (let ((prop-list (plist-get (text-properties-at (point)) :elgantt)))
    (if prop
	(mapcar (lambda (props) (plist-get props prop))
		prop-list)
      prop-list)))

;; User movement functions
(defun elgantt--forward-line (n)
  "Same as `forward-line', but preserves the current column."
  ;; `next-line' would work, but the compiler complains if we
  ;; use that function.
  (let ((col (current-column)))
    (forward-line n)
    (move-to-column col)))

(defun elgantt--scroll-to-beginning ()
  (cl-loop for overlay in elgantt--hidden-overlays
	   do (delete-overlay overlay)
	   finally (setq elgantt--hidden-overlays nil)))

(defun elgantt-scroll (direction)
  "Place, or move, an overlay on each line, hiding (or showing)
the month immediately after the headers.
DIRECTION must be a symbol: `forward' or `backard'."
  ;; HACK: This was a first draft, but it works well enough. 
  (let ((column (current-column))
	(line (line-number-at-pos))
	(date (elgantt-get-date-at-point)))
    (cond ((and (not elgantt--hidden-overlays)
		(eq direction 'forward))
	   (progn
	     (goto-char (point-min))
	     (setq elgantt--hidden-overlays 
		   (cl-loop with num-lines = (count-lines (point-min) (point-max))
			    for line from 1 to num-lines
			    collect (make-overlay (progn (move-to-column (1+ elgantt-header-column-offset)) (point))
						  (search-forward "|" (point-at-eol) t))
			    until (= (line-number-at-pos) num-lines)
			    do (forward-line)))
	     (cl-loop for overlay in elgantt--hidden-overlays
		      do (overlay-put overlay 'invisible t))))
	  (elgantt--hidden-overlays
	   (let ((end (pcase direction
			(`forward
			 (lambda (overlay)
			   (save-excursion
			     (goto-char (overlay-end overlay))
			     (search-forward "|" (point-at-eol) t))))
			(`backward
			 (lambda (overlay)
			   (goto-char (overlay-end overlay))
			   (search-backward "|" (point-at-bol) t)
			   (let ((x (search-backward "|" (point-at-bol) t)))
			     (when x
			       (1+ x))))))))
	     (if (funcall end (car elgantt--hidden-overlays))
		 (cl-loop for overlay in elgantt--hidden-overlays
			  do (move-overlay overlay
					   (overlay-start overlay)
					   (funcall end overlay)))
	       (when (eq direction 'backward)
		 (setq elgantt--hidden-overlays nil))))))
    ;; This is an elaborate `save-excursion' to try to keep the
    ;; point in a reasonable place
    (goto-line line)
    (if (not (string= "" date ))
	(elgantt--goto-date date)
      (move-to-column column))))

(defun elgantt-scroll-to-current-month ()
  "Scroll the calendar to the current month."
  (interactive)
  (when (member (string-to-number (format-time-string "%Y")) elgantt--date-range)
    (cl-loop for overlay in elgantt--hidden-overlays
	     do (delete-overlay overlay)
	     finally (setq elgantt--hidden-overlays nil))
    (dotimes (_ (+ (* (- (car (last (calendar-current-date))) 
			 (car elgantt--date-range))
		      12)
		   (1- (string-to-number (substring (format-time-string "%Y-%m-%d") 5 7)))))
      (elgantt-scroll-forward))))

(defun elgantt--move-vertically (up-or-down)
  "Move up or down one line, and then move to the nearest
entry. UP-OR-DOWN must be 'up or 'down."
  (if (eq up-or-down 'up)
      (if (> (org-current-line) 3)
	  (elgantt--forward-line -1)
	(return-from elgantt--move-vertically nil))
    (if (< (org-current-line) (count-lines (point-min) (point-max)))
	(elgantt--forward-line 1)
      (return-from elgantt--move-vertically nil)))
  (let ((next (save-excursion (re-search-forward elgantt-cell-entry-re (point-at-eol) t)))
	(previous (save-excursion (re-search-backward elgantt-cell-entry-re (point-at-bol) t))))
    (when (or next previous)
      (cond ((and (not next) (not previous))
	     (elgantt--move-vertically up-or-down))
	    ((and (not next) previous)
	     (goto-char previous))
	    ((and (not previous) next)
	     (goto-char (1- next)))
	    (t (if (< (- next (point)) (- (point) previous))
		   (goto-char (1- next))
		 (goto-char previous)))))))

(defun elgantt--reset-org-ql-cache ()
  "Invalidate the org-ql cache."
  (setq org-ql-cache (make-hash-table :weakness 'key)))

(defun elgantt--move-horizontally (n)
  "Moves N chars and ensures that the point is not on a vertical line."
  (forward-char n)
  (when (elgantt--on-vertical-line-p)
    (forward-char n)))

;; Programmatic movement functions 
(defmacro elgantt--parse-org-files-over-cells (&rest body)
  "Executes BODY at each cell in the calendar, iterating in order
of buffer position."
  `(save-excursion
     (goto-char (point-min))
     (cl-loop for points being the intervals of (current-buffer) property :elgantt
	      do (progn (goto-char (car points))
			(when (elgantt-get-prop-at-point)
			  ,@body)))))


(defun elgantt--next-match (property value)
  "Returns the point of the next (chronologically) cell that has PROPERTY and VALUE.
Returns a list containing the nearest matches that fall on the same date, sorted top to bottom.
Returns nil if there are no additional matches. Does not mach a cell which falls on the 
same day as the current cell."
  (save-excursion
    (cl-loop with target-point = nil
	     with target-date  = nil
	     with start-col = (current-column)
	     for points being the intervals of (current-buffer) property :elgantt
	     do (goto-char (car points))
	     when (and (> (current-column) start-col)
		       (--first (member value (-list it)) (elgantt-get-prop-at-point property)))
	     do (cond ((not target-date)
		       (setq target-date (elgantt-get-date-at-point)
			     target-point (point)))
		      ((elgantt--date-compare-p '< (elgantt-get-date-at-point) target-date)
		       (setq target-date (elgantt-get-date-at-point)
			     target-point (point)))
		      ((elgantt--date-compare-p '= (elgantt-get-date-at-point) target-date)
		       (setq target-point (append (-list target-point) (-list (point))))))
	     finally return (-list target-point))))

(defun elgantt--previous-match (property value)
  "Returns the point of the next (chronologically) cell that has PROPERTY and VALUE.
Returns a list containing the nearest matches that fall on the same date, sorted top to bottom.
Returns nil if there are no additional matches. Does not mach a cell which falls on the 
same day as the current cell."
  (elgantt--next-match property value t))

(defun elgantt--goto-id (id)
  "Go to the cell containing the org-id ID. Return nil if not found.
If ID represents an entry that is a time range, go do the first 
cell in that range."
  ;; If you want to go to the last date in a range, you should
  ;; goto the starting point, get :elgantt-timestamp-range and then
  ;; use `elgantt--goto-date' to go to the cadar of that value,
  ;; unless there is more than one entry in that cell, in which case
  ;; you are on your own.
  (when-let ((point (cl-loop for points being the intervals of (current-buffer) property :elgantt
			     thereis (save-excursion
				       (goto-char (car points))
				       (let ((props (elgantt-get-prop-at-point)))
					 (when (-first (lambda (x)
							 (elgantt--plist-pair-p x :elgantt-org-id id))
						       props)
					   (car points)))))))
    (goto-char point)))

(defun elgantt--goto-date (date)
  "Go to DATE in the current line. 
DATE is a string in \"YYYY-MM-DD\" format."
  (if-let ((overlay (car elgantt--hidden-overlays))
	   (start (overlay-start overlay))
	   (end (overlay-end overlay)))
      (move-to-column (- (elgantt--convert-date-to-column-number date)
			 (- end start)))
    (move-to-column (elgantt--convert-date-to-column-number date)))
  (point))

;; Interaction functions

(defmacro elgantt-with-point-at-orig-entry (props &rest body)
  "Execute BODY with point at marker stored in `:elgantt-marker'.
Buffer is retrieved from the `:elgantt-org-buffer' property. If PROPS is nil, 
then retrieve PROPS with `elgantt--select-entry’.
If PROPS is supplied, use those props instead of the props at point."
  (declare (indent 2))
  `(let* ((props (or ,props (elgantt--select-entry)))
	  (marker (plist-get props :elgantt-marker))
	  (buffer (plist-get props :elgantt-org-buffer)))
     (with-current-buffer buffer
       (when (or (> marker (point-max))
		 (< marker (point-min)))
	 (widen))
       (goto-char marker)
       ,@body)))

;; Calendar drawing functions
(defun elgantt-change-header-column-offset (&optional offset)
  "Changes `elgantt-header-column-offset' and reloads the calendar."
  (interactive)
  (setq elgantt-header-column-offset (or offset
					 (read-number "Enter new header column offset: ")))
  (elgantt-open))

(defun elgantt--draw-month-line (year)
  "Inserts the top month line for the calendar."
  (let ((inhibit-read-only t))
    (insert 
     (if (elgantt--leap-year-p year)
	 (replace-regexp-in-string "xxxx" (number-to-string year) 
				   elgantt-leap-year-month-line)
       (replace-regexp-in-string "xxxx" (number-to-string year) 
				 elgantt-normal-year-month-line)))))

(defun elgantt--draw-number-line (year)
  "Inserts the number line for the calendar."
  (let ((inhibit-read-only t))
    (insert (if (elgantt--leap-year-p year)
		elgantt-leap-year-date-line
	      elgantt-normal-year-date-line))))

(defun elgantt--draw-blank-line (year)
  "Inserts a blank line for each header of the calendar."
  (let ((inhibit-read-only t))
    (insert (if (elgantt--leap-year-p year)
		elgantt-leap-year-blank-line
	      elgantt-normal-year-blank-line))))

(defface elgantt-outline-level-1 '((t (:underline t)))
  "Face applied to top level headers.")

(defun elgantt--get-header-create (props)
  "Put point at the first char in the HEADER line, creating a new header
line if one does not exist."
  (let* ((outlinep (eq 'outline elgantt-header-type))
	 (level (plist-get props :elgantt-level))
	 (parent-level (plist-get props :elgantt-parent-level))
	 (foldedp (plist-get props :elgantt-folded))
	 (headline-text (plist-get props :elgantt-headline))
	 (header-id (plist-get props :elgantt-org-id))
	 (parent-text (if outlinep
			  (car (plist-get props :elgantt-org-parent))
			(plist-get props :elgantt-parent)))
	 (parent-id (cdr (plist-get props :elgantt-org-parent))))
    
    (cl-flet* ((at-header-p (text)
			    (string= (get-text-property (point-at-bol) 'elgantt-header)
				     text))
	       (at-id-p (id)
			(string= (get-text-property (point-at-bol) 'elgantt-org-id)
				 id))
	       (format-header (text lev &optional no-prefix)
			      (concat
			       (unless no-prefix
				 (if outlinep 
				     (make-string (1- lev) (pcase elgantt-level-prefix-char
							     ((pred characterp) elgantt-level-prefix-char)
							     ((pred stringp) (string-to-char elgantt-level-prefix-char))))
				   (pcase elgantt-level-prefix-char
				     ((pred stringp) elgantt-level-prefix-char)
				     ((pred characterp) (char-to-string elgantt-level-prefix-char)))))
			       (s-truncate elgantt-header-column-offset text)))
	       (insert-parent-header ()
				     (let ((it (format-header parent-text parent-level (not outlinep))))
				       (put-text-property 0 (length it) 'elgantt-header parent-text it)
				       (put-text-property 0 (length it) 'elgantt-level (if outlinep parent-level -1) it)
				       (put-text-property 0 (length it) 'elgantt-org-id parent-id it)
				       (put-text-property 0 (length it) 'elgantt-parent t it)
				       (put-text-property 0 (length it) 'elgantt-folded foldedp it)
				       (when elgantt-insert-blank-line-between-top-level-header
					 (unless (and outlinep parent-level (/= parent-level 1))
					   (elgantt--insert-new-header-line "")))
				       (elgantt--insert-new-header-line it)))
	       (insert-current-header ()
				      (let ((it (format-header headline-text level)))
					(put-text-property 0 (length it) 'elgantt-header headline-text it)
					(put-text-property 0 (length it) 'elgantt-level level it)
					(put-text-property 0 (length it) 'elgantt-org-id header-id it)
					(put-text-property 0 (length it) 'elgantt-folded foldedp it)
					(elgantt--insert-new-header-line it (or (eq elgantt-header-type 'outline)
										(functionp elgantt-header-type)))))
	       (parent-found-p () (cl-loop initially do (goto-char (point-min))
					   do (forward-line)
					   until (eobp)
					   if (and (at-header-p parent-text)
						   (if outlinep (at-id-p parent-id) t))
					   return (point)))
	       (header-found-p () (cl-loop initially do (goto-char (point-min))
					   do (forward-line)
					   until (eobp)
					   if (and (at-header-p headline-text)
						   (if (not foldedp)
						       (at-id-p header-id) t))
					   return (point))))
      ;; Good luck figuring this one out. 
      (cond (foldedp
	     (cond
	      ((parent-found-p)
	       (if (eq 'outline elgantt-header-type)
		   (elgantt--move-to-next 'parent-or-sibling)
		 nil))
	      (t (insert-parent-header))))
	    (t (cond ((parent-found-p)
		      (cond
		       ((header-found-p) nil)
		       (t
			(if (eq 'outline elgantt-header-type)
			    (progn
			      (elgantt--move-to-next 'parent-or-sibling)
			      (insert-current-header))
			  (parent-found-p)
			  (elgantt--move-to-next 'parent-or-sibling)
			  (forward-char -1)
			  (insert-current-header)))))
		     (t (insert-parent-header)
			(unless (equal parent-id header-id)
			  (insert-current-header)))))))))

(defun elgantt--get-level-at-point ()
  "If using the 'heading option of `elgantt-header-type', 
then get the level of the current header."
  (get-text-property (point-at-bol) 'elgantt-level))

(defun elgantt--shift-date (n &optional properties)
  "Move the timestamp up or down by one day.
N must be 1 or -1. The return value
is the prop list of the entry that has been moved."
  ;; Only allows moving by a single day
  (unless (or (= n 1)
	      (= n -1))
    (error "elgantt--shift-date: Invalid argument. N must be 1 or -1."))
  (when (looking-at elgantt-cell-entry-re)
    (let* ((props (or properties
		      (elgantt--select-entry)))
	   (date (elgantt-get-date-at-point))
	   ;; Some of the symbols stored
	   ;; in :elgantt-date-type need to be changed
	   ;; to work with `org-re-timestamp'
	   (type (pcase (plist-get props :elgantt-date-type)
		   (`timestamp 'active)
		   (`timestamp-ia 'inactive)
		   (other other))))
      (elgantt-with-point-at-orig-entry props
	  (when (re-search-forward
		 ;; BUG: if in a timestamp range, the only way to move it
		 ;; is to search for the specific date; otherwise
		 ;; we rely on `org-re-timestamp' to find the date
		 ;; to shift. This could case an issue if there is a
		 ;; scheduled time that has the same date as a timerange
		 ;; because the regexp will find the scheduled date first.
		 (if (or (eq type 'timestamp-range)
			 (eq type 'timestamp-range-ia))
		     (concat
		      "[[<]\\("
		      date
		      " ?[^]\n>]*?\\)[]>]\\|"
		      "\\(?:<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+"
		      "[dwmy]>\\)\\|\\(?:<%%\\(?:([^>\n]+)\\)>\\)")
		   (org-re-timestamp type)) nil t)
	    (org-timestamp-change n 'day)))
      ;; For some reason, `save-excursion' does not work here.
      (let ((point (point)))
	(elgantt-update-this-cell)
	(elgantt--update-display-this-cell)
	(goto-char point))
      (elgantt--move-horizontally n)
      (elgantt-update-this-cell)
      (elgantt--update-display-this-cell))))

(defun elgantt--shift-date-forward ()
  "Move the entry at point forward by one day."
  (interactive)
  (elgantt--shift-date 1))

(defun elgantt--shift-date-backward ()
  "Move the entry at point backward by one day."
  (interactive)
  (elgantt--shift-date -1))

(defmacro elgantt--with-point-at (point &rest body)
  (declare (indent defun))
  `(save-excursion (goto-char ,point)
		   (progn ,@body)))

(defun elgantt--move-to-next (&optional place)
  "Move to the next PLACE, where PLACE is 'parent,
'sibling, or 'parent-or-sibling. If SAVE-EXCURSION is non-nil,
then return the point but don't move to it.

If `elgantt-header-type' is not 'outline, then 
this will move to the next header, regardless of the value of 
PLACE."
  (when (and (null place)
	     (eq 'outline elgantt-header-type))
    (error "Must supply an argument if elgantt-header-type is 'outline."))
  (let ((level (elgantt--get-level-at-point))
	(start-line (line-number-at-pos)))
    (cl-flet* ((foundp ()
		       (if (eq 'outline elgantt-header-type)
			   (pcase place

			     (`parent
			      (and (elgantt--get-level-at-point)
				   (or (< (elgantt--get-level-at-point) level)
				       (= -1 (elgantt--get-level-at-point)))))

			     (`sibling
			      (and (elgantt--get-level-at-point)
				   (= (elgantt--get-level-at-point) level)))
			     ((or `parent-or-sibling
				  `sibling-or-parent)
			      (and (elgantt--get-level-at-point)
				   (or (<= (elgantt--get-level-at-point) level)
				       (= -1 (elgantt--get-level-at-point))))))
			 
			 (and (elgantt--get-level-at-point)
			      (or (= -1 (elgantt--get-level-at-point))
				  (and (elgantt--with-point-at (point-at-eol) (eobp))
				       (= level -1)
				       (/= (line-number-at-pos) start-line)))))))
      (cl-loop do (forward-line)
	       if (and (eobp)
		       (foundp))
	       return (point)
	       else if (foundp)
	       return
	       (if (eq 'outline elgantt-header-type)
		   (point)
		 (if (= -1 (elgantt--get-level-at-point))
		     (point)
		   (point-at-eol)))))))

(defun elgantt--insert-new-header-line (header &optional after-current-line)
  "Inserts a new header."
  (let ((inhibit-read-only t))
    (if after-current-line
	(goto-char (point-at-eol))
      (goto-char (point-max)))
    (insert "\n"
	    (substring 
	     (concat header (make-string elgantt-header-column-offset ? ))
	     0 elgantt-header-column-offset))
    (cl-loop for year in elgantt--date-range
	     do (if (elgantt--leap-year-p year)
		    (insert elgantt-leap-year-blank-line)
		  (insert elgantt-normal-year-blank-line)))))

(defun elgantt--insert-year (year &optional append)
  "For each line in the calendar, insert the appropriate
lines to display YEAR. If APPEND is t, then add the years
to the end of the calendar. (This should be calculated automatically, 
				  but currently it is not.)"
  (goto-char (point-min))
  (if append
      (end-of-line)
    (move-to-column elgantt-header-column-offset))
  (elgantt--draw-month-line year)
  (forward-line)
  (if append
      (end-of-line)
    (move-to-column elgantt-header-column-offset))
  (elgantt--draw-number-line year)
  (cl-loop until (progn (end-of-line)
			(eobp))
	   do (progn (forward-line)
		     (if append
			 (end-of-line)
		       (move-to-column elgantt-header-column-offset))
		     (elgantt--draw-blank-line year))))

(defun elgantt--add-year (year)
  "Check to see if YEAR has already been displayed in the calendar.
If so, do nothing. If not, insert that year for all calendar lines
and all header lines in the calendar, and push the year onto 
`elgantt--date-range' so that any new entries will contain the 
proper number of years."
  (when (not (memq year elgantt--date-range))
    (cond ((not elgantt--date-range)
	   (cl-pushnew year elgantt--date-range)
	   (elgantt--insert-year year))
	  ((< year (first elgantt--date-range))
	   (let ((dif (- (first elgantt--date-range) year)))
	     (setq year (first elgantt--date-range))
	     (dotimes (_ dif)
	       (setq year (1- year))
	       (cl-pushnew year elgantt--date-range)
	       (elgantt--insert-year year))))
	  ((> year (car (last elgantt--date-range)))
	   (let ((dif (- year (car (last elgantt--date-range)))))
	     (setq year (car (last elgantt--date-range)))
	     (dotimes (_ dif)
	       (setq year (1+ year))
	       (cl-pushnew year elgantt--date-range)
	       (elgantt--insert-year year t)))))
    (setq elgantt--date-range (sort elgantt--date-range #'<))))

(defun elgantt--insert-entry (props)
  "Inserts text properties of a cell at point, keeping any properties which
are already present. Updates the cell's display."
  (let ((inhibit-read-only t)
	(face (get-text-property (point) 'face))
	(date (plist-get props :elgantt-date)))
    ;; It is necessary to `mapc' over the date because date ranges
    ;; are stored as a list. If there is a date range the
    ;; properties are stored both at the first entry and the last entry
    (mapc (lambda (date)
	    ;; instead of passing only the header and level, pass all properties
	    ;;(elgantt--get-header-create (plist-get props :elgantt-header) (plist-get props :elgantt-level))
	    (elgantt--get-header-create props)
	    (when date
	      (elgantt--add-year (string-to-number (substring date 0 4)))
	      (elgantt--goto-date date)
	      (let ((old-props (plist-get (text-properties-at (point)) :elgantt)))
		(unless (cl-loop for prop in old-props
				 if (equal (plist-get prop :elgantt-org-id)
					   (plist-get props :elgantt-org-id))
				 do (cl-loop for property in (-slice props 0 nil 2)
					     do (plist-put prop property (plist-get props property)))
				 and return t)
		  (set-text-properties (point) (1+ (point)) `(:elgantt ,(append (list props)
										old-props))))
		(add-face-text-property (point) (1+ (point)) face))))
	  (or (-list date)
	      ;; If there is no date and `elgantt-insert-header-even-if-no-timestamp'
	      ;; is non-nil, then we still need to insert the heading
	      '(nil)))))

(defun elgantt--blank-header-line-p ()
  "Does this header contain any entries?"
  (save-excursion
    (goto-char (point-at-bol))
    (and (not (re-search-forward elgantt-cell-entry-re (point-at-eol) t))
	 (get-text-property (point-at-bol) 'elgantt-header))))

(defun elgantt--auto-shade-background (percent &optional base-color)
  "If the background is light, darken it. If the background is dark, lighten it. Return the
new color."
  (if (> 130 (elgantt--hexcolor-luminance (or base-color
					      (face-background 'default))))
      (color-lighten-name (or base-color (face-background 'default)) percent)
    (color-lighten-name (or base-color (face-background 'default))
			(* -1 percent))))

(defun elgantt--set-vertical-line-face ()
  (face-spec-set 'elgantt-vertical-line-face 
		 `((t (:background ,(elgantt--auto-shade-background 100)
				   :foreground ,(elgantt--auto-shade-background 100)
				   :height .1)))))

(defun elgantt--set-even-numbered-line-face ()
  "Set the value of the even numbered face dependinng on the default face."
  ;; This face is set here instead of in the `defface' because I often change my
  ;; theme and want this to be recalculated every time the calendar is drawn,
  ;; rather than calculated once when the package is loaded. 
  (face-spec-set 'elgantt-even-numbered-line-face
		 `((t (:inherit default
				:background
				,(elgantt--auto-shade-background elgantt-even-numbered-line-change))))))

(defun elgantt--clear-all-date-chars ()
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward elgantt-cell-entry-re nil t)
	(forward-char -1)
	(elgantt--change-char " ")))))

;; Updating overlays
(defun elgantt--update-display-all-cells ()
  "Clear overlays and run functions in `elgantt--display-rules'"
  (interactive)
  (remove-overlays (point-min) (point-max))
  (elgantt--remove-overarching-headers)
  (elgantt--clear-juxtapositions)
  (elgantt--clear-all-date-chars)
  (save-excursion
    (cl-loop for func in (append (list #'elgantt--display-rule-display-char) elgantt--display-rules)
	     do (progn (goto-char (point-min))
		       (while (next-single-property-change (point) :elgantt)
			 (goto-char (next-single-property-change (point) :elgantt))
			 (when (get-text-property (point) :elgantt)
			   (funcall func))))))
  (when elgantt-draw-overarching-headers
    (elgantt--draw-overarching-headers))
  (elgantt--highlight-current-day))

(defun elgantt--update-display-this-cell ()
  "Updates the overlays for the cell at point."
  (elgantt--display-rule-display-char)
  (cl-loop for func in elgantt--display-rules
	   do (funcall func)))

(defun elgantt--change-char (char &optional point)
  "Replace the character at point with CHAR, preserving all 
existing text properties."
  (let ((inhibit-read-only t))
    (save-excursion 
      (let ((props (text-properties-at (point))))
	(when point (goto-char point))
	(insert char)
	(delete-char 1)
	(forward-char -1)
	(set-text-properties (point) (1+ (point)) props)))))


;; Color functions

(defun elgantt--hexcolor-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
  ;; from https://www.emacswiki.org/emacs/HexColour
  (let* ((values (x-color-values color))
	 (r (car values))
	 (g (cadr values))
	 (b (caddr values)))
    (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))

(defun elgantt--color-rgb-to-hex (color)
  "Convert an RBG tuple '(R G B) to six digit hex string \"#RRGGBB\""
  (substring 
   (pcase-let ((`(,r ,g ,b) color))
     (color-rgb-to-hex r g b 2))
   0 7))

(defun elgantt--color-name-to-hex (color)
  "Convert named color to six digit hex color."
  (eval `(color-rgb-to-hex ,@(color-name-to-rgb color) 2)))

(defalias 'elgantt--color-name-to-rgb #'color-name-to-rgb)

(defun elgantt--color-hex-to-rgb (hex-color)
  "Convert hex color to RGB tuple."
  (let ((elements (list (string-to-number (substring hex-color 1 3) 16)
			(string-to-number (substring hex-color 3 5) 16)
			(string-to-number (substring hex-color 5 7) 16))))
    (cl-loop for element in elements
	     collect (if (= element 0)
			 0
		       (/ element 255.0)))))

(defun elgantt--color-to-hex (color)
  "Conver a color (name, RGB tuple, or hex) to hex."
  (pcase color
    ((and (pred stringp)
	  (pred (s-starts-with-p "#")))
     color)
    ((and `(,r ,g ,b)
	  (guard (numberp r))
	  (guard (numberp g))
	  (guard (numberp b)))
     (list r g b))
    ((pred stringp)
     (elgantt--color-name-to-hex color))))

(defun elgantt--color-to-rgb (color)
  "Convert a color name or hex color to RGB tuple."
  (pcase color
    ;; If it's hex...
    ((and (pred stringp)
	  (pred (s-starts-with-p "#")))
     (elgantt--color-hex-to-rgb color))
    ;; If it's a string (trust the user that the color
    ;; name is in `list-colors-display')...
    ((pred stringp)
     (elgantt--color-name-to-rgb color))
    ;; If it's already an RGB tuple...
    ((and `(,r ,g ,b)
	  (guard (numberp r))
	  (guard (numberp g))
	  (guard (numberp b)))
     color)
    ;; Otherwise...
    (_ (error (concat "Color type must be hex, e.g., \"#ffccaa\", "
		      "or color name, e.g., \"red\", or an RGB tuple, "
		      "e.g., '(1.0 .5 0)")))))

(defun elgantt--clear-juxtapositions (&optional start end property)
  "Clear all juxtaposed text, i.e., text with a 'juxtapose text property.
If PROPERTY is supplied, clear juxtapositions only if they also have PROPERTY."
  (let ((inhibit-read-only t))
    (cl-loop for points being the intervals of (current-buffer) property (or property 'juxtapose)
	     from (or start (point-min))
	     to (or end (point-max))
	     if (get-text-property (car points) (or property
						    'juxtapose))
	     do (remove-text-properties (car points) (cdr points) `(display t juxtapose t ,property t)))))

(defun elgantt--insert-juxtaposition (char &optional replace hide-buffer-char property face)
  "Draw CHAR on top of the existing text a point. If REPLACE is non-nil, replace
any previous juxaposed character. If HIDE-BUFFER-CHAR is non-nil, hide the character
in the buffer at point. If PROPERTY, add that text property. See `elgantt--clear-juxtapositions'."
  (let* ((char (if (characterp char) (char-to-string char) char))
	 (buffer-char (unless hide-buffer-char
			(buffer-substring-no-properties (point) (1+ (point)))))
	 (old-juxtaposition (unless replace
			      (when (get-text-property (point) 'juxtapose)
				(get-text-property (point) 'display))))
	 (new-string (compose-string (concat char
					     buffer-char
					     old-juxtaposition))))
    (when face
      (put-text-property 0 (length new-string) 'face face new-string))
    (put-text-property (point) (1+ (point)) 'display new-string)
    (put-text-property (point) (1+ (point)) 'juxtapose t)
    (when property 
      (put-text-property (point) (1+ (point)) property t))))

(defun elgantt--remove-overarching-headers ()
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "["
					elgantt-draw--horizontal-line
					elgantt-draw--top-left
					elgantt-draw--top-right
					"]")
				nil t)
	(backward-char)
	(elgantt--change-char " ")))))

(defun elgantt--empty-line-p ()
  (elgantt--with-point-at (point-at-bol)
    (and (> (line-number-at-pos) 2)
	 (not (get-text-property (point) 'elgantt-header)))))

(defun elgantt--draw-overarching-headers ()
  (elgantt--scroll-to-beginning)
  (let ((inhibit-read-only t))
    (save-excursion 
      (goto-char (point-min))
      (forward-line 1)
      (cl-loop until (progn (save-excursion (end-of-line)
					    (eobp)))
	       do (forward-line 1)
	       if (elgantt--blank-header-line-p)
	       do (let ((first-date nil)
			(last-date nil)
			(point (point))
			(current-level (elgantt--get-level-at-point)))
		    (save-excursion
		      (forward-line 1)
		      ;; HACK
		      (cl-loop until (or (elgantt--empty-line-p)
					 (<= (elgantt--get-level-at-point)
					     current-level)
					 (eobp))
			       do (progn
				    (save-excursion
				      (goto-char (point-at-bol))
				      (while (re-search-forward elgantt-cell-entry-re (point-at-eol) t)
					(forward-char -1)
					(setq first-date
					      (first
					       (elgantt--sort-dates
						(-flatten (list first-date
								(elgantt-get-prop-at-point :elgantt-scheduled)
								(elgantt-get-date-at-point))))))
					(forward-char 1)))
				    (save-excursion
				      (goto-char (point-at-eol))
				      (while (re-search-backward elgantt-cell-entry-re (point-at-bol) t)
					(setq last-date
					      (car
					       (last
						(elgantt--sort-dates
						 (-flatten (list last-date
								 (elgantt-get-prop-at-point :elgantt-scheduled)
								 (elgantt-get-date-at-point)))))))))
				    (forward-line 1))
			       finally (when (and first-date
						  last-date
						  (not (string= first-date last-date)))
					 (goto-char point)
					 (cl-loop with end = (1- (elgantt--goto-date last-date))
						  with start = (elgantt--goto-date first-date)
						  for x from start to end
						  if (= x start)
						  do (progn (when (not (looking-at "|"))
							      (elgantt--change-char elgantt-draw--top-left))
							    (forward-char 1))
						  else do (progn (when (not (looking-at "|"))
								   (elgantt--change-char elgantt-draw--horizontal-line))
								 (forward-char 1))
						  finally (elgantt--change-char  elgantt-draw--top-right))))))))))

;; Gradients
(defun elgantt--get-color-midpoint (color1 color2)
  "Take two colors and return their average as an RGB tuple."
  (let ((color1 (elgantt--color-to-rgb color1))
	(color2 (elgantt--color-to-rgb color2)))
    (-zip-with (lambda (c1 c2)
		 (/ (+ c1 c2) 2))
	       color1 color2)))

(defun elgantt--draw-progress-bar (start-color end-color start end divider-or-percent)
  "Draws an overlay from the points START to END starting with START-COLOR
and ending with END-COLOR, with the transition occurring at DIVIDER-OR-PERCENT.

DIVIDER-OR-PERCENT is either a percent (represented as a float) or a point (represented as
an integer)."
  (let ((start-color (elgantt--color-name-to-hex start-color))
	(end-color (elgantt--color-name-to-hex end-color))
	(divider (pcase divider-or-percent
		   ((pred floatp)
		    (+ start
		       (round (* (- end start) divider-or-percent))))
		   ((pred integerp)
		    divider-or-percent)
		   (_ (error "Invalid type: DIVIDER-OR-PERCENT.")))))    
    (save-excursion
      (goto-char start)
      (cl-loop for x from start to end
	       do (goto-char x)
	       (remove-overlays (point) (1+ (point)))
	       (unless (elgantt--on-vertical-line-p)
		 (elgantt--create-overlay (point) (1+ (point))
					  `(face ,(if (<= (point) divider)
						      `(:background ,start-color)
						    `(:background ,end-color)))))
	       (forward-char)))))

(defun elgantt--create-gradient (start-color end-color steps &optional midpoint)
  "Returns a list gradient from START-COLOR to END-COLOR with STEPS number of steps.
Much like `color-gradient' except that you can specify MIDPOINT, which will set the 
location of the midpoint between the color transition. The colors can be color names, 
i.e., a string, or a hex color, i.e., \"#xxxxxx\""
  (let* ((start-color (elgantt--color-to-rgb start-color))
	 (end-color (elgantt--color-to-rgb end-color))
	 (color-gradient (if midpoint
			     (let ((mid-color (elgantt--get-color-midpoint start-color
									   end-color)))
			       (append (color-gradient start-color mid-color midpoint)
				       (color-gradient mid-color end-color (- steps midpoint))))
			   (color-gradient start-color end-color steps))))
    color-gradient))

(defun elgantt--draw-gradient (start-color end-color start end &optional midpoint props)
  "Draws an overlay gradient from START-COLOR to END-COLOR from points START to END.
Puts the midpoint of the gradient at MIDPOINT. Adds PROPS to the overlay."
  (let ((color-gradient (elgantt--create-gradient start-color end-color
						  (1+ (- end start)) midpoint)))
    (save-excursion
      (goto-char start)
      (mapc (lambda (color)
	      (if-let ((overlay (--first (--> (overlay-properties it)
	      				      (plist-get it :elgantt-user-overlay))
	      				 (overlays-at (point)))))
	      	  (overlay-put overlay 'face `(:background ,(elgantt--color-rgb-to-hex
	      						     (elgantt--get-color-midpoint (background-color-at-point)
											  color))))
		(unless (elgantt--on-vertical-line-p)
		  (elgantt--create-overlay (point)
					   (1+ (point))
					   (-flatten-n 1 (append 
							  `(face ((:background ,(elgantt--color-rgb-to-hex color))))
							  props)))))
	      (forward-char))
	    color-gradient)))) 

(defun elgantt--change-brightness-of-background-at-point (point change &optional props)
  "if there is a background font lock color, this will change its brightness"
  (let ((overlay (make-overlay point (1+ point))))
    (overlay-put overlay 'face `(:background ,(elgantt--auto-shade-background change)))
    (when props
      (let ((i 0)
	    (len (length props)))
	(while (< i len)
	  (overlay-put overlay
		       (nth i props) (nth (setq i (1+ i)) props))
	  (setq i (1+ i)))))
    overlay))

(defun elgantt--draw-line (start-point end-point &optional property face)
  "Draw a line from START-POINT to END-POINT."
  ;; The algorithm is that it draws the line half way, draws the
  ;; line up or down to the target, and then horizontal to the target.
  ;;
  ;; Lines are drawn by using `elgantt--insert-juxtaposition', which
  ;; uses the display text property to juxtapose text. This way,
  ;; the text under the line is not lost. All points with a line
  ;; will have the text property 'juxtapose set to t. They can be cleared
  ;; with `elgantt--clear-juxtapositions'. PROPERTY is an optional
  ;; argument to add a unique property to the juxtaposition.
  (let* ((inhibit-read-only t)
	 (start (cons (progn (goto-char start-point)
			     (current-column))
		      (line-number-at-pos)))
	 (end (cons (progn (goto-char end-point)
			   (current-column))
		    (line-number-at-pos)))
	 (sorted (cond ((<= (car start) (car end))
			(list start end))
		       ((> (car start) (car end))
			(list end start))))
	 (x-distance (- (caadr sorted)
			(caar sorted)))
	 (y-distance (- (cdadr sorted)
			(cdar sorted))))
    (goto-char (point-min))
    (forward-line (1- (cdar sorted)))
    (move-to-column (caar sorted))
    (if (= x-distance 0)
	(cl-loop for y from 1 to (1+ (abs y-distance))
		 do (cond ((= y 1)
			   (if (> 0 y-distance)
			       (progn 
				 (elgantt--insert-juxtaposition elgantt-draw--top-half-vertical nil nil property face)
				 (elgantt--forward-line (if (> 0 y-distance) -1 1)))
			     (elgantt--insert-juxtaposition elgantt-draw--bottom-half-vertical nil nil property face)
			     (elgantt--forward-line (if (> 0 y-distance) -1 1))))
			  ((= y (1+ (abs y-distance)))
			   (if (> 0 y-distance)
			       (progn 
				 (elgantt--insert-juxtaposition elgantt-draw--bottom-half-vertical nil nil property face)
				 (elgantt--forward-line (if (> 0 y-distance) -1 1)))
			     (elgantt--insert-juxtaposition elgantt-draw--top-half-vertical nil nil property face)
			     (elgantt--forward-line (if (> 0 y-distance) -1 1))))
			  (t (elgantt--insert-juxtaposition elgantt-draw--vertical-line nil nil property face)
			     (elgantt--forward-line (if (> 0 y-distance) -1 1)))))
      (cl-loop for x from 1 to (abs (/ x-distance 2))
	       do (progn (unless (= x 1)
			   (elgantt--insert-juxtaposition elgantt-draw--horizontal-line nil nil property face))
			 (forward-char (if (> 0 x-distance) -1 1))))
      (cl-loop for y from 1 to (abs y-distance)
	       do (progn (if (= y 1)
			     (if (= 0 x-distance)
				 (elgantt--insert-juxtaposition elgantt-draw--vertical-line nil nil property face)
			       (elgantt--insert-juxtaposition (if (> 0 y-distance)
								  elgantt-draw--bottom-right
								elgantt-draw--top-right)
							      nil nil property face))
			   (elgantt--insert-juxtaposition elgantt-draw--vertical-line nil nil property face))
			 (elgantt--forward-line (if (> 0 y-distance) -1 1))))
      (cl-loop for x from 1 to (abs (+ (/ x-distance 2)
				       (% x-distance 2)))
	       do (progn (if (= x 1)
			     (elgantt--insert-juxtaposition (cond ((> 0 y-distance)
								   elgantt-draw--top-left)
								  ((= 0 y-distance)
								   elgantt-draw--horizontal-line)
								  (t elgantt-draw--bottom-left))
							    nil nil property face)
			   (elgantt--insert-juxtaposition elgantt-draw--horizontal-line nil nil property face))
			 (forward-char (if (> 0 x-distance) -1 1)))))))

(defun elgantt--connect-cells (property value &optional text-prop face)
  "Draw a line from the first cell that has a matching PROPERTY and VALUE
through all other matching cells."
  (save-excursion
    (when-let* ((inhibit-read-only t)
		(previous-point (progn (goto-char (point-min))
				       (car (elgantt--next-match property value))))
		(next-point (progn (goto-char previous-point)
				   (elgantt--next-match property value))))
      (while next-point
	(mapc (lambda (point)
		(--map (elgantt--draw-line point it text-prop face)
		       next-point))
	      (-list previous-point))
	(setq previous-point next-point)
	(setq next-point (elgantt--next-match property value))))))

(defun elgantt--vertical-highlight ()
  "Draws an vertical highlight at point."
  (remove-overlays (point-min) (point-max) 'elgantt-vertical-highlight t)
  (cl-loop with line-length = (- (point-at-eol) (point-at-bol))
	   with point = (cl-loop with point = (point)
				 until (< point line-length)
				 do (setq point (- point line-length 1))
				 finally return point)
	   until (> point (point-max))
	   do (progn (push (make-overlay point (1+ point)) elgantt--vertical-bar-overlay-list)
		     (overlay-put (car elgantt--vertical-bar-overlay-list) 'priority 999999)
		     (overlay-put (car elgantt--vertical-bar-overlay-list) 'elgantt-vertical-highlight t)
		     (overlay-put (car elgantt--vertical-bar-overlay-list) 'face `(:background ,(elgantt--auto-shade-background
												 15
												 (save-excursion
												   (goto-char point)
												   (background-color-at-point)))))
		     (setq point (+ point line-length 1)))))

(defun elgantt--highlight-current-day ()
  "Draws an overlay highlighting the current date, if it is visible."
  ;; TODO: The check for whether to run this (i.e., whether the current
  ;; date is actually in the calendar is in `elgantt-open', but it
  ;; should be here instead. 
  (when (member (string-to-number (format-time-string "%Y"))
		elgantt--date-range)
    (save-excursion 
      (goto-char (point-min))
      (forward-line 1)
      (let ((date-line (elgantt--convert-date-to-column-number (format-time-string "%Y-%m-%d")))
	    (x 1)
	    (total-lines (count-lines (point-min) (point-max))))
	(while (< x total-lines)
	  (move-beginning-of-line 1)
	  (forward-char date-line)
	  (elgantt--change-brightness-of-background-at-point (point) +30 '(priority 99999999))
	  (forward-line)
	  (setq x (1+ x))))
      (goto-char (point-min)))))

(defun elgantt--delete-cell-contents-at-point ()
  "Remove the character and properties at point, but keep the 
horizontal line coloring." 
  (let ((inhibit-read-only t))
    (insert " ") 
    (delete-char 1)
    (backward-char)
    (add-face-text-property (point)
    			    (1+ (point))
    			    (if (= (% (line-number-at-pos) 2) 0)
    				'elgantt-even-numbered-line-face
    			      'elgantt-odd-numbered-line-face))))

(defun elgantt-update-this-cell (&optional date)
  "Gets data for a specific cell by looking for any headings
which occur on DATE. If DATE is nil, use `elgantt-get-date-at-point'."
  (when (elgantt--on-vertical-line-p)
    (error "Error in elgantt-update-this-cell: Not on a calendar cell."))
  (save-excursion
    (let ((dates (sort
		  (-distinct
		   (-flatten
		    (append
		     (-list (or date
				(elgantt-get-date-at-point)))
		     (elgantt-get-prop-at-point :elgantt-date))))
		  #'string<)))
      (elgantt--delete-cell-contents-at-point)
      (mapc #'elgantt--insert-entry
	    (-non-nil (cl-loop for date in dates
			       append (org-ql-select elgantt-agenda-files
					`(and (ts :on ,date)
					      (not (tags ,(when elgantt-skip-archives
							    org-archive-tag))))
					:action #'elgantt--parser)))))))

(defun elgantt--post-command-hook ()
  "Runs `post-command-hook' functions created with `elgantt-create-display-rule'."
  (cl-loop for command in elgantt--post-command-hooks
	   do (funcall command)))

(cl-defmacro elgantt-create-display-rule (name &key docstring args parser body
					       append disable post-command-hook)
  "NAME is a symbol used to name new functions that are created. 

ARGS is a list of the text properties that will be used by the function. 
Any poperties supplied here will be automatically fetched from 
the cell at point and let-bound for use within BODY. ARGS should consist of only
those properties that are stored in a calendar cell. If you need to use 
data that is not contained in the stored properties, you can add a PARSER. 

PARSER is is used to add information to cells when the
calendar is generated. It must be an alist in form of ((property-name . body)).
You may specify a property-name which begins with a colon, or not. If none is 
provided, a colon will be added automatically. Body is the body of a function 
that is called when the point is at the first point of each org heading. 
Its return value will be assigned to the property-name for each cell, and 
stored as a text property.  

DOCSTRING is the docstring of the newly-defined function.

BODY is the body of the display function. DISPLAY-BODY should generally do one
of the following: Setting an overlay, setting text-properties, changing the face, etc.
The return value of BODY is ignored and all changes must be made through side-effect. 
- An overlay can be set with `elgantt--create-overlay'.
- The character of a cell can be changed by using `elgantt--change-char'.
- The gradient of a cell, or cells, can be changed with `elgantt--draw-gradient'.
- A progress bar can be drawn with `elgantt--draw-two-color-block'. 

After the display function is created, it is pushed onto `elgantt--display-functions'.
These functions are run for each cell at point, from the start of the list to the 
end. 

If APPEND is non-nil, then the function will be appended to the end of
`elgantt--display-functions' rather than pushed to the front.

If POST-COMMAND-HOOK is non-nil, then the display function will be added as a post
command hook. If this option is used for an overlay, make sure to give the overlay
a unique property value so that it can be cleared. For example adding the property
'(:my-customization t) to the overlay properties will allow you to clear and update
that overlay without interfering with other overlays in the buffer. If POST-COMMAND-HOOK
is nil, then the hook will be removed if it exists.

If DISABLE is non-nil, then the rule will be removed from the 
`elgantt--display-rules', any parsing functions created by the rule will
be removed, and any hook will be removed."

  (declare (indent defun))
  (let ((display-func-name (intern (concat "elgantt--display-rule-" (symbol-name name)))))
    `(progn
       (when ',parser
	 (cl-loop for (prop . val) in (-list ',parser)
		  do (setf (alist-get (if (s-starts-with-p ":" (symbol-name prop))
					  prop
					(intern (concat ":" (symbol-name prop))))
				      elgantt--parsing-functions)
			   `(lambda () ,@val))))
       (if (or ',parser ',args)
	   (progn
	     (defun ,display-func-name ()
	       ,docstring
	       (mapc
		(lambda (arg-list)
		  (-let ((,(append (cl-loop for arg in args
					    collect (elgantt--add-remove-prop-colon arg t))
				   (cl-loop for (prop . val) in parser
					    collect (elgantt--add-remove-prop-colon prop t)))
			  arg-list))
		    ,@body))
		(or (elgantt--zip
		     (mapcar #'elgantt-get-prop-at-point
			     (append ',(cl-loop for arg in args
						collect (elgantt--add-remove-prop-colon arg))
				     ',(cl-loop for (prop . val) in parser
						collect (elgantt--add-remove-prop-colon prop)))))
		    ;; If the preceding code returns `nil', then the `mapc' function, above,
		    ;; will not run. Since `elgantt-get-prop-at-point' will return nil
		    ;; if on an empty cell, it creates a problem if the user wants to run
		    ;; the command in an empty cell. 
		    ;; Thus, if `elgantt--zip' returns nil, this will create a list of nils to
		    ;; be assigned to the argument list, since nil is not `eq' to (nil),
		    ;; `mapc' will accept the list and run.
		    ;; NOTE: I am not sure if simply returning a single (nil) would suffice.
		    (make-list (if (> 0 (length (elgantt-get-prop-at-point))) 
				   (length (elgantt-get-prop-at-point)) 1)
			       (make-list (+ (length ',parser) (length ',args)) nil))))))
	 (defun ,display-func-name () ,docstring ,@body))
       (if ',append
	   (progn
	     (setq elgantt--display-rules (remq ',display-func-name elgantt--display-rules))
	     (add-to-list 'elgantt--display-rules #',display-func-name t))
	 (setq elgantt--display-rules (remq ',display-func-name elgantt--display-rules))
	 (cl-pushnew #',display-func-name elgantt--display-rules))
       ;; (if ',mode-hook
       ;; 	   (add-hook 'elgantt-mode-hook ',display-func-name)
       ;; 	 (remove-hook 'elgantt-mode-hook ',display-func-name))
       (if ',post-command-hook
	   (progn
	     (setq elgantt--display-rules (remq ',display-func-name elgantt--display-rules))
	     (cl-pushnew ',display-func-name elgantt--post-command-hooks))
	 (setq elgantt--post-command-hooks (remq ',display-func-name elgantt--post-command-hooks)))
       ;;(setq elgantt--display-rules (remq ',display-func-name elgantt--display-rules)))
       (when ',disable
	 (cl-loop for (name . func) in ',parser
		  do (setq elgantt--parsing-functions
			   (assq-delete-all name elgantt--parsing-functions)))
	 (setq elgantt--post-command-hooks (remq ',display-func-name elgantt--post-command-hooks))
	 (setq elgantt--display-rules (remq ',display-func-name elgantt--display-rules))))))

(elgantt-create-display-rule display-char
  :docstring "Display the appropriate character in each cell."
  :args (elgantt-date-type elgantt-timestamp-range elgantt-timestamp-range-ia)
  ;; This is disabled because it is treated specially and should not be part of `elgantt--display-rules'
  :disable t
  :body ((when (elgantt-get-prop-at-point)
	   (if (> (length (elgantt-get-prop-at-point)) 1)
	       (elgantt--change-char elgantt-multiple-entry-character)
	     (elgantt--change-char (pcase elgantt-date-type 
				     (`deadline elgantt-deadline-character)
				     (`timestamp elgantt-active-timestamp-character)
				     (`timestamp-range
				      (if (string= (elgantt-get-date-at-point) (car elgantt-timestamp-range))
					  elgantt-timestamp-range-start-character
					elgantt-timestamp-range-end-character))
				     (`timestamp-range-ia
				      (if (string= (elgantt-get-date-at-point) (car elgantt-timestamp-range-ia))
					  elgantt-timestamp-range-ia-start-character
					elgantt-timestamp-range-ia-end-character))
				     (`timestamp-ia elgantt-inactive-timestamp-character)
				     (`scheduled elgantt-scheduled-character)
				     ;; There shouldn't be anything left over
				     (_ (error "Unrecognized date type."))))))))

(cl-defmacro elgantt-create-action (name &key docstring parser args body binding)
  "NAME is a symbol used to name new functions that are created. 

ARGS is a list of the text properties that will be used by the function. 
Any poperties supplied here will be automatically fetched from 
the cell at point and let-bound for use within BODY. ARGS should consist of only
those properties that are stored in a calendar cell. If you need to use 
data that is not contained, you can add a PARSER. 

PARSER is is used to add information to cells when the
calendar is generated. It must be an alist in form of ((property-name . body)).
You may specify a property-name which begins with a colon, or not. If none is 
provided, a colon will be added automatically. Body is the body of a function 
that is called when the point is at the first point of each org heading. 
Its return value will be assigned to the property-name for each cell, and 
stored as a text property. 

DOCSTRING is the docstring of the newly-defined function.

BODY is the body of a function which performs some action on the underlying
cell, the underlying org file, or something else. If you need to perform some action
on the org file, or get data from it, use `elgantt-with-point-at-orig-entry'. Otherwise,
action can do anything you'd like. 

BINDING the key binding for the newly defined ACTION. It allows any
string accepted by `kbd'."
  (declare (indent defun))
  (let ((action-func-name (intern (concat "elgantt--action-rule-" (symbol-name name)))))
    `(progn
       (when ',parser
	 (cl-loop for (prop . val) in (-list ',parser)
		  do (setf (alist-get (if (s-starts-with-p ":" (symbol-name prop))
					  prop
					(intern (concat ":" (symbol-name prop))))
				      elgantt--parsing-functions)
			   `(lambda () ,@val))))
       (if (or ',parser ',args)
	   (progn
	     (defun ,action-func-name ()
	       ,docstring
	       (interactive)
	       (mapc
		(lambda (arg-list)
		  (-let ((,(append (cl-loop for arg in args
					    collect (elgantt--add-remove-prop-colon arg t))
				   (cl-loop for (prop . val) in parser
					    collect (elgantt--add-remove-prop-colon prop t)))
			  arg-list))
		    ,@body))
		(or (elgantt--zip
		     (mapcar #'elgantt-get-prop-at-point
			     (append ',(cl-loop for arg in args
						collect (elgantt--add-remove-prop-colon arg))
				     ',(cl-loop for (prop . val) in parser
						collect (elgantt--add-remove-prop-colon prop)))))
		    (make-list (+ (length ',parser) (length ',args)) nil)))))
	 (defun ,action-func-name () ,docstring (interactive) ,@body))
       (when ',binding 
	 (define-key elgantt-mode-map (kbd ,binding) #',action-func-name)))))

(defun elgantt--draw-even-odd-background ()
  "Set the background for even and odd lines."
  (save-excursion
    (goto-char (point-min))
    (cl-loop do (progn (add-face-text-property (point-at-bol)
					       (point-at-eol)
					       (if (= (% (line-number-at-pos) 2) 0)
						   'elgantt-even-numbered-line-face
						 'elgantt-odd-numbered-line-face)
					       'append)
		       (forward-line))
	     until (eobp))))

;;; Interactive functions

(defun elgantt--open-org-agenda-at-date ()
  "Opens `org-agenda' for the date at point."
  (interactive)
  (let ((date (ts-format "%Y-%m-%d" (ts-parse (elgantt-get-date-at-point)))))
    (org-agenda-list nil date 'day))
  (other-window 1))

(defun elgantt-navigate-to-org-file ()
  "Navigate to a location in an org file for the cell at point."
  (interactive)
  (if-let* ((props (elgantt--select-entry))
	    (buffer (plist-get props :elgantt-org-buffer))
	    (marker (plist-get props :elgantt-marker)))
      (progn 
	(switch-to-buffer-other-window buffer)
	(org-goto-marker-or-bmk marker)
	(outline-show-entry)
	(outline-show-children)
	(beginning-of-line))
    (message "Cannot navigate to org file: no data at point.")))

(defun elgantt-scroll-forward ()
  "Interactive function to scroll forward by one month."
  (interactive)
  (elgantt-scroll 'forward))

(defun elgantt-scroll-backward ()
  "Interactive function to scroll forward by one month."
  (interactive)
  (elgantt-scroll 'backward))

(defun elgantt--move-forward ()
  "Moves to the next entry on the line."
  (interactive)
  (when (<= (line-number-at-pos) 2)
    (goto-char (point-min))
    (forward-line 3))
  (when (<= (current-column) elgantt-header-column-offset)
    (forward-char elgantt-header-column-offset))
  (when-let ((point (save-excursion 
		      (forward-char 1)
		      (re-search-forward elgantt-cell-entry-re
					 (point-at-eol)
					 t))))
    (goto-char (1- point))))

(defun elgantt--move-backward ()
  "Move to the previous entry on the line."
  (interactive)
  (re-search-backward elgantt-cell-entry-re
		      (point-at-bol)
		      t))

(defun elgantt--forward-char (&optional n)
  "Move forward N chars, skipping vertical lines."
  (interactive)
  (forward-char n)
  (when (looking-at "|")
    (forward-char n)))

(defun elgantt--backward-char (&optional n)
  "Move backward N char, skipping vertical lines."
  (interactive)
  (elgantt--forward-char (or n -1)))

(defun elgantt--move-up ()
  "Move the cursor up with `elgantt--move-vertically'."
  (interactive)
  (unless (<= (line-number-at-pos) 3)
    (elgantt--move-vertically 'up)))

(defun elgantt--move-down ()
  "Move the cursor down with `elgantt--move-vertically'."
  (interactive)
  (unless (= (line-number-at-pos)
	     (count-lines (point-min) (point-max)))
    (elgantt--move-vertically 'down)))

(defun elgantt--fontify-vertical-bars ()
  (let ((inhibit-read-only t))
    (save-excursion 
      (goto-char (point-min))
      (while (re-search-forward "|" nil t)
	(forward-char -1)
	(put-text-property (point) (1+ (point))
			   'face 'elgantt-vertical-line-face)))))

;;; Major mode
(defvar elgantt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r")   #'elgantt--reload)
    (define-key map (kbd "C-r") #'elgantt-open)
    (define-key map (kbd "SPC") #'elgantt-navigate-to-org-file)
    (define-key map (kbd "p")   #'elgantt--move-up)
    (define-key map (kbd "c")   #'elgantt-scroll-to-current-month)
    (define-key map (kbd "n")   #'elgantt--move-down)
    (define-key map (kbd "f")   #'elgantt--move-forward)
    (define-key map (kbd "b")   #'elgantt--move-backward)
    ;; (define-key map (kbd "<tab>") #'elgantt--toggle-fold)
    (define-key map (kbd "<right>") #'elgantt--forward-char)
    (define-key map (kbd "<left>") #'elgantt--backward-char)
    (define-key map (kbd "C-f")   #'elgantt--forward-char)
    (define-key map (kbd "C-b")   #'elgantt--backward-char)
    (define-key map (kbd "F")   #'elgantt-scroll-forward)
    (define-key map (kbd "B")   #'elgantt-scroll-backward)
    (define-key map (kbd "R")   #'elgantt--update-display-all-cells)
    (define-key map (kbd "RET") #'elgantt--open-org-agenda-at-date)
    (define-key map (kbd "M-f") #'elgantt--shift-date-forward)
    (define-key map (kbd "M-b") #'elgantt--shift-date-backward)
    map))

(defun elgantt--reload ()
  (interactive)
  (let ((point (point))
	(inhibit-read-only t))
    (erase-buffer)
    (elgantt--reset-org-ql-cache)
    (elgantt--set-even-numbered-line-face)
    (elgantt--set-vertical-line-face)
    ;;(elgantt--set-vertical-highlight-face)
    (setq elgantt--date-range nil)
    (setq elgantt--hidden-overlays nil)
    (insert (make-string elgantt-header-column-offset ? )
	    "\n"
	    (make-string elgantt-header-column-offset ? ))
    (elgantt--iterate)
    (elgantt--draw-even-odd-background)
    (elgantt--update-display-all-cells)
    (toggle-truncate-lines 1)
    (setq header-line-format elgantt-header-line-format)
    (when elgantt-draw-overarching-headers
      (elgantt--draw-overarching-headers))
    (when elgantt-scroll-to-current-month-at-startup
      (elgantt-scroll-to-current-month))
    (elgantt--highlight-current-day)
    (goto-char point)))

;; Major mode
(define-derived-mode elgantt-mode special-mode
  "El Gantt"
  "Horizontal calendar interface for orgmode. \{keymap}"
  (let ((point (point))
	(inhibit-read-only t))
    (erase-buffer)
    (elgantt--set-vertical-line-face)
    (put-text-property 0 1 'face 'elgantt-vertical-line-face elgantt-vertical-line-char)
    (elgantt--add-vertical-line-props (elgantt-leap-year-month-line
				       elgantt-leap-year-date-line
				       elgantt-leap-year-blank-line
				       elgantt-normal-year-month-line
				       elgantt-normal-year-date-line
				       elgantt-normal-year-blank-line))
    (elgantt--reset-org-ql-cache)
    (elgantt--set-even-numbered-line-face)
    (setq elgantt--date-range nil)
    (setq elgantt--hidden-overlays nil)
    (insert (make-string elgantt-header-column-offset ? )
	    "\n"
	    (make-string elgantt-header-column-offset ? ))
    (elgantt--iterate)
    (elgantt--draw-even-odd-background)
    (elgantt--update-display-all-cells)
    (toggle-truncate-lines 1)
    (setq header-line-format elgantt-header-line-format)
    (goto-char point)
    (when elgantt-scroll-to-current-month-at-startup
      (elgantt-scroll-to-current-month)))
  (add-hook 'post-command-hook #'elgantt--post-command-hook nil t)
  (add-hook 'post-command-hook #'elgantt--vertical-highlight nil t))

;;;###autoload 
(defun elgantt-open ()
  "Open gantt calendar."
  (interactive)
  (switch-to-buffer "*El Gantt Calendar*")
  (elgantt-mode))

;;;; Footer

(provide 'elgantt)

;;; elgantt.el ends here




