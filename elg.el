;;; elg.el --- Generate integrated text-based Gantt Charts from Orgmode files  -*- lexical-binding: t; -*-

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

;; (require 'elg)

;;;; Usage

;; Run this command: 

;; `elg-open': Open a Gantt Calendar from your agenda files

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
(require 'org-ql)
(require 's)
(require 'dash)
(require 'ts)
(require 'ov)

;;;; Customization

(defgroup elg-org nil
  "Options about gantt-org."
  :tag "Elgantt"
  :group 'org
  :group 'elg)

(defcustom elg-timestamps-to-display '(deadline timestamp timestamp-ia scheduled timestamp-range timestamp-range-ia)
  "List of the types of timestamps to display in the calendar. Order matters. If an entry has two types of 
  timestamps, then the first found will be used to determine where it appears in the calendar.
  The following types are accepted: deadline timestamp timestamp-ia scheduled timestamp-range timestamp-range-ia.")

(defcustom elg-deadline-character "▲"
  "Character used for deadlines in the calendar.")

(defcustom elg-active-timestamp-character "●"
  "Character used for active timestamps in the calendar")

(defcustom elg-inactive-timestamp-character "⊚"
  "Character used for inactive timestamps in the calendar")

(defcustom elg-scheduled-character "⬟"
  "Character used for active timestamps in the calendar")

(defcustom elg-multiple-entry-character "☰"
  "Character used for cells which have multiple entries")

(defcustom elg-timestamp-range-start-character "▶"
  "Character shown at the beginning of a timerange.")

(defcustom elg-timestamp-range-end-character "◀"
  "Character shown at the end of a timerange.")

(defcustom elg-cal-timestamp-range-ia-start-character "▷"
  "Character shown at the beginning of a timerange.")

(defcustom elg-timestamp-range-ia-end-character "◁"
  "Character shown at the end of a timerange.")

(defcustom elg-agenda-files (org-agenda-files)
  "Default: `org-agenda-files'.")

(defcustom elg-skip-archives t
  "Skip archived entries if non-nil.")

(defcustom elg-start-date (format-time-string "%Y-%m-%d")
  "Beginning date for the calendar as a string YYYY-MM-DD. 
Nothing before this date will be displayed. Defaults to the current month.")

(defcustom elg-header-column-offset 20
  "Width of the header column")

(defcustom elg-header-type 'root
  "Define how to gather the headers. Values are root, category, hashtag, 
or a function that returns the desired header.")

(defcustom elg-header-line-format
  '(:eval
    (let ((string (s-pad-right (window-total-width) " "
			       (concat (when (elg-get-date-at-point)
					 (s-pad-right 30 " " (elg-get-date-at-point)))
				       (when (elg-get-header-at-point)
					 (s-pad-right 30 " " (elg-get-header-at-point)))
				       (when-let ((headlines (elg-get-prop-at-point :elg-headline)))
					 (if (> (length headlines) 1)
					     (cl-loop for headline in headlines
						      concat (concat headline " / "))
					   (concat (car headlines))))))))
      (put-text-property 0 (length string) 'face 'elg-header-line-face string)
      string))
  "See `header-line-format'.")

(defcustom elg-exclusions nil
  "Exclude headers in this list from display in the calendar.")

;;;; Faces

(defface elg-vertical-line-face
  '((t :background "white" :foreground "white" :height .1))
  "Vertical line face")

(defface elg-dependent-highlight-face
  '((t (:background "white" :foreground "white")))
  "dependent highlight face")

(defface elg-header-line-face '((t (:background "black")))
  "Header line face.")

(defface elg-odd-numbered-line '((t (:inherit default)))
  "Face applied to odd numbered lines in the calendar.")

(defface elg-even-numbered-line '((t (:inherit default :background "gray17")))
  "Face applied to even numbered lines in the calendar.")

;;;; Constants

(defconst elg-leap-year-month-line #("| January xxxx                  | February xxxx               | March xxxx                    | April xxxx                   | May xxxx                      | June xxxx                    | July xxxx                     | August xxxx                   | September xxxx               | October xxxx                  | November xxxx                | December xxxx                 " 0 1 (face elg-vertical-line-face) 32 33 (face elg-vertical-line-face) 62 63 (face elg-vertical-line-face) 94 95 (face elg-vertical-line-face) 125 126 (face elg-vertical-line-face) 157 158 (face elg-vertical-line-face) 188 189 (face elg-vertical-line-face) 220 221 (face elg-vertical-line-face) 252 253 (face elg-vertical-line-face) 283 284 (face elg-vertical-line-face) 315 316 (face elg-vertical-line-face) 346 347 (face elg-vertical-line-face)))
(defconst elg-leap-year-date-line #("|1234567890123456789012345678901|12345678901234567890123456789|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901" 0 1 (face elg-vertical-line-face) 32 33 (face elg-vertical-line-face) 62 63 (face elg-vertical-line-face) 94 95 (face elg-vertical-line-face) 125 126 (face elg-vertical-line-face) 157 158 (face elg-vertical-line-face) 188 189 (face elg-vertical-line-face) 220 221 (face elg-vertical-line-face) 252 253 (face elg-vertical-line-face) 283 284 (face elg-vertical-line-face) 315 316 (face elg-vertical-line-face) 346 347 (face elg-vertical-line-face)))
(defconst elg-leap-year-blank-line #("|                               |                             |                               |                              |                               |                              |                               |                               |                              |                               |                              |                               " 0 1 (face elg-vertical-line-face) 32 33 (face elg-vertical-line-face) 62 63 (face elg-vertical-line-face) 94 95 (face elg-vertical-line-face) 125 126 (face elg-vertical-line-face) 157 158 (face elg-vertical-line-face) 188 189 (face elg-vertical-line-face) 220 221 (face elg-vertical-line-face) 252 253 (face elg-vertical-line-face) 283 284 (face elg-vertical-line-face) 315 316 (face elg-vertical-line-face) 346 347 (face elg-vertical-line-face)))
(defconst elg-normal-year-month-line #("| January xxxx                  | February xxxx              | March xxxx                    | April xxxx                   | May xxxx                      | June xxxx                    | July xxxx                     | August xxxx                   | September xxxx               | October xxxx                  | November xxxx                | December xxxx                 " 0 1 (face elg-vertical-line-face) 32 33 (face elg-vertical-line-face) 61 62 (face elg-vertical-line-face) 93 94 (face elg-vertical-line-face) 124 125 (face elg-vertical-line-face) 156 157 (face elg-vertical-line-face) 187 188 (face elg-vertical-line-face) 219 220 (face elg-vertical-line-face) 251 252 (face elg-vertical-line-face) 282 283 (face elg-vertical-line-face) 314 315 (face elg-vertical-line-face) 345 346 (face elg-vertical-line-face)))
(defconst elg-normal-year-date-line #("|1234567890123456789012345678901|1234567890123456789012345678|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901" 0 1 (face elg-vertical-line-face) 32 33 (face elg-vertical-line-face) 61 62 (face elg-vertical-line-face) 93 94 (face elg-vertical-line-face) 124 125 (face elg-vertical-line-face) 156 157 (face elg-vertical-line-face) 187 188 (face elg-vertical-line-face) 219 220 (face elg-vertical-line-face) 251 252 (face elg-vertical-line-face) 282 283 (face elg-vertical-line-face) 314 315 (face elg-vertical-line-face) 345 346 (face elg-vertical-line-face)))
(defconst elg-normal-year-blank-line #("|                               |                            |                               |                              |                               |                              |                               |                               |                              |                               |                              |                               " 0 1 (face elg-vertical-line-face) 32 33 (face elg-vertical-line-face) 61 62 (face elg-vertical-line-face) 93 94 (face elg-vertical-line-face) 124 125 (face elg-vertical-line-face) 156 157 (face elg-vertical-line-face) 187 188 (face elg-vertical-line-face) 219 220 (face elg-vertical-line-face) 251 252 (face elg-vertical-line-face) 282 283 (face elg-vertical-line-face) 314 315 (face elg-vertical-line-face) 345 346 (face elg-vertical-line-face)))

(defconst elg-cell-entry-re (concat "["
				    elg-deadline-character
				    elg-active-timestamp-character
				    elg-inactive-timestamp-character
				    elg-scheduled-character
				    elg-multiple-entry-character
				    elg-timestamp-range-end-character
				    elg-timestamp-range-start-character
				    elg-timestamp-range-ia-end-character
				    elg-cal-timestamp-range-ia-start-character
				    "]")
  "List of display characters for use as a regexp.")

;;;; Variables

(defvar elg--parsing-functions nil
  "List of functions for parsing org files.")

(defvar elg--display-rules nil
  "List of functions for drawing overlays in the buffer based on underlying text properties.")

(defvar elg--date-range nil
  "Range of years present in the agenda files.")

(defvar elg--vertical-bar-overlay-list nil
  "List of overlays for the vertical selection bar.")

;; Utility functions

(defun elg--change-symbol-name (symbol &optional prefix suffix)
  (intern (concat prefix (symbol-name symbol) suffix)))

(defun elg--add-remove-prop-colon (prop &optional remove)
  "PROP is a symbol with or without a colon prefix. 
  Returns a symbol with a colon prefix. If REMOVE is t, 
  then returns a symbol without a colon prefix.

  This is sometimes useful for parsing user-supplied
  property names."
  (if remove
      (if (s-starts-with-p ":" (symbol-name prop))
	  (intern (substring (symbol-name prop) 1))			
	prop)
    (if (s-starts-with-p ":" (symbol-name prop))
	prop			
      (intern (concat ":" (symbol-name prop))))))

(defun elg--plist-pair-p (plist key val &optional predicate)
  "Return t if PLIST has the KEY and VAL pair. Tests using `equal'.
Optional PREDICATE provides a function which performs equality test."
  (when-let ((stored-val (plist-get plist key)))
    (cond ((not predicate)
	   (equal stored-val val))
	  ((functionp predicate)
	   (funcall predicate stored-val val)))))

(defun elg-zip (args)
  "Zips multiple lists together. Example:
    (elg-zip '((1 5 9) (2 6 10) (3 7 11) (4 8 12)))
     => '((1 2 3 4) (5 6 7 8) (9 10 11 12)).
    All lists must be the same length."
  ;; Seems `dash' should be able to do this. 
  ;; (Maybe it does?)
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

;; Date utilities

(defsubst elg--get-days-in-year (year)
  "Return the number of days in YEAR." 
  (if (elg--leap-year-p year) 366 365))

(defsubst elg--leap-year-p (year)
  "Return t if YEAR is a leap year. Otherwise, nil."
  (= (% year 4) 0))

(defun elg--convert-date-string (date)
  "Converts an org date string to YYYY-MM-DD."
  (->> date
       (ts-parse)
       (ts-format "%Y-%m-%d")))

(defun elg--convert-date-to-column-number (timestamp)
  "Accepts a date in the form of YYYY-MM-DD and returns
the column of that date."
  (let ((spaces 0)
	(date timestamp))
    (cl-subseq elg--date-range
	       0 (cl-position (string-to-number (substring date 0 4)) elg--date-range))
    (dolist (year
	     (cl-subseq elg--date-range
			0 (cl-position (string-to-number (substring date 0 4)) elg--date-range)))
      (if (elg--leap-year-p year)
	  (setq spaces (+ spaces 366 12))
	(setq spaces (+ spaces 365 12))))
    (+ spaces (elg--convert-date-to-column-in-current-year date) elg-header-column-offset)))

(defun elg--convert-date-to-day-number-in-year (date)
  "accept a date in the format YYYY-MM-DD and return an int of day number of the year"
  (time-to-day-in-year (encode-time 0 0 0 (string-to-number (substring date 8 10))
				    (string-to-number (substring date 5 7))
				    (string-to-number (substring date 0 4)))))

(defun elg--convert-date-to-column-in-current-year (date)
  "accepts a date YYYY-MM-DD and returns the position on the horizontal calendar (int)
this works on leap years"
  (+ (elg--convert-date-to-day-number-in-year date)
     (- (string-to-number (substring date 5 7)) 1)))

;; Overlay utilities

;; (defun elg--create-overlay (&optional begin end &rest properties)
;;   "Create an overlay from BEGIN to END with PROPERTIES. If BEGIN is
;;   nil, then create the overlay at point. If END is nil, then create
;;   the overlay only at point. Returns the new overlay."
;;   (let ((overlay (make-overlay (or begin (point))
;; 			       (or end (1+ (point)))))
;; 	(len (length properties))
;; 	(i 0))
;;     (overlay-put overlay :elg t)
;;     (while (< i len)
;;       (overlay-put overlay
;; 		   (nth i properties) (nth (setq i (1+ i)) properties))
;;       (setq i (1+ i)))
;;     (setq i 0)
;;     overlay))

(defun elg--create-overlay (&optional begin end properties)
  "Create an overlay from BEGIN to END with PROPERTIES. If BEGIN is
  nil, then create the overlay at point. If END is nil, then create
  the overlay only at point. Returns the new overlay."
  (let ((overlay (make-overlay (or begin (point))
			       (or end (1+ (point)))))
	(len (length properties))
	(i 0))
    (overlay-put overlay :elg t)
    (while (< i len)
      (overlay-put overlay
		   (nth i properties) (nth (setq i (1+ i)) properties))
      (setq i (1+ i)))
    (setq i 0)
    overlay))


;; Parsing function

(defun elg--parser ()
  "Runs at each org heading and returns a plist of 
relevant properties to be inserted into the calendar buffer."
  (-let* (((&alist "CATEGORY" elg-category
		   "ITEM" elg-headline
		   "FILE" elg-file
		   "TIMESTAMP" elg-timestamp
		   "TIMESTAMP_IA" elg-timestamp-ia
		   "DEADLINE" elg-deadline
		   "SCHEDULED" elg-scheduled
		   "TODO" elg-todo
		   "ALLTAGS" elg-alltags
		   "ELG-DEPENDENTS" elg-dependents
		   "ELG-ANCHOR" elg-anchor)
	   (org-entry-properties))
	  ;; Return a new property list to be
	  ;; assigned to the cell. The first set
	  ;; match proerties from `org-entry-properties'.
	  (props (list :elg-category elg-category
		       :elg-headline elg-headline
		       :elg-file elg-file
		       :elg-deadline (when elg-deadline
				       (elg--convert-date-string elg-deadline))
		       :elg-scheduled (when elg-scheduled
					(elg--convert-date-string elg-scheduled))
		       :elg-todo elg-todo
		       :elg-marker (point-marker)
		       ;; Don't get the timestamps if they are ranges.
		       :elg-timestamp (when (and elg-timestamp
						 (not (s-match "--" elg-timestamp)))
					(elg--convert-date-string elg-timestamp))
		       :elg-timestamp-ia (when (and elg-timestamp-ia
						    (not (s-match "--" elg-timestamp-ia)))
					   (elg--convert-date-string elg-timestamp-ia))
		       ;; Don't get the ranges if they are single dates.
		       :elg-timestamp-range (when elg-timestamp
					      (if (not (s-match "--" elg-timestamp))
						  nil
						(let ((dates (s-split "--" elg-timestamp)))
						  (list (elg--convert-date-string (car dates))
							(elg--convert-date-string (cadr dates))))))
		       :elg-timestamp-range-ia (when elg-timestamp-ia
						 (if (not (s-match "--" elg-timestamp-ia))
						     nil
						   (let ((dates (s-split "--" elg-timestamp-ia)))
						     (list (elg--convert-date-string (car dates))
							   (elg--convert-date-string (cadr dates))))))
		       ;; Clean up the tags
		       :elg-alltags (when-let ((tag-string elg-alltags))
				      (mapcar #'org-no-properties (s-split ":" tag-string t)))
		       :elg-header (pcase elg-header-type
				     ('root (save-excursion 
					      (while (org-up-heading-safe))
					      (cdar (org-entry-properties (point) "ITEM"))))
				     ('hashtag (when elg-alltags
						 (org-no-properties (-first (lambda (tagstring) (s-starts-with-p "#" tagstring))
									    (s-split ":" elg-alltags)))))
				     ('category elg-category)
				     ((pred functionp) (funcall elg-header-type))
				     (_ (error "Invalid header type.")))
		       :elg-org-buffer (current-buffer))))
    (setq props (append props
			;; Set the date if it contains a date type in `elg-timestamps-to-display'
			`(:elg-date ,(plist-get props
						(elg--change-symbol-name (--first (plist-get props
											     (elg--change-symbol-name it ":elg-"))
										  elg-timestamps-to-display)
									 ":elg-")))))
    ;; Return only if there is an :elg-date, but first add some additional properties
    (when (plist-get props :elg-date)
      (append props
	      `(:elg-org-id ,(org-id-get-create))
	      ;; Append properites from `org-element-at-point' in
	      ;; case anyone wants to use them
	      (cadr (org-element-at-point))
	      ;; Run all custom parsing functions and append
	      ;; those values
	      (-flatten-n 1
			  (cl-loop for (prop . function) in elg--parsing-functions
				   collect `(,prop ,(funcall function))))))))

;; Iterator 

(defun elg--iterate ()
  "Iterate over all entries."
  (mapc #'elg--insert-entry
	(-non-nil
	 (org-ql-select elg-agenda-files
	   `(and (ts :from ,elg-start-date)
		 (not (tags ,(when elg-skip-archives
			       org-archive-tag))))
	   :action #'elg--parser))))

;; Calendar buffer functions

(defun elg--on-vertical-line ()
  "Is the cursor on a vertical line?"
  (looking-at "|"))

;; Getting data from the calendar buffer

(defun elg--select-entry (&optional prop-or-all val)
  "Prompt the user to select from multiple entries.
  If PROP is `all', then skip the prompt and return the
  list of all props at point. (i.e., the same thing as
  `elg-get-props-at-point')"
  (when-let ((prop-list (elg-get-prop-at-point)))
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
	   ;; TODO: turn this into an elg-selection-function that
	   ;; can be customized by the user
	   (let ((selection (completing-read "Select entry: "
					     (elg-get-prop-at-point :elg-headline)
					     nil
					     'require-match)))
	     (-first (lambda (x) (-contains? x selection)) prop-list))))))

(defun elg-get-header-at-point ()
  "Gets the header of the cell's current position.
  Returns nil if not on a header line."
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) 'elg-header)))

(defun elg-get-date-at-point (&optional column)
  "Get the date at point in YYYY-MM-DD format."
  ;; HACK: It works, so I am not touching it.
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

(defun elg-get-prop-at-point (&optional prop)
  "Returns all text properties at point. If a property is 
  specified, then return that property for each entry at point if 
  there are multiple entries.

  If there is only one entry, the value will be returned as a list of 
  one item."
  (let ((prop-list (plist-get (text-properties-at (point)) :elg)))
    (if prop
	(mapcar (lambda (props) (plist-get props prop))
		prop-list)
      prop-list)))

;; User movement functions

(defun elg-scroll (direction)
  ;; HACK: Ugly, but it works and it is reasonably fast.
  "Place, or move, an overlay on each line, hiding (or showing)
  the month immediately after the headers.
  DIRECTION must be a symbol: `forward' or `backard'."
  (let ((column (current-column))
	(line (line-number-at-pos))
	(date (elg-get-date-at-point)))
    (cond ((and (not elg--hidden-overlays)
		(eq direction 'forward))
	   (progn
	     (goto-char (point-min))
	     (setq elg--hidden-overlays 
		   (cl-loop with num-lines = (count-lines (point-min) (point-max))
			    for line from 1 to num-lines
			    collect (make-overlay (progn (move-to-column (1+ elg-header-column-offset)) (point))
						  (search-forward "|" (point-at-eol) t))
			    until (= (line-number-at-pos) num-lines)
			    do (forward-line)))
	     (cl-loop for overlay in elg--hidden-overlays
		      do (overlay-put overlay 'invisible t))))
	  (elg--hidden-overlays
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
	     (if (funcall end (car elg--hidden-overlays))
		 (cl-loop for overlay in elg--hidden-overlays
			  do (move-overlay overlay
					   (overlay-start overlay)
					   (funcall end overlay)))
	       (when (eq direction 'backward)
		 (setq elg--hidden-overlays nil))))))
    ;; This is an elaborate `save-excursion' to try to keep the
    ;; point in a reasonable place
    (goto-line line)
    (if (not (string= "" date ))
	(elg--goto-date date)
      (move-to-column column))))

(defun elg-scroll-forward ()
  "Interactive function to scroll forward by one month."
  (interactive)
  (elg-scroll 'forward))

(defun elg-scroll-backward ()
  "Interactive function to scroll forward by one month."
  (interactive)
  (elg-scroll 'backward))

(defun elg-scroll-to-current-month ()
  (interactive)
  (dotimes (_ (+ (* (- (car (last (calendar-current-date))) 
		       (string-to-number (substring elg-start-date 0 4)))
		    12)
		 (1- (string-to-number (substring elg-start-date 5 7)))))
    (elg-scroll-forward)))

(defun elg--move-forward ()
  "Moves to the next filled cell on the line. Does not move to 
  next line if it is at the last entry on the line."
  (interactive)
  (when (<= (line-number-at-pos) 2)
    (goto-line 3))
  (when (<= (current-column) elg-header-column-offset)
    (forward-char elg-header-column-offset))
  (when-let ((point (save-excursion 
		      (forward-char 1)
		      (re-search-forward elg-cell-entry-re
					 (point-at-eol)
					 t))))
    (goto-char (1- point))))

(defun elg--move-backward ()
  "Moves to the previous filled cell on the line. Does not move to 
  next line if it is at the last entry on the line."
  (interactive)
  (re-search-backward elg-cell-entry-re
		      (point-at-bol)
		      t))

(defun elg--move-vertically (up-or-down)
  "Move up or down to the nearest calendar entry."
  (if (eq up-or-down 'up)
      (if (> (org-current-line) 3)
	  (previous-line)
	(return-from elg--move-vertically nil))
    (if (< (org-current-line) (count-lines (point-min) (point-max)))
	(forward-line)
      (return-from elg--move-vertically nil)))
  (let ((next (save-excursion (re-search-forward elg-cell-entry-re (point-at-eol) t)))
	(previous (save-excursion (re-search-backward elg-cell-entry-re (point-at-bol) t))))
    (cond ((and (not next) (not previous))
	   (elg--move-vertically up-or-down))
	  ((and (not next) previous)
	   (goto-char previous))
	  ((and (not previous) next)
	   (goto-char (1- next)))
	  (t (if (< (- next (point)) (- (point) previous))
		 (goto-char (1- next))
	       (goto-char previous))))))

(defun elg--move-up ()
  (interactive)
  (elg--move-vertically 'up))

(defun elg--move-down ()
  (interactive)
  (elg--move-vertically 'down))

(defun elg--move-horizontally (n)
  "Ensures that the point is not on a vertical line."
  (forward-char n)
  (when (elg--on-vertical-line)
    (forward-char n)))

;; Programmatic movement functions 

(defun elg--goto-id (id &optional range)
  "Go to the cell for the org entry with ID. Return nil if not found."
  ;; Note: we cannot use `text-property-any' to find the value because
  ;; comparisons are done using `eq' which will not work for string values.
  ;; TODO: account for the fact that time ranges appear in the calendar twice
  (when-let ((point (cl-loop for points being the intervals of (current-buffer) property :elg
			     thereis (save-excursion
				       (goto-char (car points))
				       (let ((props (elg-get-prop-at-point)))
					 (when (-first (lambda (x)
							 (-contains? x id))
						       props)
					   (car points)))))))
    (goto-char point)))

(defun elg--goto-date (date)
  "Go to DATE in the current header. DATE is a string in \"YYYY-MM-DD\" format."
  (let ((overlay (car elg--hidden-overlays)))
    (if overlay
	(move-to-column (- (elg--convert-date-to-column-number date)
			   (- (overlay-end overlay)
			      (overlay-start overlay))))
      (move-to-column (elg--convert-date-to-column-number date)))))

(defun elg--date-calc (date offset &optional unit)
  "DATE is a string \"YYYY-MM-DD\"
  OFFSET is a positive or negative integer representing
  the number of days. UNIT should be day, month, year."
  (->> date
       (ts-parse)
       (ts-adjust (or unit 'day) offset)
       (ts-format "%Y-%m-%d")))

;; Interaction functions

(defun elg--shift-date (n &optional properties)
  "Move the timestamp up or down by one day.
  N must be 1 or -1. The return value
  is the prop list of the entry that has been moved."
  ;; Only allows moving by a single day
  (unless (or (= n 1)
	      (= n -1))
    (error "elg--shift-date: Invalid argument. N must be 1 or -1."))
  ;; HACK: This is about to get ugly to deal with timestamp ranges.
  (let ((props (or properties
		   (elg--select-entry)))
	(date (elg-get-date-at-point)))
    (elg-with-point-at-orig-entry props
	;; This regexp is adapted from
	;; `org-element--timestamp-regexp'
	;; but matches a specific date
	(when (re-search-forward (concat
				  "[[<]\\("
				  date
				  " ?[^]\n>]*?\\)[]>]\\|"
				  "\\(?:<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+"
				  "[dwmy]>\\)\\|\\(?:<%%\\(?:([^>\n]+)\\)>\\)")
				 nil t)
	  (org-timestamp-change n 'day)))
    ;; For some reason a normal `save-excursion' does not work here. 
    (let ((point (point)))
      (elg-update-this-cell)
      (goto-char point))
    (elg--move-horizontally n)
    (elg-update-this-cell)))

(defun elg--shift-date-forward ()
  (interactive)
  (elg--shift-date 1))

(defun elg--shift-date-backward ()
  (interactive)
  (elg--shift-date -1))

(defun elg--open-org-agenda-at-date ()
  (interactive)
  (let ((date (ts-format "%Y-%m-%d" (ts-parse (elg-get-date-at-point)))))
    (org-agenda-list nil date 'day))
  (other-window 1))

(defun elg-navigate-to-org-file ()
  "Navigate to a location in an org file for the cell at point."
  (interactive)
  (if-let* ((props (elg--select-entry))
	    (buffer (plist-get props :elg-org-buffer))
	    (marker (plist-get props :elg-marker)))
      (progn 
	(switch-to-buffer-other-window buffer)
	(org-goto-marker-or-bmk marker)
	(outline-show-children)
	(outline-show-entry)
	(beginning-of-line))
    (message "Cannot navigate to org file: no data at point.")))

(defmacro elg-with-point-at-orig-entry (props &rest body)
  "Execute BODY with point at marker stored in `:elg-marker'.
  Buffer is retrieved from the `:elg-org-buffer' property. If PROPS is nil, 
  then retrieve PROPS with `elg--select-entry’.
  If PROPS is supplied, use those props instead of the props at point."
  (declare (indent 2))
  `(let* ((props (or ,props (elg--select-entry)))
	  (marker (plist-get props :elg-marker))
	  (buffer (plist-get props :elg-org-buffer)))
     (with-current-buffer buffer
       (when (or (> marker (point-max))
		 (< marker (point-min)))
	 (widen))
       (goto-char marker)
       ,@body)))

;; Calendar drawing functions

(defun elg--draw-month-line (year)
  (insert 
   (if (elg--leap-year-p year)
       (replace-regexp-in-string "xxxx" (number-to-string year) 
				 elg-leap-year-month-line)
     (replace-regexp-in-string "xxxx" (number-to-string year) 
			       elg-normal-year-month-line))))

(defun elg--draw-number-line (year)
  (insert (if (elg--leap-year-p year)
	      elg-leap-year-date-line
	    elg-normal-year-date-line)))

(defun elg--draw-blank-line (year)
  (insert (if (elg--leap-year-p year)
	      elg-leap-year-blank-line
	    elg-normal-year-blank-line)))

(defun elg--get-header-create (header)
  "Put point at the first char in the HEADER line, creating a new header
  line if one does not exist."
  (goto-char (point-min))
  (let ((new-header (concat (s-truncate elg-header-column-offset header))))
    ;; Concat is necessary for reasons I do not understand. Without it,
    ;; the text properties are not set properly. 
    (if (search-forward new-header nil t)
	(beginning-of-line)
      (put-text-property 0 (length new-header) 'elg-header header new-header)
      (elg--insert-new-header-line new-header)
      (beginning-of-line))))

;; TODO: move this into `elg--get-header-create'
(defun elg--insert-new-header-line (header)
  "Inserts a new header."
  (goto-char (point-max))
  (insert "\n"
	  (substring 
	   (concat header (make-string elg-header-column-offset ? ))
	   0 elg-header-column-offset))
  (cl-loop for year in elg--date-range
	   do (if (elg--leap-year-p year)
		  (insert elg-leap-year-blank-line)
		(insert elg-normal-year-blank-line))))

(defun elg--insert-year (year &optional append)
  "For each line in the calendar, insert the appropriate
  lines to display YEAR. If APPEND is t, then add the years
  to the end of the calendar. (This should be calculated automatically, 
  but currently it is not.)"
  (goto-char (point-min))
  (if append
      (end-of-line)
    (move-to-column elg-header-column-offset))
  (elg--draw-month-line year)
  (forward-line)
  (if append
      (end-of-line)
    (move-to-column elg-header-column-offset))
  (elg--draw-number-line year)
  (cl-loop until (progn (end-of-line)
			(eobp))
	   do (progn (forward-line)
		     (if append
			 (end-of-line)
		       (move-to-column elg-header-column-offset))
		     (elg--draw-blank-line year))))

(defun elg--add-year (year)
  "Check to see if YEAR has already been displayed in the calendar.
  If so, do nothing. If not, insert that year for all calendar lines
  and all header lines in the calendar, and push the year onto 
  `elg--date-range' so that any new entries will contain the 
  proper number of years."
  (when (not (memq year elg--date-range))
    (cond ((not elg--date-range)
	   (cl-pushnew year elg--date-range)
	   (elg--insert-year year))
	  ((< year (first elg--date-range))
	   (let ((dif (- (first elg--date-range) year)))
	     (setq year (first elg--date-range))
	     (dotimes (_ dif)
	       (setq year (1- year))
	       (cl-pushnew year elg--date-range)
	       (elg--insert-year year))))
	  ((> year (car (last elg--date-range)))
	   (let ((dif (- year (car (last elg--date-range)))))
	     (setq year (car (last elg--date-range)))
	     (dotimes (_ dif)
	       (setq year (1+ year))
	       (cl-pushnew year elg--date-range)
	       (elg--insert-year year t)))))
    (setq elg--date-range (sort elg--date-range #'<))))

(defun elg--insert-entry (props)
  "Inserts text properties of a cell at point, keeping any properties which
    are already present. Updates the cell's display."
  ;; It is necessary to `mapc' over the date because date ranges
  ;; are stored as a list. If there is a date range the
  ;; properties are stored both at the first entry and the last entry.
  (let ((date (plist-get props :elg-date)))
    (mapc (lambda (date)
	    (elg--get-header-create (plist-get props :elg-header))
	    (elg--add-year (string-to-number (substring date 0 4)))
	    (elg--goto-date date)
	    (let ((old-props (plist-get (text-properties-at (point)) :elg)))
	      (unless (cl-loop for prop in old-props
			       if (equal (plist-get prop :elg-org-id)
					 (plist-get props :elg-org-id))
			       do (cl-loop for property in (-slice props 0 nil 2)
					   do (plist-put prop property (plist-get props property)))
			       and return t)
		(set-text-properties (point) (1+ (point)) `(:elg ,(append (list props)
									  old-props)))))
	    (elg--update-display-this-cell))
	  (-list date))))

;; Updating overlays

(defun elg--update-display-all-cells ()
  "Run functions in `elg--display-rules'"
  (remove-overlays (point-min) (point-max) :elg-user-overlay t)
  (save-excursion
    (goto-char (point-min))
    (while (next-single-property-change (point) :elg)
      (goto-char (next-single-property-change (point) :elg))
      (when (get-text-property (point) :elg)
	(elg--update-display-this-cell)))))

(defun elg--update-display-this-cell ()
  ;;(elg--display-rule-display-char)
  (cl-loop for func in elg--display-rules
	   do (funcall func)))

;; Changing the display
(defun elg--change-char (char &optional point)
  "Replace the character at point with CHAR, preserving all 
  existing text properties."
  (save-excursion 
    (let ((props (text-properties-at (point))))
      (when point (goto-char point))
      (delete-char 1)
      (insert char)
      (backward-char)
      (set-text-properties (point) (1+ (point)) props))))

;; Color conversion functions
(defun elg--color-rgb-to-hex (color)
  "Convert an RBG tuple '(R G B) to six digit hex string \"#RRGGBB\""
  (pcase-let ((`(,r ,g ,b) color))
    (color-rgb-to-hex r g b 2)))

(defun elg--color-name-to-hex (color)
  "Convert named color to six digit hex color."
  (eval `(color-rgb-to-hex ,@(color-name-to-rgb color) 2)))

(defalias 'elg--color-name-to-rgb #'color-name-to-rgb)

(defun elg--color-hex-to-rgb (hex-color)
  "Convert hex color to RGB tuple."
  `(,(string-to-number (substring hex-color 1 3) 16)
    ,(string-to-number (substring hex-color 3 5) 16)
    ,(string-to-number (substring hex-color 5 7) 16)))

(defun elg--color-to-rgb (color)
  "Convert a color name or hex color to RGB tuple."
  (pcase color
    ;; If it's hex...
    ((and (pred stringp)
	  (pred (s-starts-with-p "#")))
     (elg--color-hex-to-rgb color))
    ;; If it's a string (trust the user that the color
    ;; name is in `list-colors-display')...
    ((pred stringp)
     (elg--color-name-to-rgb color))
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

;; Gradients
(defun elg--get-color-midpoint (color1 color2)
  "Take two colors (any format) and return their
  average as an RGB tuple."
  (let ((color1 (elg--color-to-rgb color1))
	(color2 (elg--color-to-rgb color2)))
    (-zip-with (lambda (c1 c2)
		 (/ (+ c1 c2) 2))
	       color1 color2)))

(defun elg--draw-two-color-block (start-color end-color start end divider)
  (let ((start-color (elg--color-name-to-hex start-color))
	(end-color (elg--color-name-to-hex end-color)))
    (save-excursion
      (goto-char start)
      (cl-loop for x from start to end
	       do (goto-char x)
	       (remove-overlays (point) (1+ (point)))
	       (elg--create-overlay (point) (1+ (point))
				    `(face ,(if (<= (point) divider)
						`(:background ,start-color)
					      `(:background ,end-color))))
	       (forward-char)))))

(defun elg--draw-gradient (start-color end-color start end &optional mid-point props)
  (let ((color-gradient
	 (let ((start-color (elg--color-to-rgb start-color))
	       (end-color (elg--color-to-rgb end-color)))
	   (if mid-point
	       (let ((mid-color (elg--get-color-midpoint start-color
							 end-color)))
		 (append (color-gradient
			  start-color
			  mid-color
			  (1+ (- mid-point start))
			  (color-gradient mid-color
					  end-color
					  (- steps mid-point)))))
	     (color-gradient start-color
			     end-color
			     (1+ (- end start)))))))
    (save-excursion
      (goto-char start)
      (mapc (lambda (color)
	      (elg--create-overlay (point)
				   (1+ (point))
				   (-flatten-n 1 (append 
						  `(face ((:background ,(elg--color-rgb-to-hex color))))
						  props)))
	      (forward-char))
	    color-gradient))))



(defun elg--change-brightness-of-background-at-point (point change)
  "if there is a background font lock color, this will change its brightness"
  (let ((overlay (make-overlay point (1+ point))))
    (overlay-put overlay 'priority 999)
    (overlay-put overlay 'face `(:background ,(color-lighten-name
					       (background-color-at-point) change)))))

(defun elg--vertical-highlight ()
  (remove-overlays (point-min) (point-max) 'elg-vertical-highlight t)
  (cl-loop with overlay = nil
	   with line-length = (- (point-at-eol) (point-at-bol))
	   with point = (cl-loop with point = (point)
				 until (< point line-length)
				 do (setq point (- point line-length 1))
				 finally return point)
	   until (> point (point-max))
	   do (progn (push (make-overlay point (1+ point)) elg--vertical-bar-overlay-list)
		     (overlay-put (car elg--vertical-bar-overlay-list) 'priority 9999)
		     (overlay-put (car elg--vertical-bar-overlay-list) 'elg-vertical-highlight t)
		     (overlay-put (car elg--vertical-bar-overlay-list) 'face `(:background ,(color-lighten-name
											     (save-excursion
											       (goto-char point)
											       (background-color-at-point)) 15)))
		     (setq point (+ point line-length 1)))))

(defun elg--highlight-current-day ()
  (interactive)
  (save-excursion 
    (goto-char (point-min))
    (let ((date-line (elg--convert-date-to-column-number (format-time-string "%Y-%m-%d")))
	  (x 1)
	  (total-lines (count-lines (point-min) (point-max))))
      (while (<= x total-lines)
	(move-beginning-of-line 1)
	(forward-char date-line)
	(elg--change-brightness-of-background-at-point (point) +30)
	(forward-line)
	(setq x (1+ x))))
    (goto-char (point-min))))

;; TODO: deal with overlays
(defun elg--delete-cell-contents-at-point ()
  "Remove the character and properties at point." 
  (delete-char 1)
  (insert " ")
  (backward-char)
  (add-face-text-property (point)
			  (1+ (point))
			  (if (= (% (line-number-at-pos) 2) 0)
			      'elg-even-numbered-line
			    'elg-odd-numbered-line)))

(defun elg-update-this-cell (&optional date)
  "Gets data for a specific cell by looking for any headings
      which occur on DATE. If DATE is nil, use `elg-get-date-at-point'."
  (when (elg--on-vertical-line)
    (error "Error in elg-update-this-cell: Not on a calendar cell."))
  (save-excursion
    (let ((dates (sort
		  (-distinct
		   (-flatten
		    (append
		     (-list (or date
				(elg-get-date-at-point)))
		     (elg-get-prop-at-point :elg-date))))
		  #'string<)))
      (elg--delete-cell-contents-at-point)
      (mapc #'elg--insert-entry
	    (-non-nil (cl-loop for date in dates
			       append (org-ql-select elg-agenda-files
					`(and (ts :on ,date)
					      (not (tags ,(when elg-skip-archives
							    org-archive-tag))))
					:action #'elg--parser)))))))

;; Creating display rules
(cl-defmacro elg-create-display-rule (name &key docstring args parser body append disable post-command-hook)
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

      BODY is the body of the display function. DISPLAY-BODY should generally do one
      of the following: Setting an overlay, setting text-properties, changing the face, etc.
      The return value of BODY is ignored and all changes must be made through side-effect. 
      - An overlay can be set with `elg--create-overlay'.
      - The character of a cell can be changed by using `elg--change-char'.
      - The gradient of a cell, or cells, can be changed with `elg--draw-gradient'.
      - A progress bar can be drawn with `elg--draw-two-color-block'. 

      After the display function is created, it is pushed onto `elg--display-functions'.
      These functions are run for each cell at point, from the start of the list to the 
      end. 

      If APPEND is non-nil, then the function will be appended to the end of
      `elg--display-functions' rather than pushed to the front.p 

      If POST-COMMAND-HOOK is non-nil, then the display function will be added as a post
      command hook. If this option is used, make sure to give the overlay a custom name
      so that it can be cleared. If it is nil, then the hook will be removed if it 
      exists.

      If DISABLE is non-nil, then the rule will be removed from the 
      `elg--display-rules', any parsing functions created by the rule will
      be removed, and any hook will be removed."

  (declare (indent defun))
  (let ((display-func-name (intern (concat "elg--display-rule-" (symbol-name name)))))
    `(progn
       (when ',parser
	 (cl-loop for (prop . val) in (-list ',parser)
		  do (setf (alist-get (if (s-starts-with-p ":" (symbol-name prop))
					  prop
					(intern (concat ":" (symbol-name prop))))
				      elg--parsing-functions)
			   `(lambda () ,@val))))
       (if (or ',parser ',args)
	   (progn
	     (defun ,display-func-name ()
	       ,docstring
	       (mapc
		(lambda (arg-list)
		  (-let ((,(append (cl-loop for arg in args
					    collect (elg--add-remove-prop-colon arg t))
				   (cl-loop for (prop . val) in parser
					    collect (elg--add-remove-prop-colon prop t)))
			  arg-list))
		    ,@body))
		(or (elg-zip
		     (mapcar #'elg-get-prop-at-point
			     (append ',(cl-loop for arg in args
						collect (elg--add-remove-prop-colon arg))
				     ',(cl-loop for (prop . val) in parser
						collect (elg--add-remove-prop-colon prop)))))
		    ;; If the preceding code returns `nil', then the `mapc' function, above,
		    ;; will not run. Since `elg-get-prop-at-point' will usually return nil
		    ;; if on an empty cell, it creates a problem if the user wants to run
		    ;; the command in an empty cell. 
		    ;; To avoid this, if `elg-zip' returns nil, this will create a list of nils to
		    ;; be assigned to the argument list, since nil is not `eq' to (nil),
		    ;; `mapc' will accept the list and run.
		    (make-list (if (> 0 (length (elg-get-prop-at-point))) 
				   (length (elg-get-prop-at-point)) 1)
			       (make-list (+ (length ',parser) (length ',args)) nil))))))
	 (defun ,display-func-name () ,docstring ,@body))
       (if ',append
	   (progn
	     (setq elg--display-rules (remq ',display-func-name elg--display-rules))
	     (add-to-list 'elg--display-rules #',display-func-name t))
	 (setq elg--display-rules (remq ',display-func-name elg--display-rules))
	 (cl-pushnew #',display-func-name elg--display-rules))
       (if ',post-command-hook
	   (add-hook 'post-command-hook #',display-func-name t t)
	 (remove-hook 'post-command-hook #',display-func-name t))
       (when ',disable
	 (cl-loop for (name . func) in ',parser
		  do (setq elg--parsing-functions
			   (assq-delete-all name elg--parsing-functions)))
	 (remove-hook 'post-command-hook #',display-func-name t)
	 (setq elg--display-rules (remq ',display-func-name elg--display-rules))))))

(elg-create-display-rule display-char
  :args (elg-deadline elg-timestamp elg-timestamp-ia elg-scheduled elg-timestamp-range elg-timestamp-range-ia)
  :body ((let ((elg-multi (> (length (elg-get-prop-at-point)) 1)))
	   (elg--change-char (cond (elg-multi elg-multiple-entry-character)
				   (elg-deadline elg-deadline-character)
				   (elg-timestamp elg-active-timestamp-character)
				   (elg-timestamp-range
				    (if (string= (elg-get-date-at-point) (car elg-timestamp-range))
					elg-timestamp-range-start-character
				      elg-timestamp-range-end-character))
				   (elg-timestamp-range-ia
				    (if (string= (elg-get-date-at-point) (car elg-timestamp-range-ia))
					elg-cal-timestamp-range-ia-start-character
				      elg-timestamp-range-ia-end-character))
				   (elg-timestamp-ia elg-inactive-timestamp-character)
				   (elg-scheduled elg-scheduled-character)
				   ;; There shouldn't be anything left over
				   (t (error "Unrecognized date type.")))))))

(elg-create-display-rule user-set-color
  :parser ((elg-color . ((org-entry-get (point) "ELG-COLOR")))
	   (elg-linked-to . ((org-entry-get (point) "ELG-LINKED-TO"))))
  :append t
  :body ((when elg-linked-to
	   (save-excursion 
	     (let ((point (point)))
	       (elg--draw-gradient 
		(progn (elg--goto-id elg-linked-to)
		       (elg-with-point-at-orig-entry nil
			   (or (org-entry-get (point) "ELG-COLOR")
			       "blue")))
		elg-color
		(point)
		point
		nil
		'(priority 99999 :elg-user-overlay t)))))))


(cl-defmacro elg-create-action (name &key docstring parser args body binding)
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
     on the org file, or get data from it, use `elg-with-point-at-orig-entry'. Otherwise,
     action can do anything you'd like. 

     BINDING the key binding for the newly defined ACTION. It allows any
     string accepted by `kbd'."
  (declare (indent defun))
  (let ((action-func-name (intern (concat "elg--action-rule-" (symbol-name name)))))
    `(progn
       (when ',parser
	 (cl-loop for (prop . val) in (-list ',parser)
		  do (setf (alist-get (if (s-starts-with-p ":" (symbol-name prop))
					  prop
					(intern (concat ":" (symbol-name prop))))
				      elg--parsing-functions)
			   `(lambda () ,@val))))
       (if (or ',parser ',args)
	   (progn
	     (defun ,action-func-name ()
	       ,docstring
	       (interactive)
	       (mapc
		(lambda (arg-list)
		  (-let ((,(append (cl-loop for arg in args
					    collect (elg--add-remove-prop-colon arg t))
				   (cl-loop for (prop . val) in parser
					    collect (elg--add-remove-prop-colon prop t)))
			  arg-list))
		    ,@body))
		(or (elg-zip
		     (mapcar #'elg-get-prop-at-point
			     (append ',(cl-loop for arg in args
						collect (elg--add-remove-prop-colon arg))
				     ',(cl-loop for (prop . val) in parser
						collect (elg--add-remove-prop-colon prop)))))
		    ;; If the preceding code returns `nil', then the `mapc' function, above,
		    ;; will not run. Since `elg-get-prop-at-point' will usually return nil
		    ;; if on an empty cell, it creates a problem if the user wants to run
		    ;; the command in an empty cell. 
		    ;; To avoid this, if `elg-zip' returns nil, this will create a list of nils to
		    ;; be assigned to the argument list, since nil is not `eq' to (nil),
		    ;; `mapc' will accept the list and run.
		    (make-list (+ (length ',parser) (length ',args)) nil)))))
	 (defun ,action-func-name () ,docstring (interactive) ,@body))
       (when ',binding 
	 (define-key elg-mode-map (kbd ,binding) #',action-func-name)))))

;; Open function
(defun elg--draw-even-odd-background ()
  "Set the background for even and odd lines."
  (save-excursion 
    (goto-char (point-min))
    (cl-loop do (progn (add-face-text-property (point-at-bol)
					       (point-at-eol)
					       (if (= (% (line-number-at-pos) 2) 0)
						   'elg-even-numbered-line
						 'elg-odd-numbered-line)
					       'append)
		       (forward-line))
	     until (eobp))))

(defun elg-open ()
  "Open gantt calendar."
  (interactive)
  (switch-to-buffer "*El Gantt Calendar*")
  (let ((point (point)))
    (setq elg--date-range nil)
    (setq elg--hidden-overlays nil)
    (setq header-line-format elg-header-line-format)
    (erase-buffer)
    (insert (make-string elg-header-column-offset ? )
	    "\n"
	    (make-string elg-header-column-offset ? ))
    (elg--iterate)
    (read-only-mode -1)
    (elg--draw-even-odd-background)
    (elg--update-display-all-cells)
    (elg-mode)
    (toggle-truncate-lines 1)
    (goto-char point)))

;; Major mode
(setq elg-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "r")   #'elg-open)
	(define-key map (kbd "SPC") #'elg-navigate-to-org-file)
	(define-key map (kbd "p")   #'elg--move-up)
	(define-key map (kbd "a")   #'elg-interaction::start-action)
	(define-key map (kbd "c")   #'elg-scroll-to-current-month)
	(define-key map (kbd "n")   #'elg--move-down)
	(define-key map (kbd "f")   #'elg--move-forward)
	(define-key map (kbd "F")   #'elg-scroll-forward)
	(define-key map (kbd "B")   #'elg-scroll-backward)
	(define-key map (kbd "b")   #'elg--move-backward)
	(define-key map (kbd "RET") #'elg--open-org-agenda-at-date)
	(define-key map (kbd "M-f") #'elg--shift-date-forward)
	(define-key map (kbd "M-b") #'elg--shift-date-backward)
	(define-key map (kbd "C-M-f") #'elg-move-date-and-dependents-forward)
	(define-key map (kbd "C-M-b") #'elg-move-date-and-dependents-backward)
	map))

(define-derived-mode elg-mode special-mode
  "El Gantt"
  "Horizontal calendar interface for orgmode. \{keymap}"
  (read-only-mode -1)
  (add-hook 'post-command-hook #'elg--vertical-highlight nil t))

;;;; Footer

(provide 'elg)

;;; elg.el ends here
