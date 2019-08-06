;;; elgantt.el --- Generate integrated text-based Gantt Charts from Orgmode files  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jeff Filipovits

;; Author: Jeff Filipovits <jrfilipovits@gmail.com>
;; Url: https://github.com/legalnonsense/elgantt
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.0") (s "1.12.0"))
;; Keywords: Org, agenda, calendar, outlines, gantt

;; This file is not part of GNU Emacs.

;;; Commentary:

;; El Gantt generates a text-based Gantt Chart/Calendar from orgmode files. 
;; El Gannt relies on the use of tags to designate how to generate the charts.
;; The goal is to for you to be able to customize your chart without altering the way you use org mode.
;; In other words, El Gantt allows you to customize your charts while staying out of the way. 
;; The chart/calendar generated is integrated with orgmode and can jump to the point
;; of an org file and open an agenda for each day of the chart. See the README. 

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

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'elgantt)

;;;; Usage

;; Run this command: 

;; `elgantt-open': Open a Gantt Calendar from your agenda files

;;;; Tips

;; + You can customize settings in the `elgantt' group.

;;;; Credits

;; This package would not be possible absent the assistance of alphapapa. See <https://github.com/alphapapa/>

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
(require 's)

;;;; Customization

(defgroup gantt-org nil
  "Options about gantt-org."
  :tag "Gantt Org"
  :group 'org
  :group 'gantt)

(defcustom elgantt-use-hashtags nil
  "If non-nil, use tags that are prefixed with a hashtag to generate the headings of the calendar. If nil (default), then use the CATEGORY property to generate the headings.")

(defcustom elgantt-default-block-start-color "#696969"
  "Default color used at the beginning of a time block.")

(defcustom elgantt-default-block-end-color "#ff4500"
  "Default end color used for time blocks.")

(defcustom elgantt-custom-colors '((discovery . "#4444ff")
				   (brief     . "#ff8c00")
				   (waiting   . "#cdcd00")
				   (court     . "#00ff7f"))
  "Colors used for specific block tags.")

(defcustom elgantt-exclusions '("Habits" "Personal" "Business" "taskmaster" "Unsorted" "Computer" "Business")
  "Exclude these categories from the calendar.")

(defcustom elgantt-deadline-character "▲"
  "Character used for deadlines in the calendar.")

(defcustom elgantt-active-timestamp-character "●"
  "Character used for active timestamps in the calendar")

(defcustom elgantt-dark-mode nil
  "If you are using a dark theme, enable this.")

(defcustom elgantt-brightness-adjust -15
  "The color of every other line in the calendar is darkened or lightened for readability. This is a percent and can be negative (darken) or positive (lighten).")

(defcustom elgantt-agenda-files 'agenda
  "Files are used to generate the calendar. Accepts any value used by org-map-entries:

nil     The current buffer, respecting the restriction if any
tree    The subtree started with the entry at point
region  The entries within the active region, if any region-start-level
        The entries within the active region, but only those at
        the same level than the first one.
file    The current buffer, without restriction
file-with-archives
        The current buffer, and any archives associated with it
agenda  All agenda files
agenda-with-archives
        All agenda files with any archive files associated with them
(file1 file2 ...)
        If this is a list, all files in the list will be scanned")

(defcustom elgantt-tentative-block-brightness 25
  "If a block also has the :tentative: tag, then make it appear this percent lighter (or darker (with a negative value)) than normal.")
    
(defcustom elgantt-default-background-color (face-attribute 'default :background)
  "Background color for the calendar; defaults to the background of the default face.")

;;not sure what this one will do in January; this could also allow the user to determine how many preceding months to show
;;but will moire likely be adapted to allow for side scrolling
(defcustom elgantt-hide-previous-months t
  "Begins the calender showing only the preceding month; if nil, it will show the entire current year. Use (elgantt-show-all-dates) to show all.")

;;;; Variables

(defvar elgantt--map-data nil)
(defvar elgantt--normal-year-date-line  "|1234567890123456789012345678901|1234567890123456789012345678|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901")
(defvar elgantt--normal-year-month-line "| January xxxx                  | February xxxx              | March xxxx                    | April xxxx                   | May xxxx                      | June xxxx                    | July xxxx                     | August xxxx                   | September xxxx               | October xxxx                  | November xxxx                | December xxxx                 ")
(defvar elgantt--normal-year-blank-line "|                               |                            |                               |                              |                               |                              |                               |                               |                              |                               |                              |                               ")
(defvar elgantt--leap-year-date-line    "|1234567890123456789012345678901|12345678901234567890123456789|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901|123456789012345678901234567890|1234567890123456789012345678901")
(defvar elgantt--leap-year-month-line   "| January xxxx                  | February xxxx               | March xxxx                    | April xxxx                   | May xxxx                      | June xxxx                    | July xxxx                     | August xxxx                   | September xxxx               | October xxxx                  | November xxxx                | December xxxx                 ")
(defvar elgantt--leap-year-blank-line   "|                               |                             |                               |                              |                               |                              |                               |                               |                              |                               |                              |                               ")
(defvar elgantt--header-column-offset nil)

;;;;; Keymaps

(define-derived-mode elgantt-mode special-mode "El Gantt"
  (define-key elgantt-mode-map (kbd "r") 'elgantt-open)
  (define-key elgantt-mode-map (kbd "SPC") 'elgantt--open-org-file-at-point)
  (define-key elgantt-mode-map (kbd "f")   'elgantt--move-selection-bar-forward)
  (define-key elgantt-mode-map (kbd "b")   'elgantt--move-selection-bar-forward)
  (define-key elgantt-mode-map (kbd "RET") 'elgantt--open-org-agenda-at-date))

(defun elgantt--highlight-weekends-in-date-line (line-number-string years)
  (let ((months [31 28 31 30 31 30 31 31 30 31 30 31]))
    (dotimes (y (length years))
      (if (elgantt--leap-year-p (nth y years))
	  (setq months [31 29 31 30 31 30 31 31 30 31 30 31])
	(setq months [31 28 31 30 31 30 31 31 30 31 30 31]))
      (dotimes (m 12)
	(dotimes (d (aref months m))
	  (when (or (= (org-day-of-week (1+ d) (1+ m) (nth y years)) 5)
		    (= (org-day-of-week (1+ d) (1+ m) (nth y years)) 6))
	    (put-text-property (elgantt--convert-date-to-column-number (elgantt--normalize-date-string (format "%d-%d-%d" (nth y years) (1+ m) (1+ d))))
			       (1+ (elgantt--convert-date-to-column-number (elgantt--normalize-date-string (format "%d-%d-%d" (nth y years) (1+ m) (1+ d)))))
			       'font-lock-face `(:background "#696969") line-number-string))))))
  line-number-string)

;;;;; Functions
 
(defun elgantt-open ()
  (interactive)
  (switch-to-buffer "*El Gantt Calendar*")
  (let ((inhibit-read-only t))
    (erase-buffer))
  (when elgantt-dark-mode
    (setq elgantt-brightness-adjust 15))
  (setq elgantt--map-data nil) ; re-initialize the orgmode data
  (setq elgantt--old-backgrounds '())
  (elgantt--draw)
  (elgantt-mode)
  (toggle-truncate-lines 1)
  (setq cursor-type 'box)
  (goto-char (point-min))
  (forward-line 2)
  (forward-char (elgantt--convert-date-to-column-number (format-time-string "%Y-%m-%d")))
  (add-hook 'post-command-hook 'elgantt--show-echo-message nil t)
  (add-hook 'post-command-hook 'elgantt--vertical-highlight nil t)
  (delete-other-windows)
  (elgantt--hide-future-dates)
  (elgantt-hide-past-dates))

(defun elgantt--draw ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (elgantt--parse-org-files)
    (erase-buffer)
    (insert (elgantt--draw-month-line))
    (insert "\n")
    (insert (elgantt--draw-number-line))
    (insert "\n")
    (setq elgantt/variables/number-of-lines 1)
    (dolist (header (elgantt--get-all-headers))
      (insert (elgantt--draw-string-for-header header))
      (insert "\n"))
    (elgantt--highlight-current-day)))

(defun elgantt--show-parsed-data ()
  "this is for debugging"
  (interactive)
  (with-output-to-temp-buffer "El Gantt Parsed Data"
    (dolist (header elgantt--map-data)
      (prin1 (car header))
      (terpri)
      (dolist (property (cdr header))
	(prin1 "    ")
	(prin1 property)
	(terpri))))
  (switch-to-buffer-other-window "El Gantt Parsed Data")
  (toggle-truncate-lines 1))

(defun elgantt--parse-org-files ()
  "returns a list of alists. the cdr of each alist is a plist of the properties of the heading
   access the elgantt--map-data data structure using the elgantt/data functions
   if use-hashtag is non-nil, then it will use tags that start with # to create the
   elgantt--map-data. the argument type-of-search parameter allows the specification of a specific file to use by passing a list of files
   and valid values are determined by the SCOPE parameter in org-map-entries"
  (interactive)
  (setq elgantt--map-data '())
  (org-map-entries (lambda ()
		     ;; get the header by interning the category
		     (let ((header (intern (elgantt--get-property-from-org-point))))
		       ;; when using hashtags, find the tag with the hashtag, remove the hashtag, and intern it
		       (when elgantt-use-hashtags
			 (if (elgantt--get-hashtag-from-org-point)
			     (setq header (intern (elgantt--get-hashtag-from-org-point)))
			   (setq header nil)))
		       (when (not (member (symbol-name header) elgantt-exclusions))
			 ;; if there is no hashtag, then skip the entry bc it will not appear in the calendar
			 (when header
			   (elgantt--add-properties header `(:category                  ,(elgantt--get-property-from-org-point)
								 :hashtag                   ,(elgantt--get-hashtag-from-org-point)
								 :tag-string                ,(elgantt--get-all-tags-as-string-from-org-point)
								 :timestamp-active          ,(elgantt--get-active-timestamp-from-org-point)
								 :timestamp-inactive        ,(elgantt--get-inactive-timestamp-from-org-point)
								 :timestamp-range-active    ,(elgantt--get-active-time-range-from-org-point)
								 :timestamp-range-inactive  ,(elgantt--get-inactive-time-range-from-org-point)	
								 :deadline                  ,(elgantt--get-deadline-from-org-point)
								 :headline-text             ,(elgantt--get-headline-text-from-org-point)
								 :todo                      ,(elgantt--get-todo-from-org-point)
								 :start-or-end-or-range     ,(nth 0 (elgantt--get-calendar-data-from-org-point))
								 :calendar-label            ,(nth 1 (elgantt--get-calendar-data-from-org-point))
								 :calendar-date             ,(nth 2 (elgantt--get-calendar-data-from-org-point))
								 :file                      ,(elgantt--get-file-from-org-point)
								 :calendar-point nil        ; done, this fills in later 
								 :calendar-column nil       ; done, fills below
								 :calendar-blocks nil       ; to be filled later
								 :org-point ,(point)))))))
  		   nil elgantt-agenda-files 'archive)

  (setq elgantt--header-column-offset (+ (elgantt--get-longest-header-length) 1))
  
  (dolist (property (elgantt--get-all-properties-from-header))
    (when (stringp (plist-get property :calendar-date))
      (plist-put property :calendar-column (elgantt--convert-date-to-column-number (plist-get property :calendar-date)))))
  elgantt--map-data)

(defun elgantt--convert-date-to-column-number (date)				    
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

(defun elgantt--get-longest-header-length ()
  (let ((x 0))
    (dolist (header elgantt--map-data)
      (when (> (length (symbol-name (car header))) x)
	(setq x (length (symbol-name (car header))))))
    x))

(defun elgantt--add-properties (header properties)
  "check to see if header is in list, if not, add it
                   then add properties"
  (when (not (elgantt--check-if-header-exists header))
    (elgantt--add-header-to-list header))
  (elgantt--add-property-list-to-header header properties))

(defun elgantt--add-header-to-list (header)
  "add a header to the list
                     adds in the form of '(,header . "")
                     (for now, it appears initializing with "" is necessary to create a blank alist"
  (add-to-list 'elgantt--map-data `(,header . "")))

(defun elgantt--check-if-header-exists (header)
  "check to see if the new header is a member of the elgantt--map-data
                     pass a quoted name e.g., 'Jones; return t or nil"
  (if (assoc header elgantt--map-data)
      't
    nil))

(defun elgantt/data/check-if-header-has-elements (header)
  "check if a header has elements"
  (if (eq (alist-get header elgantt--map-data) "")
      nil
    't))

(defun elgantt--get-property-list-for-header (header)
  (if (eq (alist-get header elgantt--map-data) "")
      nil
    (alist-get header elgantt--map-data)))

(defun elgantt--add-property-list-to-header (header property-list)
  "adds information from an org heading to the appropriate header"
  (setf (alist-get header elgantt--map-data) (append (alist-get header elgantt--map-data) (list property-list))))

(defun elgantt--get-hashtag-from-tag-string (tag-string)
  "accept a string :x:y:#z and return the tag that begins with a hash, otherwise nil only returns the first hashtag encountered"
  (catch 'hashtag 
    (dolist (tag (s-split ":" tag-string))
      (when (s-starts-with-p "#" tag)
	(throw 'hashtag (substring-no-properties tag))))))

(defun elgantt--generate-plist-for-heading (point &optional hashtag)
  "accept a point in an org file, within the context of org-map-entries, 
               and return an element of an alist in the form of '(header . '(plist of propertiets))")

(defun elgantt--get-property-from-org-point ()
  (cdr (car (org-entry-properties (point) "CATEGORY"))))

(defun elgantt--get-all-tags-as-string-from-org-point ()
  (if (symbolp (cdr (car (org-entry-properties (point) "ALLTAGS"))))
      (symbol-name (cdr (car (org-entry-properties (point) "ALLTAGS"))))
    (substring-no-properties (cdr (car (org-entry-properties (point) "ALLTAGS"))))))

(defun elgantt--get-file-from-org-point ()
  (cdr (car (org-entry-properties (point) "FILE"))))

(defun elgantt--get-hashtag-from-org-point ()
  (if (elgantt--get-hashtag-from-tag-string (elgantt--get-all-tags-as-string-from-org-point))
      (substring (elgantt--get-hashtag-from-tag-string (elgantt--get-all-tags-as-string-from-org-point)) 1)
    nil))

(defun elgantt--get-todo-from-org-point ()
  (cdr (car (org-entry-properties (point) "TODO"))))

(defun elgantt--get-deadline-from-org-point ()
  (when (cdr (car (org-entry-properties (point) "DEADLINE")))
    (if (string= (cdr (car (org-entry-properties (point) "DEADLINE"))) (elgantt--get-property-from-org-point))
	nil
      (elgantt--normalize-date-string (cdr (car (org-entry-properties (point) "DEADLINE")))))))

(defun elgantt--get-active-time-range-from-org-point ()
  (when (cdar (org-entry-properties (point) "TIMESTAMP"))
    (cond ((string= (cdar (org-entry-properties (point) "TIMESTAMP")) (elgantt--get-property-from-org-point)) nil)
	  ((not (s-match "--" (cdar (org-entry-properties (point) "TIMESTAMP")))) nil)
	  (t
	   (let ((dates (s-split "--" (cdar (org-entry-properties (point) "TIMESTAMP")))))
	     (list (elgantt--normalize-date-string (car dates)) (elgantt--normalize-date-string (cadr dates))))))))

(defun elgantt--get-active-timestamp-from-org-point ()
  (when (cdr (car (org-entry-properties (point) "TIMESTAMP")))
    (cond ((string= (cdr (car (org-entry-properties (point) "TIMESTAMP"))) (elgantt--get-property-from-org-point)) nil)
	  ((s-match "--" (cdar (org-entry-properties (point) "TIMESTAMP"))) nil)
	  (t
	   (elgantt--normalize-date-string (cdr (car (org-entry-properties (point) "TIMESTAMP"))))))))                                             

(defun elgantt--get-inactive-time-range-from-org-point ()
  (when (cdar (org-entry-properties (point) "TIMESTAMP_IA"))
    (cond ((string= (cdar (org-entry-properties (point) "TIMESTAMP_IA")) (elgantt--get-property-from-org-point)) nil)
	  ((not (s-match "--" (cdar (org-entry-properties (point) "TIMESTAMP_IA")))) nil)
	  (t
	   (let ((dates (s-split "--" (cdar (org-entry-properties (point) "TIMESTAMP_IA")))))
	     (list (elgantt--normalize-date-string (car dates)) (elgantt--normalize-date-string (cadr dates))))))))

(defun elgantt--get-inactive-timestamp-from-org-point ()
  (when (cdr (car (org-entry-properties (point) "TIMESTAMP_IA")))
    (cond ((string= (cdr (car (org-entry-properties (point) "TIMESTAMP_IA"))) (elgantt--get-property-from-org-point)) nil)
	  ((s-match "--" (cdar (org-entry-properties (point) "TIMESTAMP_IA"))) nil)
	  (t
	   (elgantt--normalize-date-string (cdr (car (org-entry-properties (point) "TIMESTAMP_IA"))))))))                                             

(defun elgantt--get-headline-text-from-org-point ()
  (cdr (car (org-entry-properties (point) "ITEM"))))

(defun elgantt--get-calendar-data-from-org-point ()
  "This function will get dates for the calendar in the following order of precedence: deadlines, active timestamps, inactive timestamps"
  (let ((label nil)
	(start-or-end-or-range nil)
	(date nil))
    (dolist (tag (s-split ":" (elgantt--get-all-tags-as-string-from-org-point)))
      (when (or (s-ends-with-p "_start" tag) (s-ends-with-p "_end" tag) (s-ends-with-p "_block" tag))
	(setq label (car (s-split "_" tag)))
	(setq start-or-end-or-range (cadr (s-split "_" tag)))
	(if (string= start-or-end-or-range "block")
	    (cond ((elgantt--get-active-time-range-from-org-point) (setq date (elgantt--get-active-time-range-from-org-point)))
		  ((elgantt--get-inactive-time-range-from-org-point) (setq date (elgantt--get-inactive-time-range-from-org-point))))
	  (cond ((elgantt--get-deadline-from-org-point) (setq date (elgantt--get-deadline-from-org-point)))
		((elgantt--get-active-timestamp-from-org-point) (setq date (elgantt--get-active-timestamp-from-org-point)))
		((elgantt--get-inactive-timestamp-from-org-point) (setq date (elgantt--get-inactive-timestamp-from-org-point)))))))
        `(,start-or-end-or-range ,label ,date)))

(defun elgantt--get-all-headers ()
  (let ((headers '()))
    (dolist (header elgantt--map-data)
      (push (car header) headers))
    headers))

(defun elgantt--get-all-properties-from-header ()
  "returns a list of all properties"
  (let ((properties-list '()))
    (dolist (header elgantt--map-data)
      (setq header (car header))
      (dolist (properties (alist-get header elgantt--map-data))
	(push properties properties-list)))
    properties-list))

(defun elgantt--get-range-of-years ()
  (let ((years '()))
    (dolist (property (elgantt--get-all-properties-from-header))
      (when (plist-get property :deadline)
	(when (not (memq (string-to-number (substring (plist-get property :deadline) 0 4)) years))
	  (push (string-to-number (substring (plist-get property :deadline) 0 4)) years)))
      (when (plist-get property :timestamp-active)
	(when (not (memq (string-to-number (substring (plist-get property :timestamp-active) 0 4)) years))
	  (push (string-to-number (substring (plist-get property :timestamp-active) 0 4)) years)))
      (when (plist-get property :timestamp-range-active)
	(when (not (memq (string-to-number (substring (cadr (plist-get property :timestamp-range-active)) 0 4)) years))
	  (push (string-to-number (substring (cadr (plist-get property :timestamp-range-active)) 0 4)) years))))
    (sort years '<)))

(defun elgantt--get-oldest-date ()
  "Gets the oldest date in the entire file of any deadline or active timestamp"
  (let ((last-date "1970-01-01"))
    (dolist (property (elgantt--get-all-properties-from-header))
      (when (plist-get property :deadline)
	(setq last-date (elgantt--return-later-date last-date (plist-get property :deadline))))
      (when (plist-get property :timestamp-active)
	(setq last-date (elgantt--return-later-date last-date (plist-get property :timestamp-active)))))
    last-date))

(defun elgantt--normalize-date-string (date)
  (when (or (string= (substring date 0 1) "<") (string= (substring date 0 1) "["))
    (setq date (substring date 1)))
  (let ((new-date ""))
    (dolist (element (s-split "-"  date))
      (if (< (string-to-number element) 10)
	  (setq new-date (concat new-date "0" (number-to-string (string-to-number element))))
	(setq new-date (concat new-date element))))
    (concat (substring new-date 0 4) "-" (substring new-date 4 6) "-" (substring new-date 6 8))))

(defun elgantt--return-later-date (date1 date2)
  "return the later of two dates, assuming YYYY-MM-DD format"
  (car (sort (list date1 date2) 'string>)))

(defun elgantt--draw-string-for-header (header)
  "this draws the string that will appear in the calendar for each header and returns it"
  (setq elgantt/variables/line-length (length (elgantt--draw-number-line)))
  (setq elgantt/variables/number-of-lines (1+ elgantt/variables/number-of-lines))
  (let ((date-line (elgantt--generate-blank-date-line header)))
    ;;put a background that matches the default background so that it is easier to manipulate later 

    ;(elgantt--highlight-weekends-in-date-line date-line (elgantt--get-range-of-years))

    (dolist (properties (elgantt--get-property-list-for-header header))
      (when (plist-get properties :deadline)
	(setq date-line 
	      (elgantt--insert-char-at-index date-line
						    (elgantt--convert-date-to-column-number
						     (plist-get properties :deadline))
						    elgantt-deadline-character))
	;;this appears to be working
	(plist-put properties :calendar-point (+ (* elgantt/variables/number-of-lines elgantt/variables/line-length)
	 					 (elgantt--convert-date-to-column-number (plist-get properties :deadline))
						 elgantt/variables/number-of-lines 1)))
      
      (when (plist-get properties :timestamp-active)
	(setq date-line 
	      (elgantt--insert-char-at-index date-line
						    (elgantt--convert-date-to-column-number
						     (plist-get properties :timestamp-active))
						    elgantt-active-timestamp-character))

	(plist-put properties :calendar-point (+ (* elgantt/variables/number-of-lines elgantt/variables/line-length)
	 					 (elgantt--convert-date-to-column-number (plist-get properties :timestamp-active))
						 elgantt/variables/number-of-lines 1))))

    (if (= 0 (% elgantt/variables/number-of-lines 2))
	(put-text-property 0 (length date-line) 'font-lock-face `(:background ,(color-lighten-name (face-attribute 'default :background) elgantt-brightness-adjust)) date-line)
      (put-text-property 0 (length date-line) 'font-lock-face `(:background ,(face-attribute 'default :background)) date-line))
    
    (dolist (bloke (elgantt--build-block-list header))
      (let ((color-start elgantt-default-block-start-color)
	    (color-end "#ff1c1c"))
	(when (alist-get (car bloke) elgantt-custom-colors)
    	  (setq color-end (alist-get (car bloke) elgantt-custom-colors)))
    	(elgantt--change-gradient-of-substring color-start color-end date-line
    							     (elgantt--convert-date-to-column-number (cadr bloke))
    							     (+ (elgantt--convert-date-to-column-number (cadr (cdr bloke))) 1))))
    date-line))

;;colorizers
(defun elgantt--get-background-of-point (point)
  "give it a point in the buffer, and it returns the background color of it"
  (plist-get (get-text-property point 'face) :background))

(defun elgantt--get-background-of-string (s index)
  "give it a point in the buffer, and it returns the background color of it"
  (plist-get (get-text-property index 'face s) :background))

(defun elgantt--change-brightness-of-background-at-point (point change)
  "if there is a background font lock color, this will change its brightness"
  (put-text-property point (1+ point) 'font-lock-face
		     `(:background ,(color-lighten-name
				     (plist-get (get-text-property point 'face) :background) change))))

(defun elgantt--change-brightness-of-string (color change string)
  "returns a string with the background color changed, assuming the background is changed"
  (put-text-property 0 (length string) 'font-lock-face `(:background ,(color-lighten-name color change)) string)
  string)

(defun elgantt--change-gradient-of-substring (start-color end-color string &optional start end)
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

(defun elgantt--draw-gradient (start-color end-color steps)
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

(defun elgantt--build-block-list (header &optional active-time-ranges inactive-time-ranges)
  (when (not active-time-ranges)
    (setq active-time-ranges t))
  (when (not inactive-time-ranges)
    (setq inactive-time-ranges nil))

  (let ((block-list '()))
    (dolist (properties (elgantt--get-property-list-for-header header))
      (when (or (string= (plist-get properties :start-or-end-or-range) "start") (string= (plist-get properties :start-or-end-or-range) "end"))
	(when (not (alist-get (intern (plist-get properties :calendar-label)) block-list))
	  (setf (alist-get (intern (plist-get properties :calendar-label)) block-list) '()))

	(if (not (alist-get (intern (plist-get properties :calendar-label)) block-list))
	    (setf (alist-get (intern (plist-get properties :calendar-label)) block-list) (plist-get properties :calendar-date))
	  (setf (alist-get (intern (plist-get properties :calendar-label)) block-list)
		(sort (list (alist-get (intern (plist-get properties :calendar-label)) block-list) (plist-get properties :calendar-date)) 'string<))))

      (when active-time-ranges
	(when (string= (plist-get properties :start-or-end-or-range) "block")
	  (when (plist-get properties :timestamp-range-active)
	    (setf (alist-get (intern (plist-get properties :calendar-label)) block-list) `(,(car  (plist-get properties :timestamp-range-active))
											   ,(cadr (plist-get properties :timestamp-range-active)))))))

      (when inactive-time-ranges
	(when (string= (plist-get properties :start-or-end-or-range) "block")
	  (when (plist-get properties :timestamp-range-inactive)
	    (setf (alist-get (intern (plist-get properties :calendar-label)) block-list) `(,(car  (plist-get properties :timestamp-range-inactive))
											 ,(cadr (plist-get properties :timestamp-range-inactive))))))))
    block-list))

(defun elgantt--draw-number-line ()
  (let ((number-line ""))
    (dolist (year (elgantt--get-range-of-years))
      (if (elgantt--leap-year-p year)
	  (setq number-line (concat number-line elgantt--leap-year-date-line))
	(setq number-line (concat number-line elgantt--normal-year-date-line))))
    (concat (make-string elgantt--header-column-offset ? ) number-line)))

(defun elgantt--draw-month-line ()
  (let ((calendar-line ""))
    (dolist (year (elgantt--get-range-of-years))
      (if (elgantt--leap-year-p year)
	  (setq calendar-line (concat calendar-line 
				      (replace-regexp-in-string "xxxx" (number-to-string year) 
								elgantt--leap-year-month-line)))
	(setq calendar-line (concat calendar-line
				    (replace-regexp-in-string "xxxx" (number-to-string year) 
							      elgantt--normal-year-month-line)))))
    (concat (make-string elgantt--header-column-offset ? ) calendar-line)))

(defun elgantt--generate-blank-date-line (header)
  "creates a blank dateline of appropriate length (spanning all years and offset by the name column size)"
  (let ((date-line ""))
    (dolist (year (elgantt--get-range-of-years))
      (if (elgantt--leap-year-p year)
	  (setq date-line (concat date-line elgantt--leap-year-blank-line))
	(setq date-line (concat date-line elgantt--normal-year-blank-line))))
    (concat (symbol-name header) (make-string (- elgantt--header-column-offset (length (symbol-name header))) ? ) date-line)))

(defun elgantt--convert-encoded-date (date)
  (let ((new-date (decode-time date)))
    (concat (number-to-string (nth 5 new-date)) "-" (number-to-string (nth 4 new-date)) "-" (number-to-string (nth 3 new-date)))))

;;depricated; use insert-string-st-index
(defun elgantt--insert-char-at-index (string index char)
  "insert a character at index (int) of given string)
                                               return the new string"
  (concat (cl-subseq string 0 index) char (cl-subseq string (+ index 1))))

(defun elgantt--insert-string-at-index (source-string index replacement)
  (concat (substring source-string 0 index) replacement (substring source-string (length replacement))))


;; GETTING INFORMATION FROM THE CALENDAR AT POINT

(defun elgantt--show-echo-message ()
  (interactive)
  (message "%s -- %s -- %s"
	   (elgantt--get-date-at-point)
	   (elgantt--get-header-at-point)
	   (elgantt--get-task-at-point)))

(defun elgantt--get-task-at-point ()
  (interactive)
  (let ((task ""))
    (when (alist-get (intern (elgantt--get-header-at-point)) elgantt--map-data)
      (dolist (property (alist-get (intern (elgantt--get-header-at-point)) elgantt--map-data))
	(when (plist-get property :calendar-point)
	  (when (= (plist-get property :calendar-point) (point))
	    (setq task (plist-get property :headline-text))))))
    task))

(defun elgantt--get-date-at-point (&optional column)
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

(defun elgantt--get-header-at-point ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (if (re-search-forward "[^|]+" nil t)
	(setq header (s-trim (match-string 0)))
      (setq header ""))))

(defun elgantt--open-org-file-at-point ()
  (interactive)
  (when (alist-get (intern (elgantt--get-header-at-point)) elgantt--map-data)
    (dolist (property (alist-get (intern (elgantt--get-header-at-point)) elgantt--map-data))
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
	 `(,(nth 4 (parse-time-string (elgantt--get-date-at-point)))
	   ,(nth 3 (parse-time-string (elgantt--get-date-at-point)))
	   ,(nth 5 (parse-time-string (elgantt--get-date-at-point))))))
    (org-agenda-list nil (calendar-absolute-from-gregorian date) 'day))
  (other-window 1))


(defun elgantt/display/days-in-year (year)
  (if (elgantt--leap-year-p year)
      366
    365))

(defun elgantt/display/convert-date-string-to-day-number-in-year (date)
  "accept a date in the format YYYY-MM-DD and return an int of #day of the year"
  (time-to-day-in-year (encode-time 0 0 0 (string-to-number (substring date 8 10))
				    (string-to-number (substring date 5 7))
				    (string-to-number (substring date 0 4)))))

(defun elgantt--convert-date-to-column-in-current-year (date)
  "accepts a date YYYY-MM-DD and returns the position on the horizontal calendar (int)
                       this works on leap years"
  (+ (elgantt/display/convert-date-string-to-day-number-in-year date)
     (- (string-to-number (substring date 5 7)) 1)))


(defun elgantt--get-total-days ()
  (let ((total-days 0))
    (dolist (year (elgantt--get-range-of-years))
      (if (elgantt--leap-year-p year)
	  (setq total-days (+ total-days 366))
	(setq total-days (+ total-days 365))))
    total-days))

(defun elgantt--calculate-days-between-dates (d1 d2)
  "return the number of days between two dates
                                   accepts: two dates in the format '(month day year) [i.e., d1 and d2]
                                   returns an int of the number of days"
  (let* ((days (- (calendar-absolute-from-gregorian d1)
		  (calendar-absolute-from-gregorian d2)))
	 (days (1+ (if (> days 0) days (- days)))))
    days))

(defun elgantt--leap-year-p (year)
  (if (= (% year 4) 0)
      t
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEALING WITH THE BUFFER / COLORIZERS

(defun elgantt--highlight-current-day ()
  (interactive)
  (save-excursion 
    (goto-char (point-min))
    (let ((date-line (+ (elgantt--convert-date-to-column-number (format-time-string "%Y-%m-%d"))))
	  (x 1)
	  (total-lines (elgantt--count-lines-in-buffer)))
      (while (< x total-lines)
	(move-beginning-of-line 1)
	(forward-char date-line)
	(elgantt--change-brightness-of-background-at-point (point) -15)
	(forward-line)
	(setq x (1+ x))))
    (goto-char (point-min))))

(defun elgantt--get-background-of-point ()
  "give it a point in the buffer, and it returns the background color of it"
  (interactive)
  (message (plist-get (get-text-property (point) 'face) :background))
  (plist-get (get-text-property (point) 'face) :background))

(defun elgantt--change-brightness-of-background-at-point (point change)
  "if there is a background font lock color, this will change its brightness"
  (when (get-text-property point 'face)
    (put-text-property point (1+ point) 'font-lock-face
		       `(:background ,(color-lighten-name
				       (plist-get (get-text-property point 'face) :background) change)))))

(defun elgantt--change-brightness-of-string (color change string)
  "returns a string with the background color changed"
  (put-text-property 0 (length string) 'font-lock-face `(:background ,(color-lighten-name color change)) string))

(defun elgantt--draw-gradient (start-color end-color steps)
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

(defun elgantt--count-lines-in-buffer ()
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

(defun elgantt-move-forward ()
  (interactive)
  (forward-word)
  (backward-char)
  (elgantt/display/get-echo-message))

(defun elgantt-move-backward ()
  (backward-word))

(defun elgantt-move-up ()
  (interactive)
  (elgantt-move-line -1))

(defun elgantt-move-down ()
  (interactive)
  (elgantt-move-line 1))


(defun elgantt-move-line (direction)
  "direction == 1 means move down
   direction == -1 means move up"
  (interactive)
  (forward-line direction)
  (when (not (thing-at-point 'word))
    (cond ((< (re-search-forward "[[:alpha:]]") (re-search-backward "[[:alpha:]]"))
	   (elgantt-move-forward))
	  ((> (re-search-forward "[[:alpha:]]") (re-search-backward "[[:alpha:]]"))
	   (elgantt-move-backward)))))

(setq elgantt--old-horizontal-highlight '())

(defun elgantt--horizontal-highlight ()
  (interactive)
  (let ((inhibit-read-only t))
    (dolist (p elgantt--old-horizontal-highlight)
      (when (cadr p)
	(put-text-property (car p) (1+ (car p)) 'font-lock-face `(:background ,(cadr p))))))
  (setq elgantt--old-horizontal-highlight '())

  (let ((start)
	(end)
	(inhibit-read-only t))
    (save-excursion
      (move-beginning-of-line nil)
      (setq start (point))
      (move-end-of-line nil)
      (setq end (current-column))
      (dotimes (x end)
	(add-to-list 'elgantt--old-horizontal-highlight `(,(+ start x) ,(plist-get (get-text-property (+ x start) 'font-lock-face) :background)))
	(elgantt--change-brightness-of-background-at-point (+ start x) elgantt-brightness-adjust)))))
    
(defun elgantt--vertical-highlight (&optional column)
  "insert a vertical highlight bar at column, and remove the previous selection bar"
  (interactive)
  (when (not column)
    (setq column (current-column)))

  (let ((inhibit-read-only t))
    (dolist (p elgantt--old-backgrounds)
      (when (cadr p)
	(put-text-property (car p) (1+ (car p)) 'font-lock-face `(:background ,(cadr p))))))
  (setq elgantt/display/old-background-props '())

  (save-excursion
    (goto-char (point-min))
    (let ((x 1)
	  (inhibit-read-only t))
      (while (< x (elgantt--count-lines-in-buffer))
	(move-beginning-of-line 1)
	(forward-char column)
	(add-to-list 'elgantt--old-backgrounds `(,(point) ,(plist-get (get-text-property (point) 'font-lock-face) :background)))
	(elgantt--change-brightness-of-background-at-point (point) -35)
	(forward-line)
	(setq x (1+ x))))))

(defun elgantt--move-selection-bar-forward ()
  (interactive)
  (when (<= (current-column) elgantt--header-column-offset)
    (forward-char elgantt--header-column-offset))
  (forward-char)
  (goto-char (1- (re-search-forward "[^| ]" nil t)))
  (elgantt--vertical-highlight (current-column)))

(defun elgantt--move-selection-bar-forward ()
  (interactive)
  (goto-char (re-search-backward "[^| ]" nil t))
  (when (< (current-column) elgantt--header-column-offset)
    (move-beginning-of-line nil)
    (forward-char elgantt--header-column-offset))                               
  (elgantt--vertical-highlight (current-column)))    


;;;; Testing

;;this seems to work
(defun elgantt-hide-past-dates ()
  "Shows only the previous month and hides all others."
  (interactive)
  (save-excursion 
    (let ((inhibit-read-only t)
	   (year (format-time-string "%Y"))
	   (month (1- (string-to-number (format-time-string "%m")))))
      (if (< month 10)
	  (setq month (concat "0" (number-to-string month)))
	(setq month (number-to-string month)))
      (let ((start-column (1+ elgantt--header-column-offset))
	    (end-column (elgantt--convert-date-to-column-number (concat year "-" month "-01"))))
	(goto-char (point-min))
	(dotimes (x (elgantt--count-lines-in-buffer))
	  (let ((start (save-excursion (move-to-column start-column) (point)))
		(end (save-excursion (move-to-column end-column) (point))))
	    (put-text-property start end 'invisible t)
	    (next-line)))))))

(defun elgantt--hide-future-dates ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
	  (start-column (elgantt--convert-date-to-column-number (elgantt--get-oldest-date))))
      (goto-char (point-min))
      (dotimes (x (elgantt--count-lines-in-buffer))
	(let ((start (1+ (save-excursion (move-to-column start-column) (point))))
	      (end (save-excursion (end-of-line) (point))))
	  (put-text-property start end 'invisible t)
	  (next-line))))))
  
(defun elgantt-show-all-dates ()
  (interactive)
  (let ((inhibit-read-only t))
    (put-text-property 1 (point-max) 'invisible nil)))

;;;; Footer

(provide 'elgantt)

;;; elgantt.el ends here
