;;; org-chronos.el --- Org-based time management -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (dash "2.18") (org-ql "0.6") (ts "0.2") (org "9.4"))
;; Keywords: outlines hypermedia
;; URL: https://github.com/akirak/org-chronos

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME

;;; Code:

(require 'dash)
(require 'org-ql)
(require 'org-element)
(require 'org-clock)
(require 'eieio)

(require 'org-chronos-utils)

(defgroup org-chronos
  nil
  "A time-tracking framework for Org."
  :group 'org
  :group 'org-clock)

;;;; Constants

(defconst org-chronos-json-version "0.1.1")

;;;; Structs

(cl-defstruct org-chronos-clock-range
  "Structure that represents a certain period of time."
  ;; Start time of the range as ts
  start
  ;; End time of the range as ts
  end
  ;; Duration string as in Org, i.e. HH:MM
  duration-string
  ;; Duration in minutes as a number
  duration-minutes)

(cl-defstruct org-chronos-log-note
  type timestamp attributes comment)

(cl-defstruct org-chronos-heading-element
  "Structure that holds information on a heading with clock entries."
  marker olp tags category properties todo-state link log-notes clock-entries
  closed created)

;;;; Custom variables

(defcustom org-chronos-logged-properties nil
  "List of properties that should be recorded."
  :group 'org-chronos
  :type '(repeat (choice string
                         (cons string plist))))

(defcustom org-chronos-show-property-summary t
  "Whether to produce summary tables for `org-chronos-logged-properties'."
  :group 'org-chronos
  :type 'boolean)

(defcustom org-chronos-log-dblock-defaults
  (list :span 'day :files #'org-agenda-files
        :sections "groups,property-groups,entries,closed,created")
  "Default parameters of the Org dynamic block."
  :group 'org-chronos
  :type 'plist)

(defcustom org-chronos-scan-containing-file nil
  "Whether to include the file that contains the dblock.

When this variable is non-nil, the dynamic block adds the
containing file to the source files."
  :group 'org-chronos
  :type 'boolean)

(defcustom org-chronos-annotate-links t
  "Whether to decorate headline names with links.

Note that a link is produced on every entry having clock
entries. This may lead to generating IDs if you have turned on
`org-id-link-to-org-use-id'.

The value can also be a function that takes no argument and
returns a boolean value."
  :group 'org-chronos
  :type '(choice boolean function))

(defcustom org-chronos-trim-headline 25
  "Maximimal length of headlines in Org dynamic block output."
  :group 'org-chronos
  :type '(choice number null))

(defcustom org-chronos-tag-groups nil
  "List of tags used to group headings."
  :group 'org-chronos
  :type '(repeat string))

(defcustom org-chronos-ignored-categories nil
  "List of categories that are excluded from the input data.

When this variable is set to a list of Org categories, items that
  belong to one of the categories are excluded from statistics
  and the output."
  :group 'org-chronos
  :type '(repeat string))

(defcustom org-chronos-clock-threshold nil
  "Threshold of duration to display an item in blocks."
  :group 'org-chronos
  :type '(choice null number))

(defcustom org-chronos-duration-format 'h:mm
  "Custom format used in this package.

If this variable is non-nil, it is used as the format for
converting durations to strings in this package.

If it is nil, the default value is used.

See `org-duration-format' for possible values of this variable."
  :group 'org-chronos
  :type 'sexp)

(defcustom org-chronos-auto-export nil
  "Whether to export the log data on every evaluation."
  :group 'org-chronos
  :type 'boolean)

(defcustom org-chronos-export-root-directory nil
  "Destination directory of exporting.

Files are saved to subdirectories of this directory based on the
time span."
  :group 'org-chronos
  :type 'directory)

(defcustom org-chronos-creation-time-property nil
  "Name of an Org property used to track creation time of an entry.

Values of the property must be an inactive timestamp."
  :group 'org-chronos
  :type '(choice string null))

(defcustom org-chronos-reduce-nested-entries t
  "Whether to exclude nested entries from certain views in the report."
  :group 'org-chronos
  :type 'boolean)

;;;; Log object

(defclass org-chronos-log ()
  ((span :initarg :span
         :type (or null (member day week month)))
   (snapshot-time :initarg :snapshot-time
                  :type ts)
   (start :initarg :start
          :type ts)
   (end :initarg :end
        :type (or null ts))
   (files :initarg :files
          :type list)
   (data :type list)))

;;;; Parsing

(defsubst org-chronos--closed-clock-p (clock)
  "Return t is CLOCK is closed."
  (and (eq 'clock (org-element-type clock))
       (eq 'closed (org-element-property :status clock))))

(defun org-chronos--timestamp-start-ts (timestamp)
  "Return the start time of TIMESTAMP as a ts object."
  (ts-fill
   (make-ts :year (org-element-property :year-start timestamp)
            :month (org-element-property :month-start timestamp)
            :day (org-element-property :day-start timestamp)
            :hour (org-element-property :hour-start timestamp)
            :minute (org-element-property :minute-start timestamp)
            :second 0)))

(defun org-chronos--timestamp-end-ts (timestamp)
  "Return the end time of TIMESTAMP as a ts object."
  (ts-fill
   (make-ts :year (org-element-property :year-end timestamp)
            :month (org-element-property :month-end timestamp)
            :day (org-element-property :day-end timestamp)
            :hour (org-element-property :hour-end timestamp)
            :minute (org-element-property :minute-end timestamp)
            :second 0)))

(defun org-chronos--parse-clock-line ()
  "Return `org-chronos-clock-range' from the current clock line."
  (let ((clock (org-element-clock-parser (line-end-position))))
    (when (org-chronos--closed-clock-p clock)
      (let ((timestamp (org-element-property :value clock))
            (duration-string (org-element-property :duration clock)))
        (make-org-chronos-clock-range
         :start (org-chronos--timestamp-start-ts timestamp)
         :end (org-chronos--timestamp-end-ts timestamp)
         :duration-string duration-string
         :duration-minutes (org-duration-to-minutes duration-string))))))

(defmacro org-chronos--with-logbook (&rest progn)
  "Evaluate PROGN with the buffer narrowed to the logbook, if any."
  `(save-excursion
     (org-back-to-heading)
     (end-of-line 1)
     (let* ((content-end (org-entry-end-position))
            (logbook-end (save-excursion
                           (re-search-forward org-logbook-drawer-re
                                              content-end t))))
       (when logbook-end
         (goto-char (car (match-data)))
         (save-restriction
           (narrow-to-region (point) logbook-end)
           ,@progn)))))

(defun org-chronos--entry-creation-time ()
  "Return the creation time of the entry.

This looks up the value of `org-chronos-creation-time-property'
if set, and converts it to a ts value."
  (when org-chronos-creation-time-property
    (-some->> (org-entry-get nil org-chronos-creation-time-property)
      (ts-parse-org)
      (ts-update))))

(defun org-chronos--clock-entries-on-heading ()
  "Return clock entries on the current Org heading after the point."
  (org-chronos--with-logbook
   (let (entries)
     (while (re-search-forward org-clock-line-re nil t)
       (when-let (range (org-chronos--parse-clock-line))
         (push range entries)))
     entries)))

(defun org-chronos--parse-logbook ()
  "Return clock entries on the current Org heading after the point."
  (org-chronos--with-logbook
   (condition-case err
       (let (clock-entries
             log-notes)
         (catch 'finish
           (while (not (eobp))
             (cond
              ((looking-at org-clock-line-re)
               (when-let (range (org-chronos--parse-clock-line))
                 (push range clock-entries))
               (forward-line))
              ((looking-at (rx (* space) "- "))
               (push (org-chronos--parse-log-note-heading)
                     log-notes))
              ((looking-at org-clock-drawer-end-re)
               (throw 'finish t))
              (t
               (beginning-of-line 2)))))
         (list :clock-entries clock-entries
               :log-notes log-notes))
     (error (error "Error while parsing logbook at %d in %s: %s"
                   (point)
                   (buffer-file-name)
                   err)))))

(defun org-chronos--log-note-heading-to-re (pattern)
  "Convert a log note pattern to a regular expression.

PATTERN should be the cdr of one of the items in
`org-log-note-headings'."
  (--> pattern
       (replace-regexp-in-string
        (rx "%" (?  "-") (* (any digit)) (any "td"))
        (replace-regexp-in-string
         "\\\\" "\\\\\\\\"
         (org-re-timestamp 'inactive))
        it t)
       (replace-regexp-in-string
        (rx "%" (?  "-") (* (any digit)) (any "TD"))
        (replace-regexp-in-string
         "\\\\" "\\\\\\\\"
         (org-re-timestamp 'active))
        it t)
       (replace-regexp-in-string
        (rx "%" (?  "-") (* (any digit)) (any "sS"))
        (replace-regexp-in-string
         "\\\\" "\\\\\\\\"
         (rx (* space)
             (?  "\""
                 (regexp org-todo-regexp)
                 ;; %s and %S can be a timestamp as well.
                 ;; I haven't found actual use of such a pattern,
                 ;; but I have to support it in the future.
                 ;;
                 ;; (or (regexp org-todo-regexp)
                 ;;     (regexp (org-re-timestamp 'inactive)))
                 "\"")
             (* space)))
        it t)
       (concat (rx (* space) "- ") it)))

(defun org-chronos--parse-log-note-heading ()
  "Parse a log note heading in the buffer.

This function parses a log note at point and returns an object of
`org-chronos-log-note' type."
  (catch 'finish
    (pcase-dolist (`(,type . ,pattern) org-log-note-headings)
      (unless (string-empty-p pattern)
        (let ((re (org-chronos--log-note-heading-to-re pattern)))
          (when (looking-at re)
            (goto-char (nth 1 (match-data)))
            (let* ((placeholders (s-match-strings-all
                                  (rx "%" (?  "-") (* (any digit))
                                      ;; TODO: Add %u and %U
                                      (group (any "sStTdD")))
                                  pattern))
                   (match-strings (-map (pcase-lambda (`(,begin ,end))
                                          (when (and begin end)
                                            (buffer-substring-no-properties begin end)))
                                        (-partition-all 2 (match-data))))
                   (note-data (org-chronos--analyse-log-note-data
                               placeholders
                               (cdr match-strings)))
                   (timestamp-ts (org-chronos--ts-from-decoded-time
                                  (org-parse-time-string
                                   (alist-get 'timestamp note-data))))
                   (comment-start (when (looking-at (rx (* space) "\\\\"))
                                    (re-search-forward (rx (* space) "\\\\\n" (* space)))))
                   (comment (if comment-start
                                (progn
                                  (re-search-forward (rx (or (regexp org-clock-line-re)
                                                             (and bol (* space) "- ")
                                                             (regexp org-clock-drawer-end-re)))
                                                     nil t)
                                  (beginning-of-line 1)
                                  (buffer-substring-no-properties comment-start (1- (point))))
                              (beginning-of-line 2)
                              nil)))
              (throw 'finish
                     (make-org-chronos-log-note
                      :type type
                      :timestamp timestamp-ts
                      :attributes note-data
                      :comment comment)))))))))

(defun org-chronos--analyse-log-note-data (placeholders match-strings)
  "Given the parsing result, return an alist of data.

Both PLACEHOLDERS and MATCH-STRINGS must be a list that have an
equal number of items."
  (->> (-zip (--map (nth 1 it) placeholders) match-strings)
       (-map (pcase-lambda (`(,c . ,s))
               (cl-labels
                   ((parse-todo
                     (raw)
                     (save-match-data
                       (when (and raw
                                  (string-match
                                   (rx bol "\"" (group (+ anything)) "\"" eol)
                                   raw))
                         (match-string 1 raw)))))
                 (pcase c
                   ("t" (cons 'timestamp s))
                   ("T" (cons 'timestamp s))
                   ("d" (cons 'timestamp s))
                   ("D" (cons 'timestamp s))
                   ("s" (cons 'old-todo (parse-todo s)))
                   ("S" (cons 'new-todo (parse-todo s)))))))))

(defun org-chronos--search-headings-with-clock (files from to)
  "Search headings with clock entries in a given time range.

FIXME: FILES, FROM, and TO."
  (let (result)
    (dolist (file files)
      (with-current-buffer (or (find-buffer-visiting file)
                               (find-file-noselect file))
        (org-save-outline-visibility t
          (org-with-wide-buffer
           (org-show-all)
           (push (org-ql-select (current-buffer)
                   `(ts-inactive :from ,from :to ,to)
                   :action
                   `(let* ((closed (progn
                                     ;; We are at the headline at the beginning,
                                     ;; so move on to the next line.
                                     (beginning-of-line 2)
                                     (when (looking-at org-closed-time-regexp)
                                       (re-search-forward org-ts-regexp-inactive)
                                       (ts-parse-org (match-string 0)))))
                           ;; Ignore values that are not within the range.
                           (closed (when (and closed
                                              (ts-in ,from ,to closed))
                                     closed))
                           (logbook (org-chronos--parse-logbook))
                           (clock-entries (-filter (lambda (x)
                                                     (ts-in ,from ,to (org-chronos-clock-range-start x)))
                                                   (plist-get logbook :clock-entries)))
                           (created (org-chronos--entry-creation-time))
                           ;; Ignore values that are not within the range.
                           (created (when (and created
                                               (ts-in ,from ,to created))
                                      created))
                           (log-notes (-filter (lambda (x)
                                                 (ts-in ,from ,to (org-chronos-log-note-timestamp x)))
                                               (plist-get logbook :log-notes))))
                      (when (or clock-entries log-notes created closed)
                        (make-org-chronos-heading-element
                         :marker (point-marker)
                         :link (when (if (functionp org-chronos-annotate-links)
                                         (funcall org-chronos-annotate-links)
                                       org-chronos-annotate-links)
                                 (save-excursion
                                   (org-store-link nil 'interactive)
                                   (pop org-stored-links)))
                         :closed closed
                         :created created
                         :olp (org-get-outline-path t t)
                         :tags (org-get-tags)
                         :category (org-get-category)
                         :properties (org-chronos--collect-properties)
                         :todo-state (org-get-todo-state)
                         :log-notes log-notes
                         :clock-entries clock-entries))))
                 result)))))
    (->> (-flatten-n 1 (nreverse result))
         (-filter #'org-chronos--meaningful-element-p))))

(defun org-chronos--collect-properties ()
  "Return an alist of property values specified in `org-chronos-logged-properties'."
  (->> org-chronos-logged-properties
    (-map (lambda (x)
            (pcase x
              ((pred stringp)
               (cons x (org-entry-get nil x)))
              (`(,name . ,plist)
               (cons name (org-entry-get nil name
                                         (plist-get plist :inherit)
                                         (plist-get plist :literal-nil)))))))
    (-filter #'cdr)))

(defun org-chronos--meaningful-element-p (element)
  "Check if ELEMENT will be meaningful in the report."
  ;; Since clocks may not be contained in the heading but in
  ;; children, you have to exclude headings without clock entries
  ;; during the period.
  (and (org-chronos-heading-element-p element)
       (not (member (org-chronos-heading-element-category element)
                    org-chronos-ignored-categories))))

;;;; Generic functions

(cl-defgeneric org-chronos--sum-minutes (x)
  "Return the clock sum on X in minutes.")

(cl-defmethod org-chronos--sum-minutes ((x org-chronos-clock-range))
  "Return the clock sum on X in minutes."
  (org-chronos-clock-range-duration-minutes x))

(cl-defmethod org-chronos--sum-minutes ((x org-chronos-heading-element))
  "Return the clock sum on X in minutes."
  (->> (org-chronos-heading-element-clock-entries x)
       (-map #'org-chronos--sum-minutes)
       (-sum)))

;;;; Grouping

(defun org-chronos--group-elements-by-tag (elements)
  "Group ELEMENTS by tags defined in `org-chronos-tag-groups'."
  (let ((entries-1 (copy-sequence elements))
        result)
    (dolist (tag org-chronos-tag-groups)
      (-let (((group-entries rest) (--separate
                                    (-contains-p (org-chronos-heading-element-tags it)
                                                 tag)
                                    entries-1)))
        (push (cons tag group-entries) result)
        (setq entries-1 rest)))
    (when entries-1
      (push (cons nil entries-1) result))
    (nreverse result)))

(defun org-chronos--group-elements-by-category (elements)
  "Group ELEMENTS by categories."
  (-group-by #'org-chronos-heading-element-category elements))

(defun org-chronos--group-elements-by-property (property elements)
  "Group elements by a property.

This uses the values of PROPERTY to group ELEMENTS."
  (->> elements
       (-group-by (lambda (x)
                    (->> (org-chronos-heading-element-properties x)
                         (assoc property)
                         (cdr))))
       ;; Ignore nil groups
       (-filter #'car)))

;;;; Org output functions

(cl-defun org-chronos--write-elements-as-org-table (elements
                                                    &key grouped
                                                    range-format
                                                    todo-state
                                                    show-total)
  "Insert elements to the buffer as an Org table.

ELEMENTS is a list of `org-chronos-heading-element'.

GROUPED, RANGE-FORMAT, TODO-STATE, and SHOW-TOTAL specifies
output options."
  (let* ((columns (-non-nil (list (when grouped '(group "Group"))
                                  '(name "Task")
                                  '(duration "Duration")
                                  (when range-format '(start "Start"))
                                  (when range-format '(end "End"))
                                  (when todo-state '(todo "State")))))
         (hline (concat "|-" (string-join (-repeat (1- (length columns)) "-+-"))
                        "-|\n"))
         (total 0))
    ;; Header
    (insert "| " (mapconcat (-partial #'nth 1) columns " | ") "\n")
    (insert hline)
    ;; Body
    (cl-labels
        ((write-element-row
          (x &optional group)
          (insert "| "
                  (mapconcat (lambda (column)
                               (pcase (car column)
                                 ('group
                                  group)
                                 ('name
                                  (if-let (link (org-chronos-heading-element-link x))
                                      (org-link-make-string (car link)
                                                            (org-chronos--trim-string-at-length
                                                             (org-chronos--escape-link-description
                                                              (nth 1 link))
                                                             org-chronos-trim-headline))
                                    (org-chronos--trim-string-at-length
                                     (org-chronos--escape-link-description
                                      (-last-item (org-chronos-heading-element-olp x)))
                                     org-chronos-trim-headline)))
                                 ('duration
                                  (org-duration-from-minutes (org-chronos--sum-minutes x)
                                                             org-chronos-duration-format))
                                 ('start
                                  (ts-format range-format
                                             (org-chronos--start-of-clocks
                                              (org-chronos-heading-element-clock-entries x))))
                                 ('end
                                  (ts-format range-format
                                             (org-chronos--end-of-clocks
                                              (org-chronos-heading-element-clock-entries x))))
                                 ('todo
                                  (or (org-chronos-heading-element-todo-state x)
                                      ""))))
                             columns
                             " | ")
                  " |\n")
          (cl-incf total (org-chronos--sum-minutes x))))
      (if grouped
          (pcase-dolist (`(,group . ,group-elements) elements)
            (dolist (x group-elements)
              (write-element-row x group)))
        (dolist (x elements)
          (write-element-row x))))
    ;; Footer (optional)
    (when show-total
      (insert hline)
      (let ((n (-find-index (lambda (column) (eq 'duration (car column))) columns)))
        (insert "| "
                (string-join (-repeat (1- n) " | "))
                " *Total* | "
                (org-duration-from-minutes total org-chronos-duration-format)
                (string-join (-repeat (1- (- (length columns) n)) " | "))
                " |\n")))
    (delete-char -1)
    (org-table-align)))

(defun org-chronos--trim-string-at-length (string max-length)
  "Trim STRING at MAX-LENGTH."
  (if (and max-length
           (> (length string) max-length))
      (concat (substring string 0 (- max-length 4)) "...")
    string))

(defsubst org-chronos--org-table-hline (columns)
  "Return a separator for a table with COLUMNS."
  (concat "|-"
          (apply #'concat (-repeat (cl-etypecase columns
                                     (list (length columns))
                                     (number columns))
                                   "-+-"))
          "-|\n"))

(defsubst org-chronos--write-org-table-row (cells)
  "Insert CELLS to the buffer as an Org table row."
  (declare (indent 0))
  (insert "| " (string-join cells " | ") " |\n"))

(cl-defun org-chronos--write-group-sums-as-org-table (groups group-type
                                                             &key show-percents)
  "Insert groups to the buffer as an Org table.

FIXME: GROUPS, GROUP-TYPE, and SHOW-PERCENTS."
  (let* ((columns (-non-nil (list group-type
                                  'duration
                                  (when show-percents
                                    'percent))))
         (hline (org-chronos--org-table-hline columns))
         (groups-with-sums (->> (-map (pcase-lambda (`(,group . ,elements))
                                        (list group
                                              elements
                                              (-sum (-map #'org-chronos--sum-minutes elements))))
                                      groups)
                                (-sort (-on #'> (-partial #'nth 2)))))
         (total (->> groups-with-sums
                     (--map (nth 2 it))
                     (-sum))))
    ;; header
    (org-chronos--write-org-table-row
      (-map (lambda (column)
              (cl-case column
                (tag "Tag")
                (category "Category")
                (duration "Sum")
                (percent "%")
                (otherwise (format "%s" column))))
            columns))
    (insert hline)
    ;; body
    (pcase-dolist (`(,group ,_elements ,sum) groups-with-sums)
      (when (> sum 0)
        (org-chronos--write-org-table-row
          (-map (lambda (column)
                  (cl-case column
                    (duration (org-duration-from-minutes sum org-chronos-duration-format))
                    (percent (format "%.0f %%" (/ (* 100 sum) total)))
                    (otherwise (or group "-"))))
                columns))))
    ;; footer
    (insert hline)
    (org-chronos--write-org-table-row
      (-map (lambda (column)
              (cl-case column
                (duration (org-duration-from-minutes total org-chronos-duration-format))
                (percent "100 %")
                (otherwise "*Total*")))
            columns))
    (delete-char -1)
    (org-table-align)))

(cl-defgeneric org-chronos--write-org (obj)
  "Write OBJ as Org into the buffer.")
(cl-defgeneric org-chronos--write-org-null-p (obj)
  "Return non-nil if OBJ has no entries.")

(cl-defstruct org-chronos-composite-view
  views)

(cl-defmethod org-chronos--write-org ((obj org-chronos-composite-view))
  "Write OBJ as Org into the buffer."
  (let (not-first)
    (dolist (view (org-chronos-composite-view-views obj))
      (unless (org-chronos--write-org-null-p view)
        (when not-first
          (insert "\n\n"))
        (org-chronos--write-org view)
        (setq not-first t)))))

(cl-defmethod org-chronos--write-org-null-p ((obj org-chronos-composite-view))
  "Return non-nil if OBJ has no entries."
  (-all-p #'org-chronos--write-org-null-p
          (org-chronos-composite-view-views obj)))

(cl-defstruct org-chronos-entry-view
  items-or-groups grouped time-format todo-state show-total)

(defun org-chronos-entry-view-clocked-items-or-groups (x)
  "Return items that have clock entries in X."
  (if (org-chronos-entry-view-grouped x)
      (->> (org-chronos-entry-view-items-or-groups x)
           (-map (pcase-lambda (`(,group . ,items))
                   (cons group
                         (-filter #'org-chronos-heading-element-clock-entries items))))
           (-filter #'cdr))
    (->> (org-chronos-entry-view-items-or-groups x)
         (-filter #'org-chronos-heading-element-clock-entries))))

(cl-defmethod org-chronos--write-org ((obj org-chronos-entry-view))
  "Write OBJ as Org into the buffer."
  (org-chronos--write-elements-as-org-table
   (org-chronos-entry-view-clocked-items-or-groups obj)
   :grouped (org-chronos-entry-view-grouped obj)
   :range-format (org-chronos-entry-view-time-format obj)
   :todo-state (org-chronos-entry-view-todo-state obj)
   :show-total (org-chronos-entry-view-show-total obj)))

(cl-defmethod org-chronos--write-org-null-p ((obj org-chronos-entry-view))
  "Return non-nil if OBJ has no entries."
  (null (org-chronos-entry-view-clocked-items-or-groups obj)))

(cl-defstruct org-chronos-closed-items-view items-or-groups grouped time-format)

(defun org-chronos-closed-items-view-closed-items (x)
  "Return items that have clock entries in X."
  (->> (if (org-chronos-closed-items-view-grouped x)
           (->> (org-chronos-closed-items-view-items-or-groups x)
                (-map (pcase-lambda (`(,group . ,items))
                        (--map (cons group it) items)))
                (-flatten-n 1))
         (--map (cons nil it) (org-chronos-closed-items-view-items-or-groups x)))
       (--map (when-let (closed (org-chronos-heading-element-closed (cdr it)))
                (cons closed it)))
       (-non-nil)
       (org-chronos--reduce-nested-entries)))

(cl-defmethod org-chronos--write-org ((obj org-chronos-closed-items-view))
  "Write OBJ as Org into the buffer."
  (let ((time-format (org-chronos-closed-items-view-time-format obj))
        (grouped (org-chronos-closed-items-view-grouped obj)))
    (insert "(Closed items)\n"
            (mapconcat (pcase-lambda (`(,time ,group . ,element))
                         (format "- %s %s%s %s (%s in sum) \\ %s"
                                 (ts-format time-format time)
                                 (org-chronos-heading-element-todo-state element)
                                 (if grouped (concat " " group) (group) "")
                                 (if-let (link (org-chronos-heading-element-link element))
                                     (apply #'org-link-make-string link)
                                   (-last-item (org-chronos-heading-element-olp element)))
                                 (org-with-point-at (org-chronos-heading-element-marker element)
                                   (org-duration-from-minutes (org-clock-sum-current-item)))
                                 (org-format-outline-path
                                  (nreverse (-butlast (org-chronos-heading-element-olp element)))
                                  nil nil " \\ ")))
                       (->> (org-chronos-closed-items-view-closed-items obj)
                         (-sort (-on #'ts< #'car)))
                       "\n"))))

(cl-defmethod org-chronos--write-org-null-p ((obj org-chronos-closed-items-view))
  "Return non-nil if OBJ has no entries."
  (null (org-chronos-closed-items-view-closed-items obj)))

(cl-defstruct org-chronos-created-items-view items-or-groups grouped time-format)

(defun org-chronos-created-items-view-created-items (x)
  "Return items that have clock entries in X."
  (->> (if (org-chronos-created-items-view-grouped x)
           (->> (org-chronos-created-items-view-items-or-groups x)
                (-map (pcase-lambda (`(,group . ,items))
                        (--map (cons group it) items)))
                (-flatten-n 1))
         (--map (cons nil it) (org-chronos-created-items-view-items-or-groups x)))
       (--map (when-let (created (org-chronos-heading-element-created (cdr it)))
                ;; Don't report items that were closed during the period.
                (unless (or (org-chronos-heading-element-closed (cdr it))
                            (member "ARCHIVE" (org-chronos-heading-element-tags (cdr it)))
                            ;; Don't report items in the same buffer as the evaluation context.
                            (equal (current-buffer)
                                   (marker-buffer (org-chronos-heading-element-marker (cdr it)))))
                  (cons created it))))
       (-non-nil)
       (org-chronos--reduce-nested-entries)))

(cl-defmethod org-chronos--write-org ((obj org-chronos-created-items-view))
  "Write OBJ as Org into the buffer."
  (let ((grouped (org-chronos-created-items-view-grouped obj)))
    (insert "(New items)\n"
            (mapconcat (pcase-lambda (`(,_ ,group . ,element))
                         (format "- %s%s \\ %s"
                                 (if (and grouped group) (concat group " ") "")
                                 (if-let (link (org-chronos-heading-element-link element))
                                     (apply #'org-link-make-string link)
                                   (-last-item (org-chronos-heading-element-olp element)))
                                 (org-format-outline-path
                                  (nreverse (-butlast (org-chronos-heading-element-olp element)))
                                  nil
                                  nil
                                  " \\ ")))
                       (->> (org-chronos-created-items-view-created-items obj)
                            (-sort (-on #'ts< #'car)))
                       "\n"))))

(cl-defmethod org-chronos--write-org-null-p ((obj org-chronos-created-items-view))
  "Return non-nil if OBJ has no entries."
  (null (org-chronos-created-items-view-created-items obj)))

(cl-defstruct org-chronos-group-statistic-view
  group-type groups show-percents)

(cl-defmethod org-chronos--write-org ((obj org-chronos-group-statistic-view))
  "Write OBJ as Org into the buffer."
  (org-chronos--write-group-sums-as-org-table
   (org-chronos-group-statistic-view-groups obj)
   (org-chronos-group-statistic-view-group-type obj)
   :show-percents (org-chronos-group-statistic-view-show-percents obj)))

(cl-defmethod org-chronos--write-org-null-p ((obj org-chronos-group-statistic-view))
  "Return non-nil if OBJ has no entries."
  (-all-p (-compose #'null #'cdr) (org-chronos-group-statistic-view-groups obj)))

(defun org-chronos--reduce-nested-entries (items)
  "If necessary, exclude nested entries from ITEMS."
  (if org-chronos-reduce-nested-entries
      (->> (--group-by (nth 1 it) items)
        (-map #'cdr)
        (-map (lambda (entries-in-group)
                (->> (--group-by (marker-buffer
                                  (org-chronos-heading-element-marker (cddr it)))
                                 entries-in-group)
                  (-map #'cdr)
                  (-map #'org-chronos--reduce-by-olp))))
        (-flatten-n 2))
    items))

(defun org-chronos--reduce-by-olp (items)
  "Check olps in ITEMS and eliminate nested entries."
  (let (result)
    (dolist (group-by-depth (->> items
                              (--map (cons (org-chronos-heading-element-olp (cddr it)) it))
                              (-group-by (-compose #'length #'car))
                              (-sort (-on #'< #'car))
                              (-map #'cdr)))
      (dolist (item group-by-depth)
        (unless (--find (-is-prefix-p (car it) (car item)) result)
          (push item result))))
    (-map #'cdr result)))

;;;; Exporting
(cl-defun org-chronos--export-log-to-json (log out-file
                                               &key groups group-type)
  "Export log data to a JSON file.

LOG must be an object of `org-chronos-log' type.

It write the data of the log object to OUT-FILE in JSON.

Optionally, it can take a list of GROUPS and its GROUP-TYPE."
  (cl-check-type log org-chronos-log)
  (with-temp-buffer
    (insert (json-serialize
             (org-chronos--build-object-for-json
              :span (oref log span)
              :start (oref log start)
              :end (oref log end)
              :elements (oref log data)
              :group-type group-type
              :groups groups
              :closed (org-chronos--log-closed-p log)
              :files (oref log files))))
    (write-region (point-min) (point-max) out-file)))

(cl-defun org-chronos--build-object-for-json (&key span start end elements
                                                   group-type groups
                                                   closed
                                                   files)
  "Build an alist that should be converted to json for exporting.

See `org-chronos--export-log-to-json' for the following parameters:
SPAN, START, END, ELEMENTS, GROUP-TYPE, GROUPS, CLOSED, FILES."
  `((version . ,org-chronos-json-version)
    (closed . ,(if closed t :false))
    (source . ((files . ,(apply #'vector files))))
    (range . ((span . ,(symbol-name span))
              (start . ,(ts-format start))
              (end . ,(ts-format end))))
    (meta . ((groups . ,(if groups
                            `((,group-type
                               . ,(apply #'vector
                                         (-map (pcase-lambda (`(,group . ,_elements))
                                                 group)
                                               groups))))
                          :null))
             (properties . ,(if org-chronos-logged-properties
                                (apply #'vector
                                       (-map (lambda (p)
                                               (pcase p
                                                 ((pred stringp) p)
                                                 (`(,name . ,_) name)))
                                             org-chronos-logged-properties))
                              :null))))
    (data . ((headings . ,(apply #'vector
                                 (-map #'org-chronos--json-serializable-object
                                       elements)))))))

(cl-defgeneric org-chronos--json-serializable-object (x)
  "Convert X to an object that can be serialized to JSON.")

(cl-defmethod org-chronos--json-serializable-object ((x org-chronos-heading-element))
  "Convert X to an object that can be serialized to JSON."
  (let ((marker (org-chronos-heading-element-marker x))
        (olp (-map #'org-link-display-format (org-chronos-heading-element-olp x)))
        (tags (org-chronos-heading-element-tags x))
        (category (org-chronos-heading-element-category x))
        (properties (org-chronos-heading-element-properties x))
        (todo-state (org-chronos-heading-element-todo-state x))
        (link (org-chronos-heading-element-link x))
        (log-notes (org-chronos-heading-element-log-notes x))
        (closed (org-chronos-heading-element-closed x))
        (created (org-chronos-heading-element-created x))
        (clock-entries (org-chronos-heading-element-clock-entries x)))
    `((buffer . ,(buffer-name (marker-buffer marker)))
      (position . ,(marker-position marker))
      (title . ,(-last-item olp))
      (outline . ,(apply #'vector olp))
      (tags . ,(apply #'vector tags))
      (properties . ,(-map (pcase-lambda (`(,key . ,value))
                             (cons (intern key) value))
                           properties))
      (link . ,(car link))
      (category . ,(or category :null))
      (todo . ,(or todo-state :null))
      (closed . ,(if closed (ts-format closed) :null))
      (created . ,(if created (ts-format created) :null))
      (log-notes . ,(apply #'vector
                           (-map #'org-chronos--json-serializable-object
                                 log-notes)))
      (clocks . ,(apply #'vector
                        (-map #'org-chronos--json-serializable-object
                              clock-entries))))))

(cl-defmethod org-chronos--json-serializable-object ((x org-chronos-log-note))
  "Convert X to an object that can be serialized to JSON."
  `((time . ,(ts-format (org-chronos-log-note-timestamp x)))
    (type . ,(symbol-name (org-chronos-log-note-type x)))
    (attributes . ,(org-chronos-log-note-attributes x))
    (comment . ,(or (org-chronos-log-note-comment x) :null))))

(cl-defmethod org-chronos--json-serializable-object ((x org-chronos-clock-range))
  "Convert X to an object that can be serialized to JSON."
  (let ((start (org-chronos-clock-range-start x))
        (end (org-chronos-clock-range-end x))
        (duration (org-chronos-clock-range-duration-minutes x)))
    `((start . ,(ts-format start))
      (end . ,(ts-format end))
      (duration . ,(ceiling duration)))))

;;;; Other utility functions

(defun org-chronos--start-of-clocks (range-list)
  "Return the first start time of many clock events.

RANGE-LIST should be a list of `org-chronos-clock-range' objects."
  (->> (-map #'org-chronos-clock-range-start range-list)
       (-sort #'ts<)
       (car)))

(defun org-chronos--end-of-clocks (range-list)
  "Return the last end time of many clock events.

RANGE-LIST should be a list of `org-chronos-clock-range' objects."
  (->> (-map #'org-chronos-clock-range-end range-list)
       (-sort #'ts>)
       (car)))

;;;; Dynamic block

(cl-defun org-chronos--log-init (&key files span start end)
  "Create an object of `org-chronos-log' for a given span from files.

This function creates an object and analyse items for a given period.

FILES is a list of source Org files.

SPAN is a symbol that denotes the type of period.

START and END are ts objects for specifying the range of the
period. The latter is optional."
  (let ((obj (make-instance 'org-chronos-log
                            :snapshot-time (ts-now)
                            :span span
                            :files files
                            :start start
                            :end (or end (org-chronos--ts-span-end span start)))))
    (org-chronos--log-update obj)
    obj))

(cl-defgeneric org-chronos--log-update (obj)
  "Update the data in OBJ without changing its domain.")

(cl-defmethod org-chronos--log-update ((obj org-chronos-log))
  "Update the data in OBJ on the same period."
  (oset obj data (org-chronos--search-headings-with-clock
                  (oref obj files)
                  (oref obj start)
                  (oref obj end))))

(defun org-chronos--log-closed-p (object)
  "Return non-nil if a log OBJECT is exceeds the end of its range."
  (cl-check-type object org-chronos-log)
  (when (ts> (oref object snapshot-time) (oref object end))
    t))

(defun org-chronos--uniq-file-names (filenames)
  "Normalize FILENAMES and remove duplicates from them."
  ;; `file-truename' is slow, so we want to apply the function only once per
  ;; file.
  (cl-delete-duplicates
   (--map (abbreviate-file-name (file-truename it)) filenames)
   :test #'string-equal))

(defun org-dblock-write:clock-journal (params)
  "Dynamic block for reporting activities for a certain period.

PARAMS is a plist for dynamic block parameters. You can override
the defaults by customizing `org-chronos-log-dblock-defaults'."
  (let* ((initial-marker (point-marker))
         (params (org-combine-plists org-chronos-log-dblock-defaults params))
         (span (plist-get params :span))
         (files (plist-get params :files))
         (sections-raw (plist-get params :sections))
         (sections (if (listp sections-raw)
                       sections-raw
                     (-> (cl-etypecase sections-raw
                           (string sections-raw)
                           (symbol (symbol-name sections-raw)))
                         (split-string ","))))
         (range-start (org-chronos--ts-span-start
                       span
                       (if-let (start (plist-get params :start))
                           (org-chronos--ts-from-decoded-time
                            (funcall org-chronos-parse-date-function
                                     (cl-etypecase start
                                       (symbol (symbol-name start))
                                       (string start))))
                         (org-chronos--find-date-in-heading))))
         (concrete-files (-> (cl-etypecase files
                               (fbound (funcall files))
                               (list files)
                               (string files))
                             (append (when org-chronos-scan-containing-file
                                       (list (buffer-file-name))))
                             (org-chronos--uniq-file-names)))
         (log (org-chronos--log-init :span span
                                     :files concrete-files
                                     :start range-start))
         (group (plist-get params :group))
         (group-percents (plist-get params :group-percents))
         views
         (group (if (stringp group)
                    (intern group)
                  group))
         (elements (if org-chronos-clock-threshold
                       (-filter (lambda (x)
                                  (>= (org-chronos--sum-minutes x) org-chronos-clock-threshold))
                                (oref log data))
                     (oref log data)))
         (groups (when group
                   (cl-ecase group
                     (tag (org-chronos--group-elements-by-tag elements))
                     (category (org-chronos--group-elements-by-category elements))))))
    (goto-char initial-marker)
    (insert "#+CAPTION: Clock journal "
            (org-chronos--describe-range span range-start)
            (if (org-chronos--log-closed-p log)
                " (closed)"
              (concat " (not closed) taken at "
                      (ts-format "%F %R" (oref log snapshot-time))))
            "\n")
    (if (null (oref log data))
        (insert "There is no activity during this period yet.")
      (dolist (section sections)
        (pcase section
          ("groups"
           (when group
             (push (make-org-chronos-group-statistic-view :group-type group
                                                          :groups groups
                                                          :show-percents group-percents)
                   views)))
          ("property-groups"
           (when org-chronos-show-property-summary
             (dolist (p org-chronos-logged-properties)
               (let ((property (pcase p
                                 ((pred stringp) p)
                                 (`(,name . ,_) name))))
                 (push (make-org-chronos-group-statistic-view
                        :groups (org-chronos--group-elements-by-property property elements)
                        :group-type (capitalize
                                     (replace-regexp-in-string
                                      "_" " " property))
                        :show-percents group-percents)
                       views)))))
          ("entries"
           (push (make-org-chronos-entry-view
                  :items-or-groups (or groups elements)
                  :grouped group
                  :time-format (cl-ecase span
                                 (day "%R")
                                 (week "%F %a")
                                 (month "%F"))
                  :todo-state t
                  :show-total t)
                 views))
          ("closed"
           (push (make-org-chronos-closed-items-view
                  :items-or-groups (or groups elements)
                  :grouped group
                  :time-format (cl-ecase span
                                 (day "%R")
                                 (week "%F %a")
                                 (month "%F")))
                 views))
          ("created"
           (push (make-org-chronos-created-items-view
                  :items-or-groups (or groups elements)
                  :grouped group
                  :time-format (cl-ecase span
                                 (day "%R")
                                 (week "%F %a")
                                 (month "%F")))
                 views))
          (_
           (error "Unsupported section type: %s" section))))
      (org-chronos--write-org (make-org-chronos-composite-view :views (nreverse views)))
      (when org-chronos-auto-export
        (unless (and (stringp org-chronos-export-root-directory)
                     (file-directory-p org-chronos-export-root-directory))
          (error "Directory org-chronos-export-root-directory is nil or does not exist"))
        (let* ((dir (expand-file-name (format "%s/" span)
                                      org-chronos-export-root-directory))
               (filename (cl-ecase span
                           (day (ts-format "%Y%m%d.json" range-start))
                           (week (ts-format "%Y%m%d.json" range-start))
                           (month (ts-format "%Y%m.json" range-start)))))
          (unless (file-directory-p dir)
            (make-directory dir))
          (org-chronos--export-log-to-json log
                                           (expand-file-name filename dir)
                                           :group-type group
                                           :groups groups))))))

;;;; Timeline buffer
;; This is an experimental feature.
(defvar org-chronos-last-activity-time nil)

;;;###autoload
(defun org-chronos-timeline (start end)
  "Display a timeline from START to END in a separate buffer.

If a numeric prefix argument is given, it displays activities
from N hours ago."
  (interactive (list (if (and (numberp current-prefix-arg)
                              (> current-prefix-arg 0))
                         (ts-adjust 'hour (- current-prefix-arg) (ts-now))
                       (make-ts
                        :unix (float-time (org-read-date t t nil "Start time: "))))
                     (ts-now)))
  (let ((entries (->> (org-chronos--search-headings-with-clock
                       (org-agenda-files) start end)
                      (-map (lambda (element)
                              (-map (lambda (clock-entry)
                                      (cons clock-entry
                                            (-last-item (org-chronos-heading-element-olp element))))
                                    (org-chronos-heading-element-clock-entries element))))
                      (-flatten-n 1)
                      (-sort (-on #'ts< (-compose #'org-chronos-clock-range-start #'car)))))
        prev)
    (unless entries
      (user-error "No activity during the period"))
    (with-current-buffer (get-buffer-create "*org-chronos timeline*")
      (erase-buffer)
      (pcase-dolist (`(,clock . ,title) entries)
        (let* ((start (org-chronos-clock-range-start clock))
               (end (org-chronos-clock-range-end clock))
               (gap (when prev
                      (ts-difference start prev))))
          (insert (if (and gap (> gap 0))
                      (format "           (%s break)\n"
                              (ts-human-format-duration gap))
                    "")
                  (ts-format "%F %R" start)
                  "-"
                  (ts-format "%R" end)
                  " "
                  title
                  "\n")
          (setq prev end)))
      (setq org-chronos-last-activity-time prev)
      (insert (format "           (%s since the last activity)\n"
                      (ts-human-format-duration (ts-difference (ts-now) prev))))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun org-chronos-add-clock (start end)
  "Add a clock entry from START to END the current Org subtree."
  (interactive (if (and (derived-mode-p 'org-mode)
                        (not (org-before-first-heading-p)))
                   (list (org-read-date t t nil "From"
                                        (-some->> org-chronos-last-activity-time
                                          (ts-unix)))
                         (org-read-date t t nil "To"))
                 (user-error "Run this command inside an Org entry")))
  (save-excursion
    (org-clock-find-position nil)
    (insert org-clock-string " "
            (format-time-string (org-time-stamp-format t t) start)
            "--"
            (format-time-string (org-time-stamp-format t t) end)
            ;; This is updated immediately
            " => 0:00"
            "\n")
    (org-beginning-of-line 0)
    (org-clock-update-time-maybe)))

(provide 'org-chronos)
;;; org-chronos.el ends here
