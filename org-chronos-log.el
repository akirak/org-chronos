;;; org-chronos-log.el --- Logging facility -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
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

(require 'org-chronos-utils)

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

(cl-defstruct org-chronos-heading-element
  "Structure that holds information on a heading with clock entries."
  marker olp tags category todo-state link clock-entries)

;;;; Custom variables

(defcustom org-chronos-log-dblock-defaults
  (list :span 'day :files #'org-agenda-files
        :sections "groups,entries")
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
`org-id-link-to-org-use-id'."
  :group 'org-chronos
  :type 'boolean)

(defcustom org-chronos-trim-headline 50
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

(defun org-chronos--clock-entries-on-heading ()
  "Return clock entries on the current Org heading after the point."
  (save-excursion
    (org-back-to-heading)
    (end-of-line 1)
    (let* ((content-end (save-excursion
                          (or (re-search-forward org-heading-regexp
                                                 nil t)
                              (point-max))))
           (logbook-end (save-excursion
                          (re-search-forward org-logbook-drawer-re
                                             content-end t)))
           entries)
      (when logbook-end
        (while (re-search-forward org-clock-line-re
                                  logbook-end t)
          (when-let (range (org-chronos--parse-clock-line))
            (push range entries))))
      entries)))

(defun org-chronos--search-headings-with-clock (files from to)
  "Search headings with clock entries in a given time range.

FIXME: FILES, FROM, and TO."
  (->> (org-ql-select files
         `(clocked :from ,from :to ,to)
         :action
         `(org-save-outline-visibility t
            (org-show-entry)
            (make-org-chronos-heading-element
             :marker (point-marker)
             :link (when org-chronos-annotate-links
                     (save-excursion
                       (org-store-link nil 'interactive)
                       (pop org-stored-links)))
             :olp (org-get-outline-path t t)
             :tags (org-get-tags)
             :category (org-get-category)
             :todo-state (org-get-todo-state)
             :clock-entries
             (-filter (lambda (x)
                        (ts-in ,from ,to (org-chronos-clock-range-start x)))
                      (org-chronos--clock-entries-on-heading)))))
       (-filter #'org-chronos--meaningful-element-p)))

(defun org-chronos--meaningful-element-p (element)
  "Check if ELEMENT will be meaningful in the report."
  ;; Since clocks may not be contained in the heading but in
  ;; children, you have to exclude headings without clock entries
  ;; during the period.
  (and (org-chronos-heading-element-clock-entries element)
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
          (unless (and org-chronos-clock-threshold
                       (< (org-chronos--sum-minutes x) org-chronos-clock-threshold))
            (insert "| "
                    (mapconcat (lambda (column)
                                 (pcase (car column)
                                   ('group
                                    group)
                                   ('name
                                    (if-let (link (org-chronos-heading-element-link x))
                                        (org-link-make-string (car link)
                                                              (org-chronos--trim-string-at-length
                                                               (nth 1 link)
                                                               org-chronos-trim-headline))
                                      (org-chronos--trim-string-at-length
                                       (-last-item (org-chronos-heading-element-olp x))
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
                    " |\n"))
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
                (percent "%")))
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

;;;; Exporting
(cl-defun org-chronos--build-object-for-json (&key span start end elements
                                                   group-type groups
                                                   files)
  "FIXME: SPAN START END ELEMENTS GROUP-TYPE GROUPS FILES."
  `((source . ((files . ,(apply #'vector files))))
    (range . ((span . ,(symbol-name span))
              (start . ,(ts-format start))
              (end . ,(ts-format end))))
    (data . ((headings . ,(apply #'vector
                                 (-map #'org-chronos--json-serializable-object
                                       elements)))
             (groups . ,(when groups
                          `((,group-type
                             . ,(apply #'vector
                                       (-map (pcase-lambda (`(,group . ,elements))
                                               `((group-type . ,(symbol-name group-type))
                                                 (group-name . ,group)
                                                 (headings
                                                  . ,(apply #'vector
                                                            (-map #'org-chronos--json-serializable-object
                                                                  elements)))))
                                             groups))))))))))

(cl-defgeneric org-chronos--json-serializable-object (x)
  "Convert X to an object that can be serialized to JSON.")

(cl-defmethod org-chronos--json-serializable-object ((x org-chronos-heading-element))
  "Convert X to an object that can be serialized to JSON."
  (let ((marker (org-chronos-heading-element-marker x))
        (olp (-map #'org-link-display-format (org-chronos-heading-element-olp x)))
        (tags (org-chronos-heading-element-tags x))
        (category (org-chronos-heading-element-category x))
        (todo-state (org-chronos-heading-element-todo-state x))
        (link (org-chronos-heading-element-link x))
        (clock-entries (org-chronos-heading-element-clock-entries x)))
    `((buffer . ,(buffer-name (marker-buffer marker)))
      (position . ,(marker-position marker))
      (title . ,(-last-item olp))
      (outline . ,(apply #'vector olp))
      (tags . ,(apply #'vector tags))
      (link . ,(car link))
      (category . ,category)
      (todo . ,todo-state)
      (clocks . ,(apply #'vector
                        (-map #'org-chronos--json-serializable-object
                              clock-entries))))))

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

(defun org-dblock-write:clock-journal (params)
  "Dynamic block for reporting activities for a certain period.

PARAMS is a plist for dynamic block parameters. You can override
the defaults by customizing `org-chronos-log-dblock-defaults'."
  (let* ((params (org-combine-plists org-chronos-log-dblock-defaults params))
         (span (plist-get params :span))
         (files (plist-get params :files))
         (sections-raw (plist-get params :sections))
         (sections (if (listp sections-raw)
                       sections-raw
                     (-> (cl-etypecase sections-raw
                           (string sections-raw)
                           (symbol (symbol-name sections-raw)))
                         (split-string ","))))
         (range-start (if-let (start (plist-get params :start))
                          (org-chronos--ts-from-decoded-time
                           (funcall org-chronos-parse-date-function
                                    (cl-etypecase start
                                      (symbol (symbol-name start))
                                      (string start))))
                        (org-chronos--find-date-in-heading)))
         (range-end (ts-adjust span 1 range-start))
         (concrete-files (-> (cl-etypecase files
                               (fbound (funcall files))
                               (list files)
                               (string files))
                             (append (when org-chronos-scan-containing-file
                                       (list (buffer-file-name))))
                             (cl-delete-duplicates :test #'file-equal-p)))
         (elements (org-chronos--search-headings-with-clock
                    concrete-files
                    range-start range-end))
         (group (plist-get params :group))
         (group-percents (plist-get params :group-percents))
         (group (if (stringp group)
                    (intern group)
                  group))
         (groups (when group
                   (cl-ecase group
                     (tag (org-chronos--group-elements-by-tag elements))
                     (category (org-chronos--group-elements-by-category elements)))))
         margin)
    (insert "#+CAPTION: Clock journal "
            (org-chronos--describe-range span range-start)
            "\n")
    (if (null elements)
        (insert "There is no activity during this period yet.")
      (when (and group (member "groups" sections))
        (org-chronos--write-group-sums-as-org-table groups group
                                                    :show-percents group-percents)
        (setq margin t))
      (when (member "entries" sections)
        (when margin
          (insert "\n\n")
          (setq margin nil))
        (org-chronos--write-elements-as-org-table
         (or groups elements)
         :grouped group
         :range-format
         (cl-ecase span
           (day "%R")
           (month "%F"))
         :todo-state t
         :show-total t))
      (when org-chronos-auto-export
        (unless (and (stringp org-chronos-export-root-directory)
                     (file-directory-p org-chronos-export-root-directory))
          (error "Directory org-chronos-export-root-directory is nil or does not exist"))
        (let* ((dir (expand-file-name (format "%s/" span)
                                      org-chronos-export-root-directory))
               (filename (cl-ecase span
                           (day (ts-format "%Y%m%d.json" range-start))
                           (month (ts-format "%Y%m.json" range-start)))))
          (unless (file-directory-p dir)
            (make-directory dir))
          (with-temp-buffer
            (insert (json-serialize
                     (org-chronos--build-object-for-json
                      :span span
                      :start range-start
                      :end range-end
                      :elements elements
                      :group-type group
                      :groups groups
                      :files concrete-files)))
            (write-region (point-min) (point-max)
                          (expand-file-name filename dir))))))))

(provide 'org-chronos-log)
;;; org-chronos-log.el ends here
