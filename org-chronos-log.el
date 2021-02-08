;;; org-chronos-log.el --- Logging facility -*- lexical-binding: t -*-

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
  (list :span 'day :files #'org-agenda-files)
  "Default parameters of the Org dynamic block."
  :type 'plist)

(defcustom org-chronos-annotate-links t
  "Whether to decorate headline names with links.

Note that a link is produced on every entry having clock
entries. This may lead to generating IDs if you have turned on
`org-id-link-to-org-use-id'."
  :type 'boolean)

(defcustom org-chronos-tag-groups nil
  "List of tags used to group headings."
  :type '(repeat string))

(defcustom org-chronos-auto-export nil
  "Whether to export the log data on every evaluation."
  :type 'boolean)

(defcustom org-chronos-export-root-directory nil
  "Destination directory of exporting.

Files are saved to subdirectories of this directory based on the
time span."
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
  (org-ql-select files
    `(clocked :from ,from :to ,to)
    :action
    `(make-org-chronos-heading-element
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

;;;; Generic functions

(defgeneric org-chronos--sum-minutes (x)
  "Return the clock sum on X in minutes.")

(defmethod org-chronos--sum-minutes ((x org-chronos-clock-range))
  (org-chronos-clock-range-duration-minutes x))
(defmethod org-chronos--sum-minutes ((x org-chronos-heading-element))
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
                                      (apply #'org-link-make-string link)
                                    (-last-item (org-chronos-heading-element-olp x))))
                                 ('duration
                                  (org-duration-from-minutes (org-chronos--sum-minutes x)))
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
                (org-duration-from-minutes total)
                (string-join (-repeat (1- (- (length columns) n)) " | "))
                " |\n")))
    (delete-backward-char 1)
    (org-table-align)))

(defun org-chronos--write-group-sums-as-org-table (groups group-type)
  (insert (format "| %s | Sum |\n" (cl-ecase group-type
                                     (tag "Tag")
                                     (categorie "Category")))
          "|-------+---------|\n")
  (let ((total 0))
    (pcase-dolist (`(,group . ,elements) groups)
      (let ((sum (-sum (-map #'org-chronos--sum-minutes elements))))
        (when (> sum 0)
          (insert (format "| %s | %s |\n"
                          (or group "-")
                          (org-duration-from-minutes sum))))
        (cl-incf total sum)))
    (insert "|-------+---------|\n")
    (insert (format "| *Total* | %s |\n" (org-duration-from-minutes total))))
  (org-table-align))

;;;; Exporting
(cl-defun org-chronos--build-object-for-json (&key span start end elements
                                                   group-type groups
                                                   files)
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

(defgeneric org-chronos--json-serializable-object (x))

(defmethod org-chronos--json-serializable-object ((x org-chronos-heading-element))
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

(defmethod org-chronos--json-serializable-object ((x org-chronos-clock-range))
  (let ((start (org-chronos-clock-range-start x))
        (end (org-chronos-clock-range-end x))
        (duration (org-chronos-clock-range-duration-minutes x)))
    `((start . ,(ts-format start))
      (end . ,(ts-format end))
      (duration . ,(ceiling duration)))))

;;;; Other utility functions

(defun org-chronos--start-of-clocks (range-list)
  "Return the first start time of many clock events."
  (->> (-map #'org-chronos-clock-range-start range-list)
       (-sort #'ts<)
       (car)))

(defun org-chronos--end-of-clocks (range-list)
  "Return the last end time of many clock events."
  (->> (-map #'org-chronos-clock-range-end range-list)
       (-sort #'ts>)
       (car)))

;;;; Dynamic block

(defun org-dblock-write:clock-journal (params)
  (let* ((params (org-combine-plists org-chronos-log-dblock-defaults params))
         (span (plist-get params :span))
         (files (plist-get params :files))
         (range-start (if-let (start (plist-get params :start))
                          (org-chronos--ts-from-string start)
                        (org-chronos--find-date-in-heading)))
         (range-end (ts-adjust span 1 range-start))
         (concrete-files (cl-etypecase files
                           (fbound (funcall files))
                           (list files)
                           (string files)))
         (elements (org-chronos--search-headings-with-clock
                    concrete-files
                    range-start range-end))
         (group (plist-get params :group))
         (group (if (stringp group)
                    (intern group)
                  group))
         (groups (when group
                   (cl-ecase group
                     (tag (org-chronos--group-elements-by-tag elements))
                     (category (org-chronos--group-elements-by-category elements))))))
    (insert "#+CAPTION: Clock journal "
            (org-chronos--describe-range span range-start)
            "\n")
    (when group
      (org-chronos--write-group-sums-as-org-table groups group)
      (insert "\n"))
    (org-chronos--write-elements-as-org-table (or groups elements)
                                              :grouped group
                                              :range-format
                                              (cl-ecase span
                                                (day "%R")
                                                (month "%F"))
                                              :todo-state t
                                              :show-total t)
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
                        (expand-file-name filename dir)))))))

(provide 'org-chronos-log)
;;; org-chronos-log.el ends here
