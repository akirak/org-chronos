;;; org-chronos.el --- Org-based time management -*- lexical-binding: t -*-

(require 'org-element)
(require 'org-clock)
(require 'org-ql)
(require 'ts)
(require 'dash)

(defgroup org-chronos
  nil
  "FIXME: Describe the group")

;;;; Custom variables

(defcustom org-chronos-beginning-of-day '(:hour 5 :minute 0 :second 0)
  "Plist that represents the beginning time of day."
  :type 'plist)

(defcustom org-chronos-tag-groups nil
  "List of tags for grouping headings or todo entries."
  :type '(repeat string))

(defcustom org-chronos-annotate-links t
  "Whether to decorate headline names with links.

Note that a link is produced on every entry having clock
entries. This may lead to generating IDs if you have turned on
`org-id-link-to-org-use-id'."
  :type 'boolean)

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

(cl-defstruct org-chronos-heading-entry
  "Structure that holds information on a heading with clock entries."
  marker olp tags category todo-state link clock-entries)

;;;; Utility functions

(defun org-chronos--ts-range-for-day (year month day)
  "Return a list which represents the range for a given day.

This function returns a list which represents a time range for a day.
The day should be given as YEAR, MONTH, and DAY in integers.
It starts at a time specified in `org-chronos-beginning-of-day'
and ends at the same time on the next day."
  (let ((range-start (ts-update (apply #'make-ts
                                       :year year :month month :day day
                                       org-chronos-beginning-of-day))))
    (list range-start (ts-adjust 'day 1 range-start))))

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
    `(make-org-chronos-heading-entry
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

(defun org-chronos--group-headings-by-tag (heading-entries)
  "Group HEADING-ENTRIES by tags defined in `org-chronos-tag-groups'."
  (let ((entries-1 (copy-sequence heading-entries))
        result)
    (dolist (tag org-chronos-tag-groups)
      (-let (((group-entries rest) (--separate
                                    (-contains-p (org-chronos-heading-entry-tags it)
                                                 tag)
                                    entries-1)))
        (push (cons tag group-entries) result)
        (setq entries-1 rest)))
    result))

(defun org-chronos--group-headings-by-category (heading-entries)
  "Group HEADING-ENTRIES by categories."
  (-group-by #'org-chronos-heading-entry-category heading-entries))

(defun org-chronos--ts-from-decoded-time (decoded-time)
  "Return a ts object from DECODED-TIME.

DECODED-TIME is a list as produced by `decode-time' or
`parse-time-string'. If it does not contain time fields,
`org-chronos-beginning-of-day' is filled."
  (ts-update (make-ts
              :year (decoded-time-year decoded-time)
              :month (decoded-time-month decoded-time)
              :day (decoded-time-day decoded-time)
              :hour (or (decoded-time-hour decoded-time)
                        (plist-get org-chronos-beginning-of-day :hour))
              :minute (or (decoded-time-minute decoded-time)
                          (plist-get org-chronos-beginning-of-day :minute))
              :second (or (decoded-time-second decoded-time)
                          (plist-get org-chronos-beginning-of-day :second)))))

(defun org-chronos--ts-from-string (string)
  "Return a ts object from a STRING.

This uses `parse-time-string' to parse a time string and then
pass it to `org-chronos--ts-from-decoded-time'."
  (org-chronos--ts-from-decoded-time (parse-time-string string)))

(defun org-chronos--find-date-heading-ts ()
  "Return a ts object for a date from the closest heading or its ancestors."
  (catch 'finish
    (save-excursion
      (while t
        (when-let (decoded-time (ignore-errors
                                  (parse-time-string
                                   (org-get-heading t t t t))))
          (throw 'finish (org-chronos--ts-from-decoded-time decoded-time)))
        (unless (org-up-heading-safe)
          (throw 'finish nil))))))

(defun org-dblock-write:clock-journal (params)
  (let* ((span (or (plist-get params :span) 'day))
         (short-time-format (cl-ecase span
                              (day "%R")))
         (range-start (if-let (start (plist-get params :start))
                          (org-chronos--ts-from-string start)
                        (org-chronos--find-date-heading-ts)))
         (range-end (ts-adjust span 1 range-start))
         (headings (org-chronos--search-headings-with-clock
                    (org-agenda-files) range-start range-end))
         (group-type (or (plist-get params :type) 'tags))
         (groups (cl-ecase group-type
                   (tags (org-chronos--group-headings-by-tag headings))
                   (categories (org-chronos--group-headings-by-category headings))))
         (total 0))
    (insert "#+CAPTION: Clock summary "
            (cl-ecase span
              (day (concat "on " (ts-format "%F" range-start)))
              (month (concat "in " (ts-format "%Y-%m" range-start))))
            "\n")
    (insert (format "| %s | Sum |\n" (cl-ecase group-type
                                       (tags "Tag")
                                       (categories "Category")))
            "|-------+---------|\n")
    (pcase-dolist (`(,group . ,entries) groups)
      (let ((sum (->> (-map #'org-chronos-heading-entry-clock-entries
                            entries)
                      (-flatten-n 1)
                      (-map #'org-chronos-clock-range-duration-minutes)
                      (-sum))))
        (when (> sum 0)
          (insert (format "| =%s= | %s |\n" group (org-duration-from-minutes sum))))
        (cl-incf total sum)))
    (insert "|-------+---------|\n")
    (insert (format "| *Total time* | %s |\n" (org-duration-from-minutes total)))
    (org-table-align)
    (insert "\n*Time on tasks*\n\n")
    (insert "| Group | Task | Duration | Start | End | State |\n")
    (insert "|------+------+----------+-------+-----+-------|\n")
    (pcase-dolist (`(,group . ,entries) groups)
      (dolist (x entries)
        (let* ((link (org-chronos-heading-entry-link x))
               (clock-entries (org-chronos-heading-entry-clock-entries x))
               (todo-state (org-chronos-heading-entry-todo-state x))
               (sum (->> clock-entries
                         (-map #'org-chronos-clock-range-duration-minutes)
                         (-sum))))
          (insert "| " group
                  " | "
                  (apply #'org-link-make-string link)
                  " | " (org-duration-from-minutes sum)
                  " | " (ts-format short-time-format
                                   (->> clock-entries
                                        (-map #'org-chronos-clock-range-start)
                                        (-sort #'ts<)
                                        (car)))
                  " | " (ts-format short-time-format
                                   (->> clock-entries
                                        (-map #'org-chronos-clock-range-end)
                                        (-sort #'ts>)
                                        (car)))
                  " | " todo-state
                  " |\n")))
      (org-table-align))
    (when (eq span 'day)
      (insert "\n*Full timeline, structured*\n\n")
      (let ((activities (->> headings
                             (-map (lambda (x)
                                     (--map (cons x it)
                                            (org-chronos-heading-entry-clock-entries x))))
                             (-flatten-n 1)
                             (-sort (-on #'ts<
                                         (-compose #'org-chronos-clock-range-start
                                                   #'cdr)))
                             (-reduce-from (lambda (xs x)
                                             (if (and xs
                                                      (equal (org-chronos-heading-entry-olp (caar xs))
                                                             (org-chronos-heading-entry-olp (car x)))
                                                      (ts= (org-chronos-clock-range-end (cdar xs))
                                                           (org-chronos-clock-range-start (cdr x))))
                                                 (let* ((start (org-chronos-clock-range-start (cdar xs)))
                                                        (end (org-chronos-clock-range-end (cdr x)))
                                                        (duration-minutes (/ (ts-difference end start) 60)))
                                                   (cons (cons (car x)
                                                               (make-org-chronos-clock-range
                                                                :start start
                                                                :end end
                                                                :duration-minutes duration-minutes
                                                                :duration-string
                                                                (org-duration-from-minutes
                                                                 duration-minutes)))
                                                         (cdr xs)))
                                               (cons x xs)))
                                           nil)
                             (nreverse)))
            h0 clock0
            activity)
        (while (setq activity (pop activities))
          (pcase-let
              ((`(,h . ,clock) activity))
            (let* ((last-end (and clock0 (org-chronos-clock-range-end clock0)))
                   (start (org-chronos-clock-range-start clock))
                   (blank-minutes (and last-end (/ (ts-difference start last-end) 60.0)))
                   (new-category (not (and h0
                                           (string= (org-chronos-heading-entry-category h)
                                                    (org-chronos-heading-entry-category h0)))))
                   (new-tags (not (and h0
                                       (equal (org-chronos-heading-entry-tags h)
                                              (org-chronos-heading-entry-tags h0)))))
                   (new-parent (not (and h0
                                         (equal (-butlast (org-chronos-heading-entry-olp h0))
                                                (-butlast (org-chronos-heading-entry-olp h))))))
                   (new-task (not (and h0
                                       (equal (org-chronos-heading-entry-olp h)
                                              (org-chronos-heading-entry-olp h0))))))
              (when (and blank-minutes
                         new-task)
                (insert (cond
                         (new-category "")
                         (new-tags (make-string 2 ?\s))
                         (new-parent (make-string 4 ?\s))
                         (new-task (make-string 6 ?\s))
                         (t (make-string 8 ?\s)))
                        (format "- %s-%s (%s) Blank.\n"
                                (ts-format "%R" last-end)
                                (ts-format "%R" start)
                                (org-duration-from-minutes blank-minutes))))
              (when new-category
                (let* ((clocks (->> (-take-while (pcase-lambda (`(,h2 . ,_))
                                                   (string= (org-chronos-heading-entry-category h2)
                                                            (org-chronos-heading-entry-category h)))
                                                 activities)
                                    (-map #'cdr)
                                    (cons clock)))
                       (start (org-chronos-clock-range-start clock))
                       (end (org-chronos-clock-range-end (-last-item clocks)))
                       (duration (/ (ts-difference end start) 60))
                       (mean-duration (-sum (-map #'org-chronos-clock-range-duration-minutes clocks))))
                  (insert (format "- %s-%s (%s/%s) Category /%s/.\n"
                                  (ts-format "%R" start)
                                  (ts-format "%R" end)
                                  (org-duration-from-minutes duration)
                                  (org-duration-from-minutes mean-duration) 
                                  (org-chronos-heading-entry-category h)))))
              (when (or new-category new-tags)
                (insert (make-string 2 ?\s)
                        (format "- Tags %s.\n"
                                (mapconcat (lambda (s) (concat "=" s "="))
                                           (org-chronos-heading-entry-tags h)
                                           " "))))
              (when (or new-category new-tags new-parent)
                (insert (make-string 4 ?\s)
                        (format "- /%s/\n"
                                (org-format-outline-path
                                 (-butlast (org-chronos-heading-entry-olp h))
                                 nil nil " > "))))
              (when new-task
                (let* ((clocks (->> (-take-while (pcase-lambda (`(,h2 . ,_))
                                                   (equal (org-chronos-heading-entry-olp h2)
                                                          (org-chronos-heading-entry-olp h)))
                                                 activities)
                                    (-map #'cdr)
                                    (cons clock)))
                       (start (org-chronos-clock-range-start clock))
                       (end (org-chronos-clock-range-end (-last-item clocks)))
                       (duration (/ (ts-difference end start) 60))
                       (mean-duration (-sum (-map #'org-chronos-clock-range-duration-minutes clocks))))
                  (insert (make-string 6 ?\s)
                          (format "- %s-%s (%s/%s) %s.\n"
                                  (ts-format "%R" start)
                                  (ts-format "%R" end)
                                  (org-duration-from-minutes duration)
                                  (org-duration-from-minutes mean-duration)
                                  (apply #'org-link-make-string
                                         (org-chronos-heading-entry-link h))))))
              (insert (make-string 8 ?\s)
                      (format "- %s-%s (%s)"
                              (ts-format "%R" (org-chronos-clock-range-start clock))
                              (ts-format "%R" (org-chronos-clock-range-end clock))
                              (org-chronos-clock-range-duration-string clock))
                      (if (not new-task)
                          (format " after %s blank"
                                  (org-duration-from-minutes blank-minutes))
                        "")
                      "\n"))
            (setq h0 h
                  clock0 clock)))))))

(provide 'org-chronos)
;;; org-chronos.el ends here

