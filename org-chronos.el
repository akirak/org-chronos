;;; org-chronos.el --- Org-based time management -*- lexical-binding: t -*-

(defun akirak/ts-range-for-day (year month dom)
  (let ((range-start (ts-update (make-ts :year year :month month :day dom :hour 5 :minute 0 :second 0))))
    (list range-start (ts-adjust 'day 1 range-start))))

(cl-defstruct akirak/org-clock-activity
  start end duration-string duration-minutes)

(defun akirak/org-clock-entries-on-heading ()
  (let* ((begin (point))
         (content-end (save-excursion
                        (org-back-to-heading)
                        (end-of-line 1)
                        (or (re-search-forward org-heading-regexp nil t)
                            (point-max))))
         (logbook-end (save-excursion
                        (re-search-forward org-logbook-drawer-re content-end t)))
         entries)
    (when logbook-end
      (while (re-search-forward org-clock-line-re logbook-end t)
        (let ((clock (org-element-clock-parser (line-end-position))))
          (when (and (eq 'clock (org-element-type clock))
                     (eq 'closed (org-element-property :status clock)))
            (let* ((timestamp (org-element-property :value clock))
                   (duration-string (org-element-property :duration clock)))
              (push (make-akirak/org-clock-activity
                     :start
                     (ts-fill
                      (make-ts :year (org-element-property :year-start timestamp)
                               :month (org-element-property :month-start timestamp)
                               :day (org-element-property :day-start timestamp)
                               :hour (org-element-property :hour-start timestamp)
                               :minute (org-element-property :minute-start timestamp)
                               :second 0))
                     :end
                     (ts-fill
                      (make-ts :year (org-element-property :year-end timestamp)
                               :month (org-element-property :month-end timestamp)
                               :day (org-element-property :day-end timestamp)
                               :hour (org-element-property :hour-end timestamp)
                               :minute (org-element-property :minute-end timestamp)
                               :second 0))
                     :duration-string duration-string
                     :duration-minutes (org-duration-string-to-minutes duration-string))
                    entries))))))
    entries))

(cl-defstruct akirak/org-heading-info-with-clock-entries
  marker olp tags category
  todo-state link
  clock-entries)

(defun akirak/org-collect-clock-entries-between-range (files from to)
  (org-ql-select files
    `(clocked :from ,from :to ,to)
    :action
    `(make-akirak/org-heading-info-with-clock-entries
      :marker (point-marker)
      :link (save-excursion
              (org-store-link nil 'interactive)
              (pop org-stored-links))
      :olp (org-get-outline-path t t)
      :tags (org-get-tags)
      :category (org-get-category)
      :todo-state (org-get-todo-state)
      :clock-entries
      (-filter (lambda (x)
                 (ts-in ,from ,to (akirak/org-clock-activity-start x)))
               (akirak/org-clock-entries-on-heading)))))

(defun akirak/org-group-headings-by-tag (entries &optional tags)
  (let ((tags (or tags akirak/org-task-type-tags))
        (entries-1 (copy-sequence entries))
        result)
    (dolist (tag tags)
      (-let (((group-entries rest) (--separate
                                    (-contains-p (akirak/org-heading-info-with-clock-entries-tags it)
                                                 tag)
                                    entries-1)))
        (push (cons tag group-entries) result)
        (setq entries-1 rest)))
    result))

(defun akirak/org-group-headings-by-category (entries)
  (-group-by #'akirak/org-heading-info-with-clock-entries-category entries))

(defun akirak/org-find-date-heading-ts ()
  (catch 'date-heading
    (save-excursion
      (while t
        (let ((heading (org-get-heading t t t t)))
          (when-let (decoded-time (ignore-errors
                                    (parse-time-string heading)))
            (throw 'date-heading (ts-update (make-ts
                                             :year (decoded-time-year decoded-time)
                                             :month (decoded-time-month decoded-time)
                                             :day (decoded-time-day decoded-time)
                                             :hour (or (decoded-time-hour decoded-time) 5)
                                             :minute (or (decoded-time-minute decoded-time) 0)
                                             :second (or (decoded-time-second decoded-time) 0))))))
        (unless (org-up-heading-safe)
          (throw 'date-heading nil))))))

(defun org-dblock-write:clock-journal (params)
  (let* ((span (or (plist-get params :span) 'day))
         (short-time-format (cl-ecase span
                              (day "%R")))
         (range-start (if-let (start (plist-get params :start))
                          (let ((decoded-time (parse-time-string start)))
                            (ts-update (make-ts
                                        :year (decoded-time-year decoded-time)
                                        :month (decoded-time-month decoded-time)
                                        :day (decoded-time-day decoded-time)
                                        :hour (or (decoded-time-hour decoded-time) 5)
                                        :minute (or (decoded-time-minute decoded-time) 0)
                                        :second (or (decoded-time-second decoded-time) 0))))
                        (akirak/org-find-date-heading-ts)))
         (range-end (ts-adjust span 1 range-start))
         (headings (akirak/org-collect-clock-entries-between-range
                    (org-agenda-files) range-start range-end))
         (group-type (or (plist-get params :type) 'tags))
         (groups (cl-ecase group-type
                   (tags (akirak/org-group-headings-by-tag headings))
                   (categories (akirak/org-group-headings-by-category headings))))
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
      (let ((sum (->> (-map #'akirak/org-heading-info-with-clock-entries-clock-entries
                            entries)
                      (-flatten-n 1)
                      (-map #'akirak/org-clock-activity-duration-minutes)
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
        (let* ((link (akirak/org-heading-info-with-clock-entries-link x))
               (clock-entries (akirak/org-heading-info-with-clock-entries-clock-entries x))
               (todo-state (akirak/org-heading-info-with-clock-entries-todo-state x))
               (sum (->> clock-entries
                         (-map #'akirak/org-clock-activity-duration-minutes)
                         (-sum))))
          (insert "| " group
                  " | "
                  (apply #'org-link-make-string link)
                  " | " (org-duration-from-minutes sum)
                  " | " (ts-format short-time-format
                                   (->> clock-entries
                                        (-map #'akirak/org-clock-activity-start)
                                        (-sort #'ts<)
                                        (car)))
                  " | " (ts-format short-time-format
                                   (->> clock-entries
                                        (-map #'akirak/org-clock-activity-end)
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
                                            (akirak/org-heading-info-with-clock-entries-clock-entries x))))
                             (-flatten-n 1)
                             (-sort (-on #'ts<
                                         (-compose #'akirak/org-clock-activity-start
                                                   #'cdr)))
                             (-reduce-from (lambda (xs x)
                                             (if (and xs
                                                      (equal (akirak/org-heading-info-with-clock-entries-olp (caar xs))
                                                             (akirak/org-heading-info-with-clock-entries-olp (car x)))
                                                      (ts= (akirak/org-clock-activity-end (cdar xs))
                                                           (akirak/org-clock-activity-start (cdr x))))
                                                 (let* ((start (akirak/org-clock-activity-start (cdar xs)))
                                                        (end (akirak/org-clock-activity-end (cdr x)))
                                                        (duration-minutes (/ (ts-difference end start) 60)))
                                                   (cons (cons (car x)
                                                               (make-akirak/org-clock-activity
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
            (let* ((last-end (and clock0 (akirak/org-clock-activity-end clock0)))
                   (start (akirak/org-clock-activity-start clock))
                   (blank-minutes (and last-end (/ (ts-difference start last-end) 60.0)))
                   (new-category (not (and h0
                                           (string= (akirak/org-heading-info-with-clock-entries-category h)
                                                    (akirak/org-heading-info-with-clock-entries-category h0)))))
                   (new-tags (not (and h0
                                       (equal (akirak/org-heading-info-with-clock-entries-tags h)
                                              (akirak/org-heading-info-with-clock-entries-tags h0)))))
                   (new-parent (not (and h0
                                         (equal (-butlast (akirak/org-heading-info-with-clock-entries-olp h0))
                                                (-butlast (akirak/org-heading-info-with-clock-entries-olp h))))))
                   (new-task (not (and h0
                                       (equal (akirak/org-heading-info-with-clock-entries-olp h)
                                              (akirak/org-heading-info-with-clock-entries-olp h0))))))
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
                                                   (string= (akirak/org-heading-info-with-clock-entries-category h2)
                                                            (akirak/org-heading-info-with-clock-entries-category h)))
                                                 activities)
                                    (-map #'cdr)
                                    (cons clock)))
                       (start (akirak/org-clock-activity-start clock))
                       (end (akirak/org-clock-activity-end (-last-item clocks)))
                       (duration (/ (ts-difference end start) 60))
                       (mean-duration (-sum (-map #'akirak/org-clock-activity-duration-minutes clocks))))
                  (insert (format "- %s-%s (%s/%s) Category /%s/.\n"
                                  (ts-format "%R" start)
                                  (ts-format "%R" end)
                                  (org-duration-from-minutes duration)
                                  (org-duration-from-minutes mean-duration) 
                                  (akirak/org-heading-info-with-clock-entries-category h)))))
              (when (or new-category new-tags)
                (insert (make-string 2 ?\s)
                        (format "- Tags %s.\n"
                                (mapconcat (lambda (s) (concat "=" s "="))
                                           (akirak/org-heading-info-with-clock-entries-tags h)
                                           " "))))
              (when (or new-category new-tags new-parent)
                (insert (make-string 4 ?\s)
                        (format "- /%s/\n"
                                (org-format-outline-path
                                 (-butlast (akirak/org-heading-info-with-clock-entries-olp h))
                                 nil nil " > "))))
              (when new-task
                (let* ((clocks (->> (-take-while (pcase-lambda (`(,h2 . ,_))
                                                   (equal (akirak/org-heading-info-with-clock-entries-olp h2)
                                                          (akirak/org-heading-info-with-clock-entries-olp h)))
                                                 activities)
                                    (-map #'cdr)
                                    (cons clock)))
                       (start (akirak/org-clock-activity-start clock))
                       (end (akirak/org-clock-activity-end (-last-item clocks)))
                       (duration (/ (ts-difference end start) 60))
                       (mean-duration (-sum (-map #'akirak/org-clock-activity-duration-minutes clocks))))
                  (insert (make-string 6 ?\s)
                          (format "- %s-%s (%s/%s) %s.\n"
                                  (ts-format "%R" start)
                                  (ts-format "%R" end)
                                  (org-duration-from-minutes duration)
                                  (org-duration-from-minutes mean-duration)
                                  (apply #'org-link-make-string
                                         (akirak/org-heading-info-with-clock-entries-link h))))))
              (insert (make-string 8 ?\s)
                      (format "- %s-%s (%s)"
                              (ts-format "%R" (akirak/org-clock-activity-start clock))
                              (ts-format "%R" (akirak/org-clock-activity-end clock))
                              (akirak/org-clock-activity-duration-string clock))
                      (if (not new-task)
                          (format " after %s blank"
                                  (org-duration-from-minutes blank-minutes))
                        "")
                      "\n"))
            (setq h0 h
                  clock0 clock)))))))

(org-dynamic-block-define "clock-journal" #'org-dblock-write:clock-journal)

(provide 'org-chronos)
;;; org-chronos.el ends here

