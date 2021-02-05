;;; org-chronos-utils.el --- Utilities -*- lexical-binding: t -*-

(require 'ts)
(require 'org)

(defcustom org-chronos-beginning-of-day '(:hour 5 :minute 0 :second 0)
  "Plist that represents the beginning time of day."
  :type 'plist)

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

(defun org-chronos--find-date-in-heading ()
  "Return a ts object for a date from the closest heading or its ancestors."
  (catch 'finish
    (save-excursion
      (while t
        (when-let (decoded-time (ignore-errors
                                  (parse-time-string
                                   (org-get-heading t t t t))))
          ;; If the time is invalid, most of the fields will be nil
          (when (decoded-time-year decoded-time)
            (throw 'finish (org-chronos--ts-from-decoded-time decoded-time))))
        (unless (org-up-heading-safe)
          (throw 'finish nil))))))

(defmethod org-chronos--describe-range (span ts)
  "Format a range starting at TS."
  (cl-ecase span
    (day (concat "on " (ts-format "%F" ts)))
    (month (concat "in " (ts-format "%Y-%m" ts)))))

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

(provide 'org-chronos-utils)
;;; org-chronos-utils.el ends here
