;;; org-chronos.el --- Org-based time management -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (dash "2.18") (org-ql "0.6") (ts "0.2") (org "9.4"))
;; Keywords: outlines hypermerdia
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

(defgroup org-chronos
  nil
  "FIXME: Describe the group"
  :group 'org
  :group 'org-clock)

(require 'org-chronos-log)

(provide 'org-chronos)
;;; org-chronos.el ends here

