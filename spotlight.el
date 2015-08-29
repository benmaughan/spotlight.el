;;; spotlight.el --- search files with Mac OS X spotlight                     -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Ben Maughan <benmaughan@gmail.com>
;; URL: http://www.pragmaticemacs.com
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1") (swiper "0.4.0"))
;; Keywords: search

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Provides a function `spotlight' which prompts for a query string to
;; search the spotlight database. The list of matching files
;; containing the query string in their bodies are presented and the
;; user can select the file or type a string to dynamically filter the
;; list of files by filename. The selected file is then opened and a
;; swiper search using the original query is launched.

;; In a nutshell, you search for files containing some text, narrow
;; the list of matching files by filtering on name, and then select
;; the file and jump to the matches of your original query string

;; Some of the code is based on parts of `counsel.el' by Oleh Krehel
;; at https://github.com/abo-abo/swiper

;;; Code:

(require 'swiper)

(defgroup spotlight nil
  "Search for files with Mac OS X spotlight."
  :group 'external
  :prefix "spotlight-")

(defcustom spotlight-base-dir "~/"
  "Search spotlight database for files below this directory. Default is user's home directory. Use '/' to search everywhere."
  :type 'string)

;; Function to be called by ivy to filter the spotlight file list
(defun spotlight-filter (spotlight-ivy-filter-string &rest _unused)
  "Filter spotlight results"
  (delq nil (mapcar (lambda (x) (and (string-match spotlight-ivy-filter-string x) x)) spotlight-list)))


;; Main function
(defun spotlight ()
  "Search for a string in the spotlight database.
You'll be given a list of files that match. Narrow to the
filename you want by typing text to match the filename and then
selecting a file will launch `swiper' for that file to search for
your original query. If file is a pdf, it will be opened but
swiper will not run."
  (interactive)

  ;;prompt for spotlight query
  (setq spotlight-query (read-from-minibuffer "Spotlight query: "))

  ;;set up command
  (setq spotlight-command (format "mdfind -onlyin %s '%s'" spotlight-base-dir spotlight-query))

  ;; capture to string
  (setq spotlight-result (shell-command-to-string spotlight-command))

  ;; split to list
  (setq spotlight-list (split-string spotlight-result "\n"))

  ;;use ivy to narrow
  (let ((ivy-height 20))
    (ivy-read "Filter: " 'spotlight-filter
            ;;:initial-input initial-input
            :dynamic-collection t
            :sort nil
            :action (lambda (x)
                      (when (string-match "\\(^\/.*\\)\\'" x)
                        (let ((file-name (match-string 1 x)))
                          (find-file file-name)
                          (unless (string-match "pdf$" x)
                            (swiper spotlight-query))))))))

(provide 'spotlight)
;;; spotlight.el ends here
