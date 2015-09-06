;;; spotlight.el --- search files with Mac OS X spotlight                     -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Ben Maughan <benmaughan@gmail.com>

;; Author: Ben Maughan <benmaughan@gmail.com>
;; URL: http://www.pragmaticemacs.com
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.1") (swiper "0.4.0") (counsel "0.1.0"))
;; Keywords: search

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
;; Provides two functions. These are:
;;
;; `spotlight' which prompts for a query string to search the
;; spotlight database. The list of matching files containing the query
;; string in their bodies are presented and the user can select the
;; file or type a string to dynamically filter the list of files by
;; filename. The selected file is then opened and a `swiper' search
;; using the original query is launched.
;;
;; In a nutshell, you search for files containing some text, narrow
;; the list of matching files by filtering on name, and then select
;; the file and jump to the matches of your original query string

;; `spotlight-ivy' prompts for a query string and searches the
;; spotlight database with dynamic updates for each new character
;; entered. You'll be given a list of files that match. Selecting a
;; file will launch `swiper' for that file searching for the query
;; string.

;; Customise the variable `spotlight-min-chars' to set the minimum
;; number of characters that must be entered before the first
;; spotlight search is performed in `spotlight-ivy'. Setting
;; spotlight-min-chars to a lower number will result in more matches
;; and can lead to slow performance.

;; Customise the variable `spotlight-base-dir' to specify the base
;; directory for the spotlight search for both `spotlight' and
;; `spotlight-ivy'. The spotlight database will be queried for files
;; below this directory. Default is user's home directory. Use '/' to
;; search everywhere.

;; Credits:

;; Some of the code is based on parts of `counsel.el' by Oleh Krehel
;; at https://github.com/abo-abo/swiper
;;
;; Thanks to commenters on https://www.reddit.com/r/emacs for feedback
;; on an early version of the package

;;; Code:

(require 'swiper)
(require 'counsel)

(defgroup spotlight nil
  "Search for files with Mac OS X spotlight."
  :group 'external
  :prefix "spotlight-")

(defcustom spotlight-base-dir "~"
  "Search spotlight database for files below this directory. Default is user's home directory. Use '/' to search everywhere."
  :type 'string)

(defcustom spotlight-min-chars 5
  "Minimum number of characters required before running spotlight
search in spotlight-ivy. After this many characters have been
entered, the search is updated with each new character. Setting
spotlight-min-chars to a lower number will result in more matches
and can lead to slow performance."
  :type 'integer)

(defcustom spotlight-ivy-height 20
  "Height in characters of minibuffer displaying search results."
  :type 'integer)

(defvar spotlight-list nil
  "Contains results of spotlight query.")


;; Function to be called by ivy to filter the spotlight file list
(defun spotlight-filter (spotlight-ivy-filter-string &rest _unused)
  "Filter spotlight results"
  (delq nil (mapcar (lambda (x) (and (string-match spotlight-ivy-filter-string x) x)) spotlight-list)))


;; Main function
;;;###autoload
(defun spotlight ()
  "Search for a string in the spotlight database.
You'll be given a list of files that match. Narrow to the
filename you want by typing text to match the filename and then
selecting a file will launch `swiper' for that file to search for
your original query."
  (interactive)


  (let (spotlight-query spotlight-command spotlight-result)
    ;;prompt for spotlight query
    (setq spotlight-query (read-from-minibuffer "Spotlight query: "))

    ;;set up command
    (setq spotlight-command (concat "mdfind -onlyin "
                                    (shell-quote-argument
                                     (expand-file-name spotlight-base-dir))
                                    " "
                                    spotlight-query))


    ;; capture to string
    (setq spotlight-result (shell-command-to-string spotlight-command))

    ;; split to list
    (setq spotlight-list (split-string spotlight-result "\n"))

    ;;use ivy to narrow
    (let ((ivy-height spotlight-ivy-height))
      (ivy-read "Filter: " 'spotlight-filter
                :dynamic-collection t
                :sort nil
                :action (lambda (x)
                          (find-file x)
                          (swiper spotlight-query))))))


;; alternative function with incremental spotlight search but no
;; filename filter

;; Function to be called by spotlight-ivy
(defun ivy-mdfind-function (string &rest _unused)
  "Issue mdfind for STRING."
  (if (< (length string) spotlight-min-chars)
      (counsel-more-chars spotlight-min-chars)
    (counsel--async-command
     (concat "mdfind -onlyin "
             (shell-quote-argument
              (expand-file-name spotlight-base-dir))
             " "
             string))
    nil))

;; Main function
;;;###autoload
(defun spotlight-ivy (&optional initial-input)
  "Search for a string in the spotlight database with dynamic
updates for each new character entered. You'll be given a list of
files that match. Selecting a file will launch `swiper' for that
file. INITIAL-INPUT can be given as the initial minibuffer input.

Customise the variable spotlight-min-chars to set the minimum
number of characters that must be entered before the first
spotlight search is performed. Setting spotlight-min-chars to a
lower number will result in more matches and can lead to slow
performance."
  (interactive)
  (let ((ivy-height spotlight-ivy-height))
    (ivy-read "spotlight: " 'ivy-mdfind-function
              :initial-input initial-input
              :dynamic-collection t
              :action (lambda (x)
                        (find-file x)
                        (swiper ivy-text)))))


(provide 'spotlight)
;;; spotlight.el ends here
