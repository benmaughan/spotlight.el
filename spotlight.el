;;; spotlight.el --- search files with Mac OS X spotlight                     -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Ben Maughan <benmaughan@gmail.com>

;; Author: Ben Maughan <benmaughan@gmail.com>
;; URL: http://www.pragmaticemacs.com
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.1") (swiper "0.6.0") (counsel "0.6.0"))
;; Keywords: search, external

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
;; `spotlight' prompts for a query string and searches the
;; spotlight database with dynamic updates for each new character
;; entered. You'll be given a list of files that match. Selecting a
;; file will launch `swiper' for that file searching for the query
;; string.
;;
;; Alternatively, the user can use <M-f> to dynamically filter the
;; list of matching files to reduce the number of matches before
;; selecting a file.
;;
;; `spotlight-fast' is the same as `spotlight' but the user is
;; prompted for a query string to search the spotlight database
;; without incremental updates. This can be much faster than
;; `spotlight'. The list of matching files containing the query string
;; in their bodies are presented and the user can select the file or
;; type a string to dynamically filter the list of files by filename.
;; The selected file is then opened and a `swiper' search using the
;; original query is launched.
;;
;; Customise the variable `spotlight-min-chars' to set the minimum
;; number of characters that must be entered before the first
;; spotlight search is performed in `spotlight'. Setting
;; `spotlight-min-chars' to a lower number will result in more matches
;; and can lead to slow performance.
;;
;; Customise the variable `spotlight-base-dir' to specify the default
;; base directory for the spotlight search for both `spotlight' and
;; `spotlight-live'. The spotlight database will be queried for files
;; below this directory. Default is user's home directory. Use '/' to
;; search everywhere. Alternatively, both `spotlight' and
;; `spotlight-fast' can be called with a prefix argument, in which
;; case they will prompt for a base directory.

;; Credits:

;; Some of the code is based on parts of counsel.el by Oleh Krehel
;; at https://github.com/abo-abo/swiper
;;
;; The dynamic filtering is done with the ivy library by the same
;; author
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
  "Search spotlight database for files below this directory.  Default is user's home directory.  Use '/' to search everywhere."
  :type 'string)

(defcustom spotlight-min-chars 5
  "Minimum number of characters required before running spotlight search in `spotlight' and `spotlight-live'.  After this many characters have been entered, the search is updated with each new character.  Setting `spotlight-min-chars' to a lower number will result in more matches and can lead to slow performance."
  :type 'integer)

(defcustom spotlight-ivy-height 20
  "Height in characters of minibuffer displaying search results."
  :type 'integer)

(defvar spotlight-user-base-dir nil
  "String containing base directory for spotlight search.  May be used to override `spotlight-base-dir'.")

(defvar spotlight-file-filter-flag nil
  "Flag to record if filename filtering is requested.")

;;add key to ivy keymap to launch filename filter
(define-key ivy-minibuffer-map (kbd "M-f")
  (lambda () (interactive)
    (setq spotlight-file-filter-flag t)
    (ivy-done)))


;; Function to be called by ivy to filter the spotlight file list
(defun spotlight-filter (regex candidates)
  "Filter spotlight results list of CANDIDATES to match REGEX."
  (delq nil (mapcar (lambda (x) (and (string-match regex x) x)) candidates)))

;; Main function
;;;###autoload
(defun spotlight-fast (arg &optional initial-input)
  "Search for a string in the spotlight database.

You'll be given a list of files that match.  Narrow to the
filename you want by typing text to match the filename and then
selecting a file will launch `swiper' for that file to search for
your original query.

Optionally provide INITIAL-INPUT to specify the query string and
jump straight to the filename filter.

If used with a prefix argument then it will prompt the user for a
base directory to search below, otherwise it will use
`spotlight-base-dir' as the base directory."
  (interactive "P")

  ;;see if prefix arg was used
  (if arg
      ;; prompt for dir
      (setq spotlight-user-base-dir (read-directory-name "base directory: "))
    ;; else use default
    (setq spotlight-user-base-dir spotlight-base-dir))

  ;;run query
  (let (spotlight-query spotlight-command spotlight-result spotlight-list)
    ;;test if initial input is used - this would be the case if called
    ;;from spotlight
    (if initial-input
        (setq spotlight-query initial-input)
      ;;else prompt for spotlight query
      (setq spotlight-query (read-from-minibuffer "spotlight-fast query: ")))

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

    (let ((ivy-height spotlight-ivy-height))
      (ivy-read "filename filter: " spotlight-list
                :matcher #'spotlight-filter
                :re-builder #'ivy--regex
                :action (lambda (x)
                          (find-file x)
                          (swiper spotlight-query))))))



;; Function to be called by spotlight
(defun ivy-mdfind-function (string &rest _unused)
  "Issue mdfind for STRING."
  (if (< (length string) spotlight-min-chars)
      (counsel-more-chars spotlight-min-chars)
    (counsel--async-command
     (concat "mdfind -onlyin "
             (shell-quote-argument
              (expand-file-name spotlight-user-base-dir))
             " "
             string))
    nil))

;; Function to run the ivy filter in the file list
(defun spotlight-file-select (dir query)
  "Run spotlight in base directory DIR with query QUERY and filter list."

  ;;run query
  (let (spotlight-command spotlight-result spotlight-list)

    ;;set up command
    (setq spotlight-command (concat "mdfind -onlyin "
                                    (shell-quote-argument
                                     (expand-file-name dir))
                                    " "
                                    query))

    ;; capture to string
    (setq spotlight-result (shell-command-to-string spotlight-command))

    ;; split to list
    (setq spotlight-list (split-string spotlight-result "\n"))

    (let ((ivy-height spotlight-ivy-height))
      (ivy-read "filename filter: " spotlight-list
                :matcher #'spotlight-filter
                :re-builder #'ivy--regex
                :action (lambda (x)
                          (find-file x)
                          (swiper query))))))


;; Spotlight function
;;;###autoload
(defun spotlight (arg &optional initial-input)
  "Search for a string in the spotlight database.

Uses `ivy-read' to perform dynamic updates for each new character
entered.

You'll be given a list of files that match.  Selecting a file will
launch `swiper' for that file to search it for your query string.
INITIAL-INPUT can be given as the initial minibuffer input.

Customise the variable `spotlight-min-chars' to set the minimum
number of characters that must be entered before the first
spotlight search is performed.  Setting `spotlight-min-chars' to a
lower number will result in more matches and can lead to slow
performance.

Use <M-f> to filter the list of matching files by filename.

If used with a prefix argument then it will prompt the user for a
base directory to search below, otherwise it will use
`spotlight-base-dir' as the base directory."
  (interactive "P")
  (let ((ivy-height spotlight-ivy-height))
    ;;see if prefix arg was used
    (if arg
        ;; prompt for dir
        (setq spotlight-user-base-dir (read-directory-name "base directory: "))
      ;; else use default
      (setq spotlight-user-base-dir spotlight-base-dir))

    ;;run query
    (ivy-read "spotlight query: " 'ivy-mdfind-function
              :initial-input initial-input
              :dynamic-collection t
              :action (lambda (x)
                        (if spotlight-file-filter-flag
                            ;;run filename filter
                            (progn (setq spotlight-file-filter-flag nil)
                                   (spotlight-file-select spotlight-user-base-dir ivy-text))
                          ;; open file
                          (progn (setq spotlight-file-filter-flag nil)
                                 (find-file x)
                                 (swiper ivy-text)))))))


(provide 'spotlight)
;;; spotlight.el ends here
