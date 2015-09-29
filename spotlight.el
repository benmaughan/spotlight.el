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
;; Alternatively, the user can use M-RET to dynamically
;; filter the list of matching files to reduce the number of matches
;; before selecting a file.
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
;; Customise the variable `spotlight-default-base-dir' to specify the default
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up variables                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup spotlight nil
  "Search for files with Mac OS X spotlight."
  :group 'external
  :prefix "spotlight-")

(defcustom spotlight-default-base-dir "~"
  "Search spotlight database for files below this directory.  Default is user's home directory.  Use '/' to search everywhere."
  :group 'external
  :type 'string)

(defcustom spotlight-min-chars 2
  "Minimum number of characters required before running spotlight search in `spotlight'.  After this many characters have been entered, the search is updated with each new character.  Setting `spotlight-min-chars' to a lower number will result in more matches and can lead to slow performance."
  :group 'external
  :type 'integer)

(defcustom spotlight-ivy-height 10
  "Height in characters of minibuffer displaying search results."
  :group 'external
  :type 'integer)

(defcustom spotlight-tmp-file "~/.emacs-spotlight-tmp-file"
  "Temporary file to store spotlight results."
  :group 'external
  :type 'string)

(defvar spotlight-user-base-dir nil
  "String containing base directory for spotlight search.  May be used to override `spotlight-default-base-dir'.")

(defvar spotlight-file-filter-flag nil
  "Flag to record if filename filtering is requested.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; function to break out of spotlight and filter on filename
(defun spotlight-launch-file-filter ()
  "Function to break out of the ivy spotlight search into the filename filter."
  (interactive)
  (setq spotlight-file-filter-flag t)
  (ivy-done))

;; create keymap
(defvar spotlight-map nil
  "Keymap for spotlight.")
(setq spotlight-map (make-sparse-keymap))
(define-key spotlight-map (kbd "M-RET") 'spotlight-launch-file-filter)

;; Function to be called by ivy to filter the spotlight file list
;; used by spotlight and spotlight-fast
(defun spotlight-filter (regex candidates)
  "Filter spotlight results list of CANDIDATES to match REGEX."
  (delq nil (mapcar (lambda (x) (and (string-match regex x) x)) candidates)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spotlight functions                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function to be called by ivy to run mdfind
(defun ivy-mdfind-function (string &rest _unused)
  "Issue mdfind for STRING."
  (if (< (length string) spotlight-min-chars)
      (counsel-more-chars spotlight-min-chars)
    (spotlight-async-command
     (concat "mdfind -onlyin "
             (shell-quote-argument
              (expand-file-name spotlight-user-base-dir))
             " "
             (shell-quote-argument string)
             " > "
             (expand-file-name spotlight-tmp-file)))
    '("" "working...")))

;; Modified version of counsel--async-command from counsel.el
(defun spotlight-async-command (cmd)
  (let* ((counsel--process "*spotlight*")
         (proc (get-process counsel--process))
         (buff (get-buffer counsel--process)))
    (when proc
      (delete-process proc))
    (when buff
      (kill-buffer buff))

    ;; delete tmp file if exists
    (when (file-exists-p spotlight-tmp-file)
      (delete-file spotlight-tmp-file))

    (setq proc (start-process-shell-command
                counsel--process
                counsel--process
                cmd))
    (set-process-sentinel proc #'spotlight-async-sentinel)))

;; Modified version of counsel--async-sentinel from counsel.el
(defun spotlight-async-sentinel (process event)
  (if (string= event "finished\n")
      (if (file-exists-p spotlight-tmp-file)
          (progn
            (with-current-buffer (process-buffer process)
              (insert-file-contents spotlight-tmp-file)
              ;; check if no matches
              (if (= (buffer-size (process-buffer process)) 0)
                  (setq ivy--all-candidates '("No matches"))
                  (setq ivy--all-candidates
                    (ivy--sort-maybe
                     (split-string (buffer-string) "\n" t))))
              (setq ivy--old-cands ivy--all-candidates))
            (ivy--exhibit))
        (progn
          (setq ivy--all-candidates '("Error - tmp file not found"))
          (setq ivy--old-cands ivy--all-candidates)
          (ivy--exhibit)))
    (if (string= event "exited abnormally with code 1\n")
        (progn
          (setq ivy--all-candidates '("Error"))
          (setq ivy--old-cands ivy--all-candidates)
          (ivy--exhibit)))))

;; Function to run the ivy filter in the file list for the spotlight
;; search where the file list is in the buffer *spotlight*
(defun spotlight-file-select-cached (query)
  "Filter file list in spotlight buffer and then open file with swiper search for string QUERY."

  ;;run query
  (let (spotlight-list)

    (with-current-buffer "*spotlight*"
      (setq spotlight-list (split-string (buffer-string) "\n" t)))

    (let ((ivy-height spotlight-ivy-height))
      (ivy-read "filename filter: " spotlight-list
                :matcher #'spotlight-filter
                :re-builder #'ivy--regex
                :action (lambda (x)
                          (find-file x)
                          (swiper query))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main spotlight function                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun spotlight (arg &optional initial-input)
  "Search for a string ARG in the spotlight database.

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

Use \\<spotlight-map> \\[spotlight-launch-file-filter] to filter the list of matching files by filename.

If used with a prefix argument then it will prompt the user for a
base directory to search below, otherwise it will use
`spotlight-default-base-dir' as the base directory."
  (interactive "P")

  ;;see if prefix arg was used
  (setq spotlight-user-base-dir (if arg
                                    ;;prompt for dir
                                    (read-directory-name "base directory: ")
                                  ;;else use default
                                  spotlight-default-base-dir))

  ;;run query with ivy
  (let ((ivy-height spotlight-ivy-height))
    (ivy-read "spotlight query: " 'ivy-mdfind-function
              :initial-input initial-input
              :dynamic-collection t
              :sort nil
              :keymap spotlight-map
              :action (lambda (x)
                        (if spotlight-file-filter-flag
                            ;;run filename filter
                            (progn (setq spotlight-file-filter-flag nil)
                                   (spotlight-file-select-cached
                                    ivy-text))
                          ;;else open file
                          (progn (setq spotlight-file-filter-flag nil)
                                 (find-file x)
                                 (swiper ivy-text)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spotlight-fast                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                                    (shell-quote-argument query)))

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

;; Main spotlight-fast function
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
`spotlight-default-base-dir' as the base directory."
  (interactive "P")

  ;;see if prefix arg was used
  (setq spotlight-user-base-dir (if arg
                                    ;;prompt for dir
                                    (read-directory-name "base directory: ")
                                  ;;else use default
                                  spotlight-default-base-dir))

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
                                     (expand-file-name spotlight-user-base-dir))
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



(provide 'spotlight)
;;; spotlight.el ends here
