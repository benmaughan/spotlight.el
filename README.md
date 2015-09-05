# spotlight.el
Emacs package to search for files using Mac OS X spotlight.

## Overview
The functionality is similar to e.g. `rgrep` with the advantages that

- The search is quicker as it queries the spotlight database
- The search can be performed dynamically, updating as the search
string is changed
- The list of file names can also be dynamically narrowed by typing a
filter
- When a file is selected it is opened with a `swiper` search running
with the original search query

## Functions
Provides two functions. These are:

1. `spotlight` which prompts for a query string to search the
spotlight database. The list of matching files containing the query
string in their bodies are presented and the user can select the file
or type a string to dynamically filter the list of files by file name.
The selected file is then opened and a `swiper` search using the
original query is launched.

 In a nutshell, you search for files containing some text, narrow
 the list of matching files by filtering on name, and then select
 the file and jump to the matches of your original query string

2. `spotlight-ivy` prompts for a query string and searches the
spotlight database with dynamic updates for each new character
entered. You`ll be given a list of files that match. Selecting a
file will launch `swiper` for that file searching for the query
string.

## Customisation
Customise the variable `spotlight-min-chars` to set the minimum
number of characters that must be entered before the first
spotlight search is performed in `spotlight-ivy`. Setting
spotlight-min-chars to a lower number will result in more matches
and can lead to slow performance.

Customise the variable `spotlight-base-dir` to specify the base
directory for the spotlight search for both `spotlight` and
`spotlight-ivy`. The spotlight database will be queried for files
below this directory. Default is user`s home directory. Use `/` to
search everywhere.


## Credits
Some of the code is based on parts of `counsel.el` by Oleh Krehel
at https://github.com/abo-abo/swiper

Thanks to commenters on https://www.reddit.com/r/emacs for feedback
on an early version of the package
