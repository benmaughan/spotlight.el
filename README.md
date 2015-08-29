# spotlight.el
Emacs package to query Mac OS X spotlight

Provides a function `spotlight' which prompts for a query string to
search the spotlight database. The list of matching files
containing the query string in their bodies are presented and the
user can select the file or type a string to dynamically filter the
list of files by filename. The selected file is then opened and a
swiper search using the original query is launched.

In a nutshell, you search for files containing some text, narrow
the list of matching files by filtering on name, and then select
the file and jump to the matches of your original query string

Some of the code is based on parts of `counsel.el' by Oleh Krehel
at https://github.com/abo-abo/swiper
