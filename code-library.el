;;; code-library.el --- use org-mode to collect code snippets

;; Copyright (C) 2004-2015 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2015-11-23
;; Version: 0.1
;; Keywords: lisp, code

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; code-library's code can be found here:
;;   http://github.com/lujun9972/code-library

;;; Commentary:

;; code-library is a tool that use org-mode to collect code snippets.

;;; Code:

(defgroup code-library nil
  "code library group"
  :prefix "code-library-")

(defcustom code-library-mode-file-alist '((c++-mode . "cpp.org")
                                          (dos-mode . "bat.org")
                                          (emacs-lisp-mode . "elisp.org")
                                          (perl-mode . "perl.org")
                                          (python-mode . "python.org")
                                          (sh-mode . "bash.org")
                                          (js-jsx-mode . "javascript.org")
                                          (js-mode . "javascript.org")
                                          (js2-jsx-mode . "javascript.org")
                                          (js2-mode . "javascript.org"))

  "Mapping the correspondence between `major-mode' and the snippet file."
  :group 'code-library)

(defcustom code-library-directory "~/CodeLibrary/"
  "Snippet files are stored in the directory."
  :group 'code-library)

(defcustom code-library-use-tags-command t
  "Automatically run `org-mode' tags prompt when saving a snippet."
  :group 'code-library)

(defcustom code-library-downcased-org-keywords nil
  "Control how org mode keywords are inserted.

When set to t, #+BEGIN_SRC and #+END_SRC will be inserted as
lower case instead."
  :group 'code-library)

(defcustom code-library-keep-indentation '(makefile-mode
                                           makefile-gmake-mode)
  "List of modes which will be keep tabs and indentation as is.

Normally code-library removed tabs to normalise indentation
because code can come from a range of sources where the
formatting and buffer local tab width can be in use."
  :group 'code-library)

(defun code-library-trim-left-margin ()
  "Remove common line whitespace prefix."
  (save-excursion
    (goto-char (point-min))
    (let ((common-left-margin) )
      (while (not (eobp))
        (unless (save-excursion
                  (looking-at "[[:space:]]*$"))
          (back-to-indentation)
          (setq common-left-margin
                (min (or common-left-margin (current-column)) (current-column))))
        (forward-line))
      (when (and common-left-margin (> common-left-margin 0))
        (goto-char (point-min))
        (while (not (eobp))
          (delete-region (point)
                         (+ (point)
                            (min common-left-margin
                                 (save-excursion
                                   (back-to-indentation)
                                   (current-column)))))
          (forward-line))))))

(defsubst code-library-buffer-substring (beginning end &optional keep-indent)
  "Return the content between BEGINNING and END.

Tabs are converted to spaces according to mode.

The first line is whitespace padded if BEGINNING is positioned
after the beginning of that line.

Common left margin whitespaces are trimmed.

If KEEP-INDENT is t, tabs and indentation will be kept."
  (let ((content (buffer-substring-no-properties beginning end))
        (content-tab-width tab-width)
        (content-column-start (save-excursion
                                (goto-char beginning)
                                (current-column))))
    (with-temp-buffer
      (let ((tab-width content-tab-width))
        (unless keep-indent
          (insert (make-string content-column-start ?\s)))
        (insert content)
        (unless keep-indent
          (untabify (point-min) (point-max))
          (code-library-trim-left-margin))
        (buffer-substring-no-properties (point-min) (point-max))))))


(defun code-library-get-thing (&optional keep-indent)
  "Return what's supposed to be saved to the conde library as a string."
  (let* ((bod (bounds-of-thing-at-point 'defun))
         (r (cond
             ((region-active-p) (cons (region-beginning) (region-end)))
             (bod bod)
             (t (cons (point-min) (point-max))))))
    (code-library-buffer-substring (car r) (cdr r) keep-indent)))

(defun code-library-create-snippet (head &optional keep-indent)
  "Create and return a new org heading with source block.

HEAD is the org mode heading"
  (let ((content (code-library-get-thing keep-indent))
        (code-major-mode (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
        (tangle-file (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)))))
    (with-temp-buffer
      (insert content)
      (org-escape-code-in-region (point-min) (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert "#+"
              (if code-library-downcased-org-keywords
                  "end_src"
                "END_SRC")
              "\n")
      (goto-char (point-min))
      (insert (format "* %s\n" head))
      (insert
       "#+"
       (if code-library-downcased-org-keywords
           "begin_src"
         "BEGIN_SRC")
       " "
       code-major-mode)
      (when tangle-file
        (insert (format " :tangle %s" tangle-file)))
      (insert "\n")
      (buffer-string))))


(defun code-library-save-code()
  "Save the snippet to it's file location."
  (interactive)
  (let* ((keep-indent (member major-mode code-library-keep-indentation))
         (head (read-string "Please enter this code description: " nil nil "Untitled"))
         (snippet (code-library-create-snippet head keep-indent))
         (code-major-mode (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
         (library-base-file (or (cdr (assoc major-mode code-library-mode-file-alist))
                                (concat code-major-mode ".org")))
         (library-file (expand-file-name library-base-file
                                         (file-name-as-directory code-library-directory))))
    (with-current-buffer
        (find-file-noselect library-file)
      (when (and keep-indent
                 (not (buffer-local-value 'org-src-preserve-indentation (current-buffer))))
        (add-file-local-variable-prop-line 'org-src-preserve-indentation t))
      (save-excursion
        (goto-char (point-max))
        (beginning-of-line)
        (unless (looking-at "[[:space:]]*$")
          (insert "\n"))
        (insert snippet)
        (when code-library-use-tags-command
          (org-set-tags-command)))
      (save-buffer))))

(provide 'code-library)

;;; code-library.el ends here
