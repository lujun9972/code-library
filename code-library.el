;;; code-library.el --- use org-mode to manage the code blocks

;; Copyright (C) 2004-2015 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2015-11-03
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
;; typing-game's code can be found here:
;;   http://github.com/lujun9972/code-library

;;; Commentary:

;; code-library is a tool that use org-mode to manager code blocks.

;;; Code:

(defgroup code-library-group nil
  "code library group"
  :prefix "code-library-")
(defcustom code-library-mode-file-alist '((c++-mode . "cpp.org")
										  (emacs-lisp-mode . "elisp.org")
										  (python-mode . "python.org")
										  (perl-mode . "perl.org")
										  (dos-mode . "bat.org")
										  (sh-mode . "bash.org"))
  "映射major-mode与保存代码片段文件的对应关系")

(defcustom code-library-path "~/CodeLibrary/"
  "代码库文件存储的目录")

(defun code-library-save-code()
  "保存所选代码到代码库文件中"
  (interactive)
  (let* ((code (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'defun)))
		 (library-base-file (or (cdr (assoc major-mode code-library-mode-file-alist))
								"temp.org"))
		 (library-file (concat code-library-path library-base-file))
		 (export-file (file-name-nondirectory  (buffer-file-name)))
		 (head (read-string "请输入这段代码的说明"))
		 (code-major-mode (replace-regexp-in-string "-mode$" "" (format "%s" major-mode))))
	(save-excursion 
	  (find-file library-file)
	  (end-of-buffer)
	  (newline)
	  (insert (concat "* " head))
	  (newline-and-indent)
	  (insert (format "#+BEGIN_SRC %s :tangle %s" code-major-mode export-file))
	  (newline-and-indent)
	  (newline-and-indent)
	  (insert "#+END_SRC")
	  (forward-line -1)                   ;上一行
	  (org-edit-src-code)
	  (insert code)
	  (org-edit-src-exit)
	  (org-set-tags-command)              ;设置代码tags
	  (save-buffer))))
