;; 记录代码片段到特定文件中
(require 'file-helper)
(require 'buffer-helper)
(defgroup code-library-group nil
  "代码库设置")
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
  (let* ((code (get-region-or-thing 'defun))
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
