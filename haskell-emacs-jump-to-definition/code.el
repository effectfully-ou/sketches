;; hasktags
(add-to-list 'load-path "<path-to-your-scripts-folder>/hasktags-emacs")
(load "hasktags")

(defun my-xref-find-etags ()
  (interactive)
  (let* ((xref-backend-functions '(etags--xref-backend))
         (thing (xref-backend-identifier-at-point 'etags)))
    (xref-find-definitions thing)))
(global-set-key (kbd "C-M-.") 'my-xref-find-etags)

(defun jds-find-tags-file ()
  "recursively searches each parent directory for a file named 'TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
  (progn
      (defun find-tags-file-r (path)
         "find the tags file from the parent directories"
         (let* ((parent (file-name-directory path))
                (possible-tags-file (concat parent "TAGS")))
           (cond
             ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
             ((string= "/TAGS" possible-tags-file) (error "no tags file found"))
             (t (find-tags-file-r (directory-file-name parent))))))
    (if (buffer-file-name)
        (catch 'found-it
          (find-tags-file-r (buffer-file-name)))
        (error "buffer is not visiting a file"))))

(defun jds-set-tags-file-path ()
  "calls `jds-find-tags-file' to recursively search up the directory tree to find
a file named 'TAGS'. If found, adds it to 'tags-table-list', otherwise raises an error."
  (interactive)
  (add-to-list 'tags-table-list (jds-find-tags-file)))

(add-hook 'haskell-mode-hook 'jds-set-tags-file-path)

(custom-set-variables
  <...>
 '(tags-case-fold-search nil)
 '(tags-table-list (quote ("<path-to-your-registry-folder>/TAGS"))))
