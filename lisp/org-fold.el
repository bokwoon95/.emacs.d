;;; org-fold.el
;; Copyright (C) 2009 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;
;; Proof of concept implementation of saving folding information in
;; org buffers. This variant saves folding information in a separate
;; file.
;; 

;; bokwoon additions
;; Instead of automatically saving on killing the buffer, manually call an interactive function org-fold-save
;; This removes the indiscriminate creation of .fold files everywhere when you don't need it.
;;
;; Courtesy of the compile log warnings & advisory
;; ‘show-all’ is an obsolete function (as of 25.1) ; use ‘outline-show-all’ instead.
;; ‘hide-subtree’ is an obsolete function (as of 25.1); use ‘outline-hide-subtree’ instead.

(require 'cl)
(require 'org)

(defun org-fold-get-fold-info-file-name ()
  (concat (buffer-file-name) ".fold"))
  ;; (concat "." (buffer-file-name) ".fold"))

(defun org-fold-save ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (foldstates)
      (unless (looking-at outline-regexp)
        (outline-next-visible-heading 1))
      (while (not (eobp))
        (push (if (some (lambda (o) (overlay-get o 'invisible))
                        (overlays-at (line-end-position)))
                  t)
              foldstates)
        (outline-next-visible-heading 1))
      (with-temp-file (org-fold-get-fold-info-file-name)
	(prin1 (nreverse foldstates) (current-buffer))))))

(defun org-fold-restore ()
  (save-excursion
    (goto-char (point-min))
    (let* ((foldfile (org-fold-get-fold-info-file-name))
	   (foldstates
	    (if (file-readable-p foldfile)
		(with-temp-buffer
		  (insert-file-contents foldfile)
 		  (read (current-buffer))))))
      (when foldstates
	(outline-show-all)
        (goto-char (point-min))
        (unless (looking-at outline-regexp)
          (outline-next-visible-heading 1))
        (while (and foldstates (not (eobp)))
          (if (pop foldstates)
	      (outline-hide-subtree))
          (outline-next-visible-heading 1))
        (message "Restored saved folding state")))))

(add-hook 'org-mode-hook 'org-fold-activate)

(defun org-fold-activate ()
  (org-fold-restore))

;; (defun org-fold-activate ()
;;   (org-fold-restore)
;;   (add-hook 'kill-buffer-hook 'org-fold-kill-buffer nil t))

(defun org-fold-kill-buffer ()
  ;; don't save folding info for unsaved buffers
  (unless (buffer-modified-p)
    (org-fold-save)))

(provide 'org-fold)
