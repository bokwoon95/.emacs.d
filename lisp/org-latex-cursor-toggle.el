;; -*- lexical-binding: t; -*-
;; http://kitchingroup.cheme.cmu.edu/blog/2015/10/09/Automatic-latex-image-toggling-when-cursor-is-on-a-fragment/
;; when the point is on a latex image, toggle it to latex code
;; else display the corresponding latex image

;;; bokwoon additions
;; According to the comments, as of the latest org-mode you have to replace all 'org-latex-fragment-image-overlays' with (org--list-latex-overlays)
;; However the function (org--list-latex-overlays) seems to have been removed (at least in my version of emacs) so I found a webpage online mentioning this function declaration (https://code.orgmode.org/bzg/org-mode/commit/cadfbbe8af9b978a02f48ee70bf6c855fdd3e19d) and copied it into this file

;; 2 alternative scripts
;; http://slumpy.org/blog/2017-02-01-automatic-latex-preview-in-org-mode/
;; https://gist.github.com/cvcore/760008a4dfb2eadf42afdc9cf01ef979

;; This function definition was obtained from https://code.orgmode.org/bzg/org-mode/commit/cadfbbe8af9b978a02f48ee70bf6c855fdd3e19d
;; The script needs this function to work
;; (defun org--list-latex-overlays (&optional beg end)
;;   "List all Org LaTeX overlays in current buffer.
;; Limit to overlays between BEG and END when those are provided."
;;   (org-remove-if-not
;;    (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
;;    (overlays-in (or beg (point-min)) (or end (point-max)))))

;; (defvar org-latex-fragment-last nil
;;   "Holds last fragment/environment you were on.")

;; (defun org-latex-fragment-toggle ()
;;   "Toggle a latex fragment image "
;;   (interactive)
;;   (and (eq 'org-mode major-mode)
;;        (let* ((el (org-element-context))
;;               (el-type (car el)))
;;          (cond
;;           ;; were on a fragment and now on a new fragment
;;           ((and
;;             ;; fragment we were on
;;             org-latex-fragment-last
;;             ;; and are on a fragment now
;;             (or
;;              (eq 'latex-fragment el-type)
;;              (eq 'latex-environment el-type))
;;             ;; but not on the last one this is a little tricky. as you edit the
;;             ;; fragment, it is not equal to the last one. We use the begin
;;             ;; property which is less likely to change for the comparison.
;;             (not (= (org-element-property :begin el)
;;                     (org-element-property :begin org-latex-fragment-last))))
;;            ;; go back to last one and put image back
;;            (save-excursion
;;              (goto-char (org-element-property :begin org-latex-fragment-last))
;;              (org-preview-latex-fragment))
;;            ;; now remove current image
;;            (goto-char (org-element-property :begin el))
;;            (let ((ov (cl-loop for ov in (org--list-latex-overlays)
;;                            if
;;                            (and
;;                             (<= (overlay-start ov) (point))
;;                             (>= (overlay-end ov) (point)))
;;                            return ov)))
;;              (when ov
;;                (delete-overlay ov)))
;;            ;; and save new fragment
;;            (setq org-latex-fragment-last el))

;;           ;; were on a fragment and now are not on a fragment
;;           ((and
;;             ;; not on a fragment now
;;             (not (or
;;                   (eq 'latex-fragment el-type)
;;                   (eq 'latex-environment el-type)))
;;             ;; but we were on one
;;             org-latex-fragment-last)
;;            ;; put image back on
;;            (save-excursion
;;              (goto-char (org-element-property :begin org-latex-fragment-last))
;;              (org-preview-latex-fragment))
;;            ;; unset last fragment
;;            (setq org-latex-fragment-last nil))

;;           ;; were not on a fragment, and now are
;;           ((and
;;             ;; we were not one one
;;             (not org-latex-fragment-last)
;;             ;; but now we are
;;             (or
;;              (eq 'latex-fragment el-type)
;;              (eq 'latex-environment el-type)))
;;            (goto-char (org-element-property :begin el))
;;            ;; remove image
;;            (let ((ov (cl-loop for ov in (org--list-latex-overlays)
;;                            if
;;                            (and
;;                             (<= (overlay-start ov) (point))
;;                             (>= (overlay-end ov) (point)))
;;                            return ov)))
;;              (when ov
;;                (delete-overlay ov)))
;;            (setq org-latex-fragment-last el))))))


;; (add-hook 'post-command-hook 'org-latex-fragment-toggle)

;; ;; (add-hook 'org-mode-hook
;; ;; 	  (add-hook 'post-command-hook 'org-latex-fragment-toggle))

;; (defun org-latex-auto-on ()
;;   (interactive)
;;   (add-hook 'org-mode-hook
;; 	    (add-hook 'post-command-hook 'org-latex-fragment-toggle)))

;; (defun org-latex-auto-off ()
;;   (interactive)
;;   (add-hook 'org-mode-hook
;; 	    (remove-hook 'post-command-hook 'org-latex-fragment-toggle)))

;; (provide 'org-latex-cursor-toggle)
