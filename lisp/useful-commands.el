;; -*- lexical-binding: t; -*-

;; Delete current file and buffer (http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/)
(global-set-key (kbd "C-c D")  'delete-file-and-buffer)
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename))))))

;; Rename current file and buffer (http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/)
(global-set-key (kbd "C-c r")  'rename-file-and-buffer)
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; Mark two windows and swap
(setq swapping-buffer nil)
(setq swapping-window nil)
(defun swap-buffers-in-windows ()
"Swap buffers between two windows"
(interactive)
(if (and swapping-window
	swapping-buffer)
    (let ((this-buffer (current-buffer))
	    (this-window (selected-window)))
	(if (and (window-live-p swapping-window)
		(buffer-live-p swapping-buffer))
	    (progn (switch-to-buffer swapping-buffer)
		(select-window swapping-window)
		(switch-to-buffer this-buffer)
		(select-window this-window)
		(message "Swapped buffers."))
	(message "Old buffer/window killed.  Aborting."))
	(setq swapping-buffer nil)
	(setq swapping-window nil))
    (progn
    (setq swapping-buffer (current-buffer))
    (setq swapping-window (selected-window))
    (message "Buffer and window marked for swapping."))))
(global-set-key (kbd "C-c p") 'swap-buffers-in-windows)

(provide 'useful-commands)
