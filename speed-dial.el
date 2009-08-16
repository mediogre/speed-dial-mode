;;; speed-dial.el --- jet to a buffer in no time

;; Author: Ed Sinjiashvili <mediogre@gmail.com>
;; Keywords: convenience, window

;;; Commentary:
;; 
;; speed-dial-mode is a minor mode that allows you to mark buffers of interest
;; with <ESC> M-C-<digit> and then quickly pop into the marked buffer by M-C-<digit>. 
;;
;; The main idea of speed-dial-mode is not mere (fast) switching to the particular buffer,
;; but popping it alongside the buffer from which M-C-<digit> event originated.
;;
;;; TODO:
;; - [ ] handles errors gracefully
;; - [ ] M-C-~ contextual speed-dial (.c/.h, .rb/_spec.rb, etc)
;; - [ ] popup and no focus
;; - [ ] re-read the file (if possible) if buffer was closed
;; - [ ] persistent state
;; - [ ] speed-dial schemes

;;; Code:
(setq speed-dial-ary (make-vector 10 nil)
      speed-dial-keymap (make-sparse-keymap))

(dotimes (i 10) 
  (define-key speed-dial-keymap `[?\e (control meta ,(aref (int-to-string i) 0))]
    `(lambda ()
       (interactive)
       (message "Speed-dial %s with M-C-%d" (buffer-name) ,i)
       (aset speed-dial-ary ,i (buffer-name))))
  (define-key speed-dial-keymap `[(control meta ,(aref (int-to-string i) 0))]
    `(lambda ()
       (interactive)
       (speed-dial-to ,i))))

(defun speed-dial-to (slot)
  (interactive "nSlot: ")
  (let* ((name (aref speed-dial-ary slot))
	 (window (get-buffer-window name 'visible)))
	 (if (null window)
	     (pop-to-buffer name)
	   (unless (eq (selected-window) window)
	     (switch-to-buffer-other-window name)))))

;;;###autoload
(define-minor-mode speed-dial-mode
  "Speed dial mode. Mark buffers and pop into them in an instance"
  :init-value t :global t :keymap speed-dial-keymap)

(provide 'speed-dial)
;;; speed-dial.el ends here
