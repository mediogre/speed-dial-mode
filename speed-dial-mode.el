(setq speed-dial-ary (make-vector 10 nil))

(dotimes (i 10) 
  (define-key global-map `[?\e (control meta ,(aref (int-to-string i) 0))]
    `(lambda ()
       (interactive)
       (aset speed-dial-ary ,i (buffer-name))))
  (define-key global-map `[(control meta ,(aref (int-to-string i) 0))]
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