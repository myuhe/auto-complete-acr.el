;; ess-R-object-popup.el
;; 
;; I have defined a function, ess-R-object-popup, that when
;; invoked, will return a popup with some information about
;; the object at point.  The information returned is
;; determined by which R function is called.  This is controlled
;; by an alist, called ess-R-object-popup-alist.  The default is
;; given below.  The keys are the classes of R object that will
;; use the associated function.  For example, when the function
;; is called while point is on a factor object, a table of that
;; factor will be shown in the popup.  The objects must of course
;; exist in the associated inferior R process for this to work.
;; The special key "other" in the alist defines which function
;; to call when the class is not mached in the alist.  By default,
;; the str function is called, which is actually a fairly useful
;; default for data.frame and function objects.
;; 
;; The last line of this file shows my default keybinding.
;; I simply save this file in a directory in my load-path
;; and then place (require 'ess-R-object-popup) in my .emacs

;; the alist
(setq ess-R-object-popup-alist
      '((numeric    . "summary")
        (factor     . "table")
        (integer    . "summary")
        (lm         . "summary")
        (other      . "str")))

(defun ess-R-object-popup ()
  "Get info for object at point, and display it in a popup."
  (interactive)
  (let ((objname (current-word))
        (curbuf (current-buffer))
        (tmpbuf (get-buffer-create "**ess-R-object-popup**")))
    (if objname
        (progn
          (ess-command (concat "class(" objname ")\n")  tmpbuf )
          (set-buffer tmpbuf)
          (let ((bs (buffer-string)))
            (if (not(string-match "\(object .* not found\)\|unexpected" bs))
                (let* ((objcls (buffer-substring
                                (+ 2 (string-match "\".*\"" bs))
                                (- (point-max) 2)))
                       (myfun (cdr(assoc-string objcls
                                                ess-R-object-popup-alist))))
                  (progn
                    (if (eq myfun nil)
                        (setq myfun
                              (cdr(assoc-string "other"
                                                ess-R-object-popup-alist))))
                    (ess-command (concat myfun "(" objname ")\n") tmpbuf)
                    (let ((bs (buffer-string)))
                      (progn
                        (set-buffer curbuf)
                        (popup-tip bs)))))))))
    (kill-buffer tmpbuf)))

;; my default key map
(define-key ess-mode-map "\C-c\C-g" 'ess-R-object-popup)

 (provide 'ess-R-object-popup)

