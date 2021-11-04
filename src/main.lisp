(ql:quickload :ltk)

(defun main (*posix-argv*)
  ; gui main function
  (ltk:with-ltk ()
    ; set window title
    (ltk:wm-title ltk:*tk* "rsa")
    ; initialize gui widgets
    (let ((rsa-button (make-instance 'ltk:button :text "rsa")))
      ; put gui widgets on grid
      (ltk:grid rsa-button 0 0 :padx 5 :pady 5))))
