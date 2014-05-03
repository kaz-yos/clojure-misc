;;; 2014-04-12 Clojure

(require 'clojure.java.io)

	  ;; (delete-other-windows)

	  ;; ;; Make window1 keep the selected (only) window
	  ;; (setq window1 (selected-window))
	  ;; ;; Make name-script-buffer keep the selected (only) buffer
	  ;; (setq name-script-buffer (buffer-name))
	  ;; ;; (split-window &optional WINDOW SIZE SIDE)
	  ;; ;; Split window1 (only one) without size, and create a new window on the right.
	  ;; ;; Use the return value (new window) for window2.
	  ;; ;; window1: left, window2: right
	  ;; (setq window2 (split-window window1 nil "right"))

	  ;; ;; Activate the REPL
	  ;; (funcall fun-repl-start)
	  ;; ;; Make name-repl-buffer keep the selected buffer (REPL)
	  ;; (setq name-repl-buffer (buffer-name))

	  ;; ;; REPL on the left (window1)	; This gives an error.
	  ;; (set-window-buffer window1 name-repl-buffer)
	  ;; ;; Script on the right (window2)
	  ;; (set-window-buffer window2 name-script-buffer)
	  ;; ;; Select the script window on the right (window2)
	  ;; (select-window window2)
