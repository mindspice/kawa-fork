(define-alias <marker> <gnu.jemacs.buffer.Marker>)
(define-alias <process> <gnu.jemacs.buffer.ProcessMode>)
(define-alias <buffer> <gnu.jemacs.buffer.Buffer>)
(define-alias <window> <gnu.jemacs.buffer.Window>)
(define-alias <frame> <gnu.jemacs.buffer.Frame>)
(define-alias <keymap> <javax.swing.text.Keymap>)

(define (set-size (win :: <java.awt.Component>) (w :: <int>) (h :: <int>))
  (invoke win w h))

;;; From here on, the functions are ordered as in the Lisp Reference
;;; Manual, XEmacs version 21.x.

;;; READ AND PRINT

(define (open-output-buffer (buffer <buffer>))
  (make <gnu.jemacs.buffer.BufferWriter> buffer))

(define (open-output-marker (marker <marker>))
  (make <gnu.jemacs.buffer.BufferWriter> marker #f))

;;; MINIBUFFERS

(define (read-dialog prompt)
  (symbol->string
   (invoke (as <frame> (window-frame)) 'ask prompt)))

(define read-from-minibuffer read-dialog)

;;; KEYMAPS
(define (make-keymap #!optional name)
  (invoke-static <gnu.jemacs.buffer.BufferKeymap> 'makeEmptyKeymap
                 (as <String> name)))

(define (make-sparse-keymap #!optional name)
  (invoke-static <gnu.jemacs.buffer.BufferKeymap> 'makeEmptyKeymap
                 (as <String> name)))

(define (set-keymap-name keymap new-name) #!void)  ;; ignored FIXME

(define (keymap-name (keymap <keymap>))
  (invoke keymap 'getName))

(define (set-keymap-parent (keymap  :: <keymap>) parent)
  (invoke keymap 'setResolveParent (or parent #!null)))

(define (set-keymap-parents (keymap  :: <keymap>) parents)
  (cond ((null? parents) (invoke keymap 'setResolveParent #!null))
	((null? (cdr parents)) (invoke keymap 'setResolveParent (car parents)))
	(else (error "not implemented - more than one keymap parent"))))

(define (keymap-parent (keymap :: <keymap>))
  (invoke keymap 'getResolveParent))

(define (keymap-parents keymap)
  (list (keymap-parent keymap)))

(define (set-keymap-default-binding
	 (keymap :: <keymap>) command)
  (invoke keymap 'setDefaultAction
	  (invoke-static <gnu.jemacs.buffer.BufferKeymap> 'asAction command)))
  
(define (keymap-default-binding (keymap :: <keymap>))
  (invoke-static <gnu.jemacs.buffer.BufferKeymap> 'asNonAction
		 (invoke keymap 'getDefaultAction)))

(define global-map
  (static-field <gnu.jemacs.buffer.BufferKeymap> 'globalKeymap))

(define esc-map
  (static-field <gnu.jemacs.buffer.BufferKeymap> 'metaKeymap))

(define (current-global-map)
  global-map)

(define (current-local-map #!optional (buffer :: <buffer> (current-buffer)))
  (invoke (field buffer 'keymap) 'getLocalKeymap))

(define (use-local-map keymap #!optional (buffer :: <buffer> (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.BufferKeymap> "setLocalKeymap"
                             <void> (<keymap>))
   (field buffer 'keymap)
   keymap))

(define (lookup-key (keymap :: <keymap>)
		    (keys :: <gnu.kawa.util.Sequence>)
		    #!optional (accept-defaults :: <boolean> #f))
  (let ((binding
         (invoke-static <gnu.jemacs.buffer.BufferKeymap> 'lookupKey
			keymap keys accept-defaults)))
    (if (eq? binding #!null) #f binding)))

(define (define-key keymap key binding)
  (invoke-static <gnu.jemacs.buffer.BufferKeymap> 'defineKey
		 keymap key binding))

;;; MENUS

(define current-menubar #!null)

(define (set-menubar-dirty-flag #!optional (frame :: <frame> (selected-frame)))
  (invoke frame 'setMenuBar
	  (make <gnu.jemacs.buffer.Menu> current-menubar)))

(define (set-menubar menubar)
  (set! current-menubar menubar)
  (set-menubar-dirty-flag))

;;; MODES

(define (redraw-modeline)  ;;XEmacs has optional "all" parameter!
  (invoke (as <buffer> (current-buffer)) 'redrawModeline))

(define (force-mode-line-update) (redraw-modeline))

;;; FILES

(define (find-file #!optional (filename (read-from-minibuffer "Find file: ")))
  (switch-to-buffer (find-file-noselect filename)))

(define (find-file-noselect
         #!optional (filename (read-from-minibuffer "Find file: ")))
  (invoke-static <buffer> 'findFile filename))

(define (find-file-other-window
	 #!optional (filename
		     (read-from-minibuffer "Find file in other window: ")))
  (switch-to-buffer-other-window (find-file-noselect filename)))

(define (find-file-other-frame
	 #!optional (filename
		     (read-from-minibuffer "Find file in other frame: ")))
  (switch-to-buffer-other-frame (find-file-noselect filename)))

(define (save-buffer #!optional (buffer :: <buffer> (current-buffer)))
  (if (buffer-file-name buffer)
      (invoke buffer 'save)
      (write-file (read-from-minibuffer "File to save in: ") buffer)))

(define (write-file #!optional (filename (read-from-minibuffer "Write-file: "))
                    (buffer (current-buffer)))
  (set-visited-file-name filename buffer)
  (save-buffer buffer))

(define (insert-file #!optional (filename (read-from-minibuffer "Insert file: "))
		     (buffer (current-buffer)))
  ((primitive-virtual-method <buffer> "insertFile"
                             <void> (<String>))
   buffer filename))

;;; BUFFERS

(define (pop-to-buffer buffer
		       #!optional not-this-window-p
		       (on-frame :: <frame> #!null))
  (select-window (display-window buffer not-this-window-p on-frame)))

(define (display-window (buffer :: <buffer>)
			#!optional not-this-window-p
			(on-frame :: <frame> #!null))
  (invoke buffer 'display not-this-window-p on-frame))

(define (current-buffer)
  (invoke-static <buffer> 'getCurrent))

;; Emacs allows a buffer name as well as a buffer.
(define (set-buffer buffer)
  (invoke-static <buffer> 'setCurrent buffer))

;; Emacs returns an Emacs string, not a Java string. 
(define (buffer-name #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <buffer> "getName"
			       <String> ())
   buffer))


(define (get-buffer buffer-or-name)
  (invoke-static <buffer> 'coerceBuffer buffer-or-name))

(define (generate-new-buffer-name starting-name)
  ((primitive-static-method <buffer> "generateNewBufferName"
			    <String> (<String>))
   starting-name))

(define (buffer-file-name #!optional (buffer (current-buffer)))
  (let ((name
         ((primitive-virtual-method <buffer> "getFileName"
                                    <java.lang.String> ())
          buffer)))
    (if (eq? name #!null)
        #f
        (symbol->string name))))

(define (set-visited-file-name filename #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <buffer> "setFileName" <void>
                             (<String>))
   buffer filename))

(define (get-buffer-create name)
  (let ((buf
	 ((primitive-static-method  <buffer> "getBuffer"
				    <buffer> (<String>))
	  name)))
    (if (eq? buf #!null)
	((primitive-constructor <buffer> (<String>)) name)
	buf)))

(define (generate-new-buffer name)
  ((primitive-constructor <buffer> (<String>))
   (generate-new-buffer-name name)))

;;; WINDOWS

(define (split-window #!optional (window (selected-window)) (size -1) (horizontal #f))
  ((primitive-virtual-method <window> "split"
			     <window> (<int> <boolean>))
   window size horizontal))

(define (split-window-vertically #!optional (arg -1))
  ; "Split current window into two windows, one above the other."
  ; (interactive "P")
  (split-window (selected-window) arg #f))

(define (split-window-horizontally #!optional (arg -1))
  ; "Split current window into two windows, one above the other."
  ; (interactive "P")
  (split-window (selected-window) arg #t))

(define (delete-window #!optional (window :: <window> (selected-window)))
  (invoke window 'delete))

(define (delete-other-windows #!optional (window (selected-window)))
  ((primitive-virtual-method <window> "deleteOtherWindows"
                             <void> ())
   window))

(define (selected-window)
  (invoke-static <window> 'getSelected))

(define (select-window (window :: <window>))
  (invoke-static <window> 'setSelected window)
  window)

;; Emacs allows extra options.
(define (next-window (window :: <window>))
  (invoke window 'getNextWindowInFrame 1))

;; Emacs allows some special values for frame.
(define (other-window #!optional (count 1) (frame (selected-frame)))
  (select-window
   ((primitive-virtual-method <gnu.jemacs.buffer.Window> "getNextWindowInFrame"
                              <gnu.jemacs.buffer.Window> (<int>))
    (frame-selected-window frame) count)))

(define (window-buffer #!optional (window :: <window> (selected-window)))
  (invoke window 'getBuffer))

(define (switch-to-buffer
         #!optional (buffer (read-from-minibuffer "Switch to buffer: ")))
  (let ((buf (get-buffer buffer)))
    (if (eq? buf #!null)
        (set! buf (generate-new-buffer buffer)))
    (set-buffer buf)
    (set-window-buffer (selected-window) buf)))

(define (switch-to-buffer-other-window (buffer :: <buffer>))
  (pop-to-buffer buffer #t (selected-frame)))

(define (switch-to-buffer-other-frame (buffer :: <buffer>))
  (pop-to-buffer buffer #f (make-frame buffer)))

(define (set-window-buffer (window :: <window>) (buffer :: <buffer>))
  (invoke window 'setBuffer (get-buffer buffer)))

(define (window-point (window :: <window>))
  (invoke window 'getPoint))

(define (set-window-point (window :: <window>) position)
  (invoke window 'setDot (invoke (invoke window 'getBuffer) 'positionToOffset position)))

(define (window-height #!optional (window :: <window> (selected-window)))
  (invoke window 'getHeightInLines))

(define (window-width #!optional (window :: <window> (selected-window)))
  (invoke window 'getWidthInColumns))

(define (window-pixel-height #!optional (window :: <window> (selected-window)))
  (invoke (invoke window 'getPanel) 'getHeight))

(define (window-pixel-width #!optional (window :: <window> (selected-window)))
  (invoke (invoke window 'getPanel) 'getWidth))

(define (window-text-area-pixel-height
	 #!optional (window :: <window> (selected-window)))
  (invoke window 'getHeight))

(define (window-text-area-pixel-width
	 #!optional (window :: <window> (selected-window)))
  (invoke window 'getWidth))

;;; FRAMES

(define (make-frame #!optional (buffer :: <buffer> (current-buffer)))
  (let ((frame (make <frame> buffer)))
    (set-menubar default-menubar)
    frame))

(define (delete-frame #!optional (frame :: <frame> (selected-frame)))
  (invoke frame 'delete))

;; Emacs:  frame-live-p
(define (frame-live? (frame :: <frame>))
  (invoke frame 'isLive))

(define (window-frame #!optional (window (selected-window)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Window> "getFrame"
                             <gnu.jemacs.buffer.Frame> ())
   window))

(define (frame-selected-window #!optional (frame (selected-frame)))
  ((primitive-virtual-method <gnu.jemacs.buffer.Frame> "getSelectedWindow"
                             <gnu.jemacs.buffer.Window> ())
   frame))

(define (selected-frame)
  (invoke-static <gnu.jemacs.buffer.Frame> 'getSelectedFrame))

;;; POSITIONS

(define (point #!optional (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'getPoint))

(define (point-min #!optional (buffer :: <buffer> (current-buffer)))
  (+ (invoke buffer 'minDot) 1))

(define (point-max #!optional (buffer :: <buffer> (current-buffer)))
  (+ (invoke buffer 'maxDot) 1))

(define (buffer-end flag #!optional (buffer :: <buffer> (current-buffer)))
  ((if (<= flag 0) point-min point-max) buffer))

(define (buffer-size #!optional (buffer :: <buffer> (current-buffer))) :: <int>
  (- (invoke buffer 'maxDot) (invoke buffer 'minDot)))

(define (goto-char position #!optional (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'setDot (invoke buffer 'positionToOffset position))
  (invoke buffer 'getDot))

#| Scheme only
(define (forward-char #!optional
		      (count :: <int> 1) (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'forwardChar count))

(define (backward-char #!optional
		      (count :: <int> 1) (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'backwardChar count))
|#

(define (point-at-bol
         #!optional
         (n  1)
         (buffer  :: <buffer> (current-buffer)))
  <int>
  (if (eq? n '()) (set! n 1))
  (+ 1 (as <int> (invoke buffer 'forwardLine (- n 1) (invoke buffer 'getDot)))))

(define (point-at-eol #!optional (count  :: <int> 1)
                      (buffer  :: <buffer> (current-buffer)))
  <int>
  (let* ((pos-shortage
          (invoke buffer 'scan
                  #\Newline (invoke buffer 'getDot)
                  0 (- count (if (> count 0) 0 1)) #t))
         (shortage (arithmetic-shift pos-shortage -32))
         (pos (as <int> pos-shortage)))
    (if (zero? shortage) pos (+ pos 1))))

;; FIXME - ineffecient!
(define (goto-line line #!optional (buffer  :: <buffer> (current-buffer)))
  (goto-char (point-min buffer) buffer)
  (forward-line (- line 1) buffer))

(define (beginning-of-line
         #!optional
         (n  1)
         (buffer :: <buffer> (current-buffer))) <void>
  (invoke buffer 'setPoint (point-at-bol n buffer)))

(define (end-of-line #!optional
                     (n :: <int> 1)
                     (buffer :: <buffer> (current-buffer)))
  <void>
  (invoke buffer 'setPoint (point-at-eol n buffer)))

(define (forward-line #!optional
                      (count :: <int> 1)
                      (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'forwardLine count))

#|
(define (next-line #!optional (count 1) (buffer :: <buffer> (current-buffer)))
  (line-move count))

(define (previous-line #!optional (count 1) (buffer :: <buffer> (current-buffer)))
  (line-move (- count)))
|#

(define (line-move arg #!optional (buffer ::  <buffer> (current-buffer)))
  (let ((goal-column (current-column buffer)))
    (forward-line arg buffer)
    (move-to-column goal-column #f buffer)))

;;; POSITIONS/EXCURSIONS

;;; MARKERS

(define (marker? x)
  (instance? x <marker>))

(define (make-marker)
  (make <marker>))

(define (point-marker #!optional
		      (share :: <boolean> #f)
		      (buffer :: <buffer> (current-buffer)))
  <marker>
  (invoke buffer 'getPointMarker share))

(define (copy-marker position #!optional kind)
  (let ((buffer :: <buffer>
		(if (marker? position)
		    (invoke (as <marker> position) 'getBuffer)
		    (current-buffer))))
    (if (eq? buffer #!null)
	(make <marker>)
	(make <marker>
	  buffer
	  (invoke buffer 'positionToOffset position)
	  (if kind 2 1)))))

(define (marker-position (marker <marker>))
  (let ((value (invoke marker 'getPoint)))
    (if (= value 0) #f value)))

(define (marker-buffer (marker <marker>))
  (invoke marker 'getBuffer))

(define (set-marker (marker <marker>) position
                    #!optional (buffer :: <buffer> (current-buffer)))
  (invoke marker 'set buffer (invoke buffer 'positionToOffset position)))

;;; TEXT

(define (insert-char ch #!optional (count :: <int> 1)
		     (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'insert ch count #!null))

(define (insert #!rest (args :: <Object[]>))
  (let ((buffer :: <buffer> (current-buffer)))
    (invoke buffer 'insertAll args #!null)))

(define (erase-buffer #!optional buffer '())
  (let ((buf :: <buffer> (if (eq? buffer '()) (current-buffer)
                             (get-buffer buffer))))
    (invoke buf 'removeAll)))

(define (delete-region start end
		       #!optional (buffer  :: <buffer> (current-buffer)))
  (let ((start-offset :: <int>
		      (invoke buffer 'positionToOffset start))
	 (end-offset :: <int>
		     (invoke buffer 'positionToOffset end)))
  (invoke buffer 'removeRegion start-offset end-offset)))

(define (delete-char #!optional (count :: <int> 1) killp
		     (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'deleteChar count))

(define (delete-backward-char #!optional (count 1) killp
			      (buffer :: <buffer> (current-buffer)))
  (delete-char (- count) killp buffer))

;;; TEXT/COLUMNS

(define (current-column #!optional (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'currentColumn))

(define (move-to-column column
			#!optional force (buffer :: <buffer> (current-buffer)))
  (invoke buffer 'moveToColumn column force))

;;; PROCESSES

(define (process? obj)
  (instance? x <process>))

(define (process-send-string (process :: <process>) string)
  (invoke process 'writeToInferior string))

(define (process-buffer (process :: <process>))
  (invoke process 'getBuffer))

(define (process-mark (process :: <process>))
  (invoke process 'getProcessMark))

(define (get-buffer-process buffer-or-name)
  (invoke-static <process> 'getProcessMode (get-buffer buffer-or-name)))

;;; NETWORK CONNECTION

; (define (open-network-stream name buffer-or-name host service) ...)

;;; DEFAULT BINDINGS

(define (emacs-help)
  (format #t "Sorry - no help available.~%~!"))

#|
(define-key global-map #(backspace) delete-backward-char)
(define-key global-map #(down) next-line)
(define-key global-map #(up) previous-line)
(define-key global-map #(left) backward-char)
(define-key global-map #(right) forward-char)
(define-key global-map "\C-a" beginning-of-line)
(define-key global-map "\C-b" backward-char)
(define-key global-map "\C-n" next-line)
(define-key global-map "\C-p" previous-line)
(define-key global-map "\C-d" delete-char)
(define-key global-map "\C-e" end-of-line)
(define-key global-map "\C-f" forward-char)
(define-key global-map "\C-x\C-w" write-file)
(define-key global-map "\C-x\C-s" save-buffer)
(define-key global-map "\C-x0" delete-window)
(define-key global-map "\C-x1" delete-other-windows)
(define-key global-map "\C-x2" split-window-vertically)
(define-key global-map "\C-x3" split-window-horizontally)
(define-key global-map "\C-x4f" find-file-other-window)
(define-key global-map "\C-xb" switch-to-buffer)
(define-key global-map "\C-xi" insert-file)
(define-key global-map "\C-x\C-f" find-file)
(define-key global-map "\C-x50" delete-frame)
(define-key global-map "\C-x52" make-frame)
(define-key global-map "\C-x5f" find-file-other-frame)
(define-key global-map "\C-xo" other-window)
(define-key global-map '(control h) emacs-help)
|#

(define default-menubar
  (list
   (list "File"
	 (vector "Open..." find-file)
	 (vector "Open in Other Window..." find-file-other-window)
	 (vector "Open in New Frame..." find-file-other-frame)
	 #("Insert File..." insert-file )
	 ;; #("View File..." view-file )
	 "------"
	 (vector "New Frame" make-frame)
	 (vector "Delete Frame" delete-frame)
	 "------"
	 #("Save" save-buffer active: (buffer-modified-p)))
   (list "Tools"
	 (vector "Scheme interaction" scheme-swing-window))
   #!null
   (list "Help"
	 #( "About JEmacs..." about-jemacs ) )))

(define (emacs)
  (set-buffer (get-buffer-create "*scratch*"))
  (make-frame))

;;; REPL

(define (term-send-input #!optional (buffer (current-buffer)))
  ((primitive-virtual-method <gnu.jemacs.buffer.ReplBuffer> "enter"
                             <void> ())
   buffer))

(define repl-map (make-keymap 'repl-map))
(define-key repl-map "\r" term-send-input)
(define-key repl-map "\n" term-send-input)
(define-key repl-map 'return term-send-input)

;;; TELNET

(define (telnet #!optional (host (read-from-minibuffer "Open telnet connecttion to host:"))
		(port :: <int> 23))
  (let ((buffer (get-buffer-create "*Telnet*")))
    (invoke-static <gnu.jemacs.buffer.TelnetMode> 'telnetMode buffer host port)
    (use-local-map (static-field <process> 'modeMap) buffer)
    (switch-to-buffer buffer)
    buffer))    

(define (shell #!optional (cmd "/bin/bash"))
  (let ((buffer (get-buffer-create "*Shell*")))
    (invoke-static <gnu.jemacs.buffer.InfProcessMode> 'shellMode buffer cmd)
    (use-local-map (static-field <process> 'modeMap) buffer)
    (switch-to-buffer buffer)
    buffer))    

(define (scheme-swing-window)
  (let ((buffer
         (invoke-static <gnu.jemacs.buffer.ReplBuffer> 'make 'scheme)))
    (use-local-map repl-map buffer)
    ; (make-frame buffer)
    (switch-to-buffer buffer)
    buffer))

(define (decode-buffer buffer)
  (if (eq? '() buffer) (current-buffer)
      (get-buffer buffer)))
