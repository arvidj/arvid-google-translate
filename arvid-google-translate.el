;; TODO: Set translation to read-only
;; TODO: How to make more stable:
;; TODO 1) Only translate after period of
;; idle-ness 2) Only run one request at a time. How to control
;; requests?
;; TODO Memoize translations
;; TODO Add settings for controlling languages,
;; TODO Display current language target and source language in mode line?
;; TODO Still trouble with encoding. "Jag het" sv -> fre "J&#39;ai chaud"


(require 'url)
(require 'url-http)
(require 'json)

(defvar agt-mode nil
  "Enables the google translate minor mode")

(defvar agt-window-config nil
  "Old window configuration")

(defvar agt-timer-ref nil
  "Old window configuration")

(defvar agt-async-buffer 'nil
  "Buffer that stores the temporary JSON result for arvid-google-translate.")

(defconst agt-google-translate-backend-url
  "http://ajax.googleapis.com/ajax/services/language/translate")

(defconst agt-timer-timeout "0.3s"
  "Old window configuration")

(defconst agt-buffer-source "*AGT-Source*"
  "Buffer to use for the AGT source.")

(defconst agt-buffer-translation "*AGT-Translation*"
  "Buffer to use for the AGT translation.")

(setq agt-mode-map (make-sparse-keymap))
(defvar agt-mode-map (make-sparse-keymap)
  "Keymap used by AGT.")

(defun agt-quit ()
  ""
  (interactive)
  (message "quit")

  ;; (bury-buffer agt-buffer-source)
  ;; (bury-buffer agt-buffer-translation)

  (kill-buffer agt-buffer-source)
  (kill-buffer agt-buffer-translation)

  ;; Kill async buffer if it is still hanging around.
  (when (bufferp agt-async-buffer)
	(kill-buffer agt-async-buffer))


  ;; TODO Save custom setting target / source here instead.

  (set-window-configuration agt-window-config))

(defun agt-translate ()
  ""
  (interactive)
  (message "translate"))

(define-derived-mode agt-mode nil "Arvid Google Translate"
  "Major mode for interactive google translations."
  (message "agt-mode")

  ;; This stuff can be moved into def-var.
  (define-key agt-mode-map "\C-c\C-q" 'agt-quit)
  (define-key agt-mode-map "\C-c\C-t" 'agt-translate)

  (use-local-map agt-mode-map)
  (add-hook 'after-change-functions 'agt-auto-update nil t))

(defun agt-auto-update (pos end len)
  ;; (message "update")

  (let ((text (agt-get-source-buffer-text)))
	;; TODO should unset timer even though buffer text is empty.
	(if (not (> (length text) 0))
		(set-translation-buffer-content "")
	  (when (and (boundp 'agt-timer-ref) agt-timer-ref)
		(cancel-timer agt-timer-ref)
		(setq agt-timer-ref nil))
	  (setq agt-timer-ref
			(run-at-time "0.1 sec" nil 'agt-timer-callback)))))

(defun agt-get-source-buffer-text ()
  (save-current-buffer
	(set-buffer agt-buffer-source)
	(buffer-string)))

(defun agt-timer-callback ()
  (agt-translate-async
   (agt-get-source-buffer-text)
   'agt-update-translation-buffer-callback
   "fr"
   "en"))

(defun agt-update-translation-buffer-callback (status text)
  ""
  (set-translation-buffer-content text))

(defun set-translation-buffer-content (content)
  ""
  (save-current-buffer
	(set-buffer agt-buffer-translation)
	;; TODO Would using replace substring reduce flickering?
	(erase-buffer)
	(insert content)))

(defun agt ()
  "Translate interactively"
  (interactive)

  (setq agt-window-config (current-window-configuration))
  (setq source-win (split-window (selected-window) (- (window-height) 4)))
  (setq translation-win (split-window source-win (/ (window-width source-win) 2) t))

  (select-window translation-win)
  (switch-to-buffer (get-buffer-create agt-buffer-translation))

  ;; TODO how to set this and insert at the same time?
  ;; (setq buffer-read-only t)

  (select-window source-win)
  (switch-to-buffer (get-buffer-create agt-buffer-source))
  (agt-mode))

;; Example query:
;; http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&q=hello%20world&langpair=en%7Ci
(defun agt-translate-async (str callback from to)
  "Returns the buffer. Need to parse json and but in translation buffer."
  (let* ((url-request-method "GET")
         (url (concat agt-google-translate-backend-url
					   "?v=1.0&q=" (url-hexify-string str)
					   "&langpair=" from "%7C" to))
         (url-show-status nil))
    (setq agt-async-buffer (url-retrieve url 'agt-handle-response (list callback)))))

(defun agt-handle-response (status callback)
  ""
  ;; TODO check status of response, however, status is always nil :C
  ;; TODO Some really sketchy error-handling here.
  (save-current-buffer
  	(when (bufferp agt-async-buffer)
	  (set-buffer agt-async-buffer)
	  (when (> (length (buffer-string)) 0)
		(goto-char (point-max))
		(beginning-of-line)
		(condition-case nil
			(let* ((json-string (buffer-substring (point) (point-max)))
				   (json-string-decoded (decode-coding-string json-string 'utf-8))
				   (json-res (json-read-from-string json-string-decoded))
				   (translatedText (cdr (assq 'translatedText
											  (assoc 'responseData json-res))))
				   (translatedTextDecoded (decode-entities (url-unhex-string translatedText))))
			  (kill-buffer agt-async-buffer)
			  (funcall callback status translatedTextDecoded))
		  (error
		   (message "error")
		   (print (buffer-string))
		   (print status)
		   (kill-buffer agt-async-buffer)
		   (agt-quit)
		   )))
  	  )))

(defun decode-entities (str)
  str)
  ;; (decode-numerical-entities (decode-string-entities str)))

(defun decode-string-entities (str)
  ""
  (interactive)
  ;; TODO Could use some fold here
  (mapc '(lambda (pair)
		   (let ((char (car pair))
				 (enti (cdr pair)))
			 (setq str (gsub enti char str))))
		'(("\"" . "&quot;")
		  ("&" . "&amp;")
		  ("<" . "&lt;")
		  (">" . "&gt;")))
  str)

(defun decode-numerical-entities (string)
  "Interprets a string of space-separated numbers as an
ASCII string"
  (let ((start 0))
	(while (string-match "&#\\([[:digit:]]\\)+;" string start)
	  (let (replace (format "%c" (string-to-number (match-string 1))))
		(setq string (replace-match
					  (format "%c" (string-to-number match))
					  nil t string 1))
		(setq start (+ (match-end 0) (- (length replace)
										(length search))))))
	string))

(defun clean-encoded-string (string)
  "Removes useless characters and replaces linebreaks with a
line-feed (LF) char."
  (gsub ";" " "                                   ; add an empty space to split-string later
   (gsub "&#" ""                                  ; first 2 characters are useless
    (gsub "\\(\n\\|<br />\\)" "&#10; " string)))) ; replace line break with LF (10)

(defun gsub (what with in)
  (replace-regexp-in-string (regexp-quote what) with in))

