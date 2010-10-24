;; TODO: Set translation to read-only
;; TODO: How to make more stable:
;; TODO 1) Only translate after period of
;; idle-ness 2) Only run one request at a time. How to control
;; requests?
;; TODO Memoize translations
;; TODO Display current language target and source language in mode line?
;; TODO Fails silently when internet is down.
;; TODO Why store and replace window configuration? Why not just delete both used windows?
;; TODO Do not store current source and target language in custom variables. Only fetch on load and store on quit.
;; TODO Write doc-strings ;)
;; TODO Implement support for auto-recognition of source language.
;; TODO Switching languages should re-translate.

;; Remove later
(add-to-list 'load-path "~/.emacs.d/plugins/arvid-google-translate")

(require 'url)
(require 'url-http)
(require 'json)
(require 'arvid-google-translate-languages)

(defvar agt-mode nil
  "Enables the google translate minor mode")

(defvar agt-window-config nil
  "Old window configuration")

(defvar agt-timer-ref nil
  "Old window configuration")

(defvar agt-async-buffer nil
  "Buffer that stores the temporary JSON result for arvid-google-translate.")

;; TODO We can create our own customize types to provide useful
;; completion here.
(defgroup agt nil
  "Customization for arvid-google-translate")
(defcustom agt-language-source nil
  "The language code of the source language"
  :type 'string
  :group 'agt-google-translate)
(defcustom agt-language-target nil
  "The language code of the target language"
  :type 'string
  :group 'agt-google-translate)
(defcustom agt-use-ido nil
  "Use ido for language selection prompts."
  :type 'boolean
  :group 'agt-google-translate)

(defconst agt-google-translate-backend-url
  "http://ajax.googleapis.com/ajax/services/language/translate")

(defconst agt-timer-timeout "0.3s"
  "Old window configuration")

(defconst agt-buffer-source "*AGT-Source*"
  "Buffer to use for the AGT source.")

(defconst agt-buffer-translation "*AGT-Translation*"
  "Buffer to use for the AGT translation.")

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
  ;; (message "agt-mode")

  ;; This stuff can be moved into def-var.
  (define-key agt-mode-map "\C-c\C-q" 'agt-quit)
  (define-key agt-mode-map "\C-j" 'agt-translate)

  (define-key agt-mode-map "\C-c\C-l" 'agt-read-languages)
  (define-key agt-mode-map "\C-c\C-s" 'agt-read-language-source)
  (define-key agt-mode-map "\C-c\C-t" 'agt-read-language-target)
  (define-key agt-mode-map "\C-c\C-w" 'agt-swap-languages)
  (use-local-map agt-mode-map))

(defun agt-auto-update (pos end len)
  ;; (message "update")

  (let ((text (agt-get-source-buffer-text)))
	;; TODO should unset timer even though buffer text is empty.
	(if (not (> (length text) 0))
		(agt-set-target-buffer-text "")
	  (when (and (boundp 'agt-timer-ref) agt-timer-ref)
		(cancel-timer agt-timer-ref)
		(setq agt-timer-ref nil))
	  (setq agt-timer-ref
			(run-at-time "0.1 sec" nil 'agt-timer-callback)))))

(defun agt-get-source-buffer-text ()
  (save-current-buffer
	(set-buffer agt-buffer-source)
	(buffer-string)))

(defun agt-get-target-buffer-text ()
  (save-current-buffer
	(set-buffer agt-buffer-translation)
	(buffer-string)))

(defun agt-timer-callback ()
  (agt-translate-async
   (agt-get-source-buffer-text)
   'agt-update-translation-buffer-callback
   agt-language-source
   agt-language-target))

(defun agt-update-translation-buffer-callback (status text)
  ""
  (agt-set-target-buffer-text text))


(defun agt-set-buffer-text (buffer text)
  ""
  (save-current-buffer
	(set-buffer buffer)
	;; (setq buffer-read-only nil)
	;; TODO Would using replace substring reduce flickering?
	(erase-buffer)
	(insert text)
	;; (setq buffer-read-only t)
	))
(defun agt-set-target-buffer-text (content)
  (agt-set-buffer-text agt-buffer-translation content))
(defun agt-set-source-buffer-text (content)
  (agt-set-buffer-text agt-buffer-source content))

(defun completing-read-language-into-custom (prompt var)
  (let ((choosen-language (agt-completing-read-language-name prompt)))
	(customize-save-variable var (agt-get-language-code choosen-language))))

(defun agt-completing-read-language-name (prompt)
  (if agt-use-ido
	  (ido-completing-read prompt (agt-get-language-names))
	(completing-read prompt agt-available-languages)))


(defun agt-swap-languages ()
  (interactive)
  (let ((old-source agt-language-source)
		(old-source-text (agt-get-source-buffer-text)))

	;; Switch source and target language.
	(customize-save-variable 'agt-language-source agt-language-target)
	(customize-save-variable 'agt-language-target old-source)

	;; Switch source and target text.
	(agt-set-source-buffer-text (agt-get-target-buffer-text))
	(agt-set-target-buffer-text old-source-text)))

(defun agt-read-languages ()
  (interactive)
  (agt-read-language-source)
  (agt-read-language-target))

(defun agt-read-language-target ()
  (interactive)
  (completing-read-language-into-custom "Target language: " 'agt-language-target))

(defun agt-read-language-source ()
  (interactive)
  (completing-read-language-into-custom "Source language: " 'agt-language-source))

(defun agt ()
  "Translate interactively"
  (interactive)

  ;; Get target and source language.
  (unless (agt-available-language-code agt-language-source)
	(agt-read-language-source))
  (unless (agt-available-language-code agt-language-target)
	(agt-read-language-target))
  ;; (message (concat agt-language-source " -> " agt-language-target))

  (when (and agt-language-target agt-language-source)
	;; Store old window configuration
	(setq agt-window-config (current-window-configuration))

	;; Setup our new windows
	(setq source-win (split-window (selected-window) (- (window-height) 4)))
	(setq translation-win (split-window source-win (/ (window-width source-win) 2) t))

	;; Setup the translation buffer in the translation window
	(select-window translation-win)
	(switch-to-buffer (get-buffer-create agt-buffer-translation))
	(agt-mode)

	;; TODO how to set this and insert at the same time?
	;; (setq buffer-read-only t)

	;; Setup the source buffer in the source window
	(select-window source-win)
	(switch-to-buffer (get-buffer-create agt-buffer-source))
	(agt-mode)
	(add-hook 'after-change-functions 'agt-auto-update nil t)))

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
  (decode-numerical-entities (decode-string-entities str)))


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
	(while (string-match "&#\\([[:digit:]]+\\);" string start)
	  (let ((whole-match (match-string 0 string))
			(replace (format "%c" (string-to-number (match-string 1 string)))))
		(setq string (replace-match replace nil t string))
		(setq start (+ (match-end 0) (- (length replace)
										(length whole-match))))))
	string))

(defun clean-encoded-string (string)
  "Removes useless characters and replaces linebreaks with a
line-feed (LF) char."
  (gsub ";" " "                                   ; add an empty space to split-string later
   (gsub "&#" ""                                  ; first 2 characters are useless
    (gsub "\\(\n\\|<br />\\)" "&#10; " string)))) ; replace line break with LF (10)

(defun gsub (what with in)
  (replace-regexp-in-string (regexp-quote what) with in))

