;;;; -*- indent-tabs-mode: nil -*-

(require 'epg)
(require 'url)
(require 'cl)

(defun encrypt (str for-nick)
  (let ((context (epg-make-context 'OpenPGP)))
    (epg-context-set-armor context t)
    (epg-encrypt-string
     context
     (encode-coding-string str 'utf-8)
     (epg-list-keys context for-nick))))

(defun decrypt (str)
  (let ((context (epg-make-context 'OpenPGP)))
    (epg-context-set-armor context t)
    (epg-decrypt-string context str)))

(defun erc-cmd-SENDENCRYPTED (nick &rest words)
  (let* ((message (mapconcat 'identity words " ")) 
         (keyname (or (cdr (assoc nick *nickname-to-keyname*)) nick))) ;;TODO cdrassoc
    (encrypt-and-paste-string message keyname
                              (lambda (url nick buffer)
                                (with-current-buffer buffer
                                  (erc-send-message 
                                   (format "%s: PGP MESSAGE %s" nick url))))
                              `(,nick ,(current-buffer)))))

(defun find-buffer (name) (dolist (b (erc-buffer-list)) (if (equalp (buffer-name b) name) (return b))))

(defvar *nickname-to-keyname* nil
  "An alist associating irc nicknames with key identifiers")
;TODO: move these to .ercrc.el
(push '("bavardage" . "jebavarde") *nickname-to-keyname*)
(push '("bavfoo" . "jebavarde") *nickname-to-keyname*)
(push '("bavbar" . "jebavarde") *nickname-to-keyname*)



(defun listen-for-gpg-message (process parsed &rest ignore)
  (let* ((sspec (aref parsed 1))
         (nick (substring (nth 0 (erc-parse-user sspec)) 1))
         (tgt (car (aref parsed 4)))
         (true-target (if (equal tgt (erc-current-nick))
                          nick tgt))
         (msg (aref parsed 5)))
    (when (string-match "\\(.+\\): PGP MESSAGE \\(.+\\)" msg)
      (when (equal (erc-current-nick) (match-string 1 msg))
        (erc-display-message nil 'notice
                             (find-buffer true-target)
                             (format "Got an encrypted message from %s" nick))
        (get-url-contents
         (match-string 2 msg)
         (lambda (ciphertext true-target)
           (erc-display-message nil 
                                'notice 
                                (find-buffer true-target)
                                (format "Message decrypts as: %s"
                                        (decode-coding-string
                                         (decrypt ciphertext)
                                         'utf-8))))
         `(,true-target))))))

(add-hook 'erc-server-PRIVMSG-functions 'listen-for-gpg-message)


(setf *gpg-pastebin-function* 'paste-to-dpaste)
(defun encrypt-and-paste-string (str for-nick callback cbargs)
  "Encrypts and pastes the message, returns the url for the message"
  (apply *gpg-pastebin-function* `(,(encrypt str for-nick) ,callback ,cbargs)))

(defun paste-to-dpaste (str callback cbargs)
  "Pastes str to dpaste and calls callback with the url to the raw paste"
  (let ((dpaste-url "http://dpaste.com/api/v1/")
        (url-max-redirections 10)
        (url-request-method "POST")
        (url-request-data (make-query-string `(("content" . ,str)))))
    (url-retrieve 
     dpaste-url
     (lambda (status callback cbargs)
       (with-current-buffer (current-buffer)
         (let* ((paste (buffer-string))
                (ignore (string-match "\\/\\([0-9]+\\)\\/plain\\/" paste))
                (paste-id (match-string 1 paste))
                (url (format "http://dpaste.com/%s/plain/" paste-id)))
           (apply callback `(,url ,@cbargs)))))
     `(,callback ,cbargs))))


(defun make-query-string (params)
  "Returns a query string constructed from PARAMS, which should be
a list with elements of the form (KEY . VALUE). KEY and VALUE
should both be strings."
  (mapconcat
   (lambda (param)
     (concat (url-hexify-string (car param)) "="
             (url-hexify-string (cdr param))))
   params "&"))

(defun get-url-contents (url callback cbargs)
  (url-retrieve url
                (lambda (status callback cbargs)
                  (with-current-buffer (current-buffer)
                    (beginning-of-buffer)
                    (search-forward-regexp "\n\n")
                    (delete-region (point-min) (point))
                    (apply callback `(,(buffer-string) ,@cbargs))))
                `(,callback ,cbargs)))

(provide 'erc-gpg)