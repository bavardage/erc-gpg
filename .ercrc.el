(require 'epg)

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
    (epg-decrypt-string
     context
     str)))

(defun erc-cmd-TEST (&rest ignore)
  (let* ((ciphertext (encrypt "hello this is only a test" "jebavarde"))
	 (lines (split-string ciphertext "\n")))
    (dolist (line lines)
      (erc-send-message line))))

(defun erc-cmd-SENDENCRYPTED (recipient &rest words)
  (let* ((message (mapconcat 'identity words " "))
	 (ciphertext (encrypt message recipient))
	 (lines (split-string ciphertext "\n")))
    (dolist (line lines)
      (erc-send-message line))))
    

(setf *gpg-message-cache* (make-hash-table :test #'equal))
(setf *gpg-nicks-sending-messages* nil)
(setf *gpg-nicks-to-channels* (make-hash-table :test #'equal))

(defun add-line (nick line)
  (push line (gethash nick *gpg-message-cache*)))

(defun clear-lines (nick)
  (setf (gethash nick *gpg-message-cache*) nil))

(defun get-encrypted-message (nick)
  (let* ((lines (reverse (gethash nick *gpg-message-cache*)))
	 (ciphertext-body (mapconcat 'identity
				     (cdr lines)
				     "\n")))
    (mapconcat 'identity (list
			  "-----BEGIN PGP MESSAGE-----"
			  (car lines)
			  ""
			  ciphertext-body
			  "-----END PGP MESSAGE-----")
	       "\n")))

(defun get-and-clear-encrypted-message (nick)
  (let ((message (get-encrypted-message nick)))
    (clear-lines nick)
    message))

(defun listen-for (nick chan)
  (print "receiving encrypted message...")
  (setf (gethash nick *gpg-nicks-to-channels*) chan)
  (unless (member nick *gpg-nicks-sending-messages*)
    (push nick *gpg-nicks-sending-messages*)))
(defun stop-listen-for (nick)
  (setf *gpg-nicks-sending-messages*
	(remove nick *gpg-nicks-sending-messages*)))

(defun find-buffer (name) (dolist (b (erc-buffer-list)) (if (equalp (buffer-name b) name) (return b))))

(defun message-over (nick)
  (stop-listen-for nick)
  (print "decrypting ciphertext...")
  (let* ((ciphertext (get-and-clear-encrypted-message nick))
	 (plaintext (decrypt ciphertext))
	 (buffer-name (gethash nick *gpg-nicks-to-channels*))
	 (buffer (find-buffer buffer-name)))
    (print plaintext)
    (erc-display-message nil 'notice buffer plaintext))) ;;WTF IS THE BUFFER I SHOULD USE HERE?
						
(defun gpg-process (process parsed &rest ignore)
  (let* ((sspec (aref parsed 1))
         (nick (substring (nth 0 (erc-parse-user sspec)) 1))
         (tgt (car (aref parsed 4)))
         (msg (aref parsed 5)))
    (cond
     ((equal msg "-----BEGIN PGP MESSAGE-----")
      (listen-for nick tgt)) ;;tgt isn't quite right :| since for queries it breaks
     ((equal msg "-----END PGP MESSAGE-----")
      (message-over nick))
     ((member nick *gpg-nicks-sending-messages*)
      (add-line nick msg)))))
    

(add-hook 'erc-server-PRIVMSG-functions 'gpg-process)

