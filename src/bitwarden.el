;;; bitwarden.el --- Emacs wrapper for the official bitwarden CLI -*- lexical-binding: t -*-

;; Copyright (C) 2024 Leonardo Tamiano

;; Author: Leonardo Tamiano <leonardotamiano95@gmail.com>
;; Homepage: https://github.com/LeonardoE95/password-manager.el
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <https://www.gnu.org/licenses/>.

;;
;; This is an extremely simple Emacs wrapper over 'bw', the official
;; CLI client for bitwarden. Currently, only read access is supported
;; and one time at a time can be loaded to avoid exposure of sensitive
;; data.
;;
;; ------------------------------------------------------------------------------------

(require 'transient)
(require 'json)
(require 'seq)
(require 'timer)
(require 'select)
(require 'ivy)

;; --------------------------

(defvar bw/binpath "/usr/bin/bw"
  "Path to the bitwarden CLI binary")

(defvar bw/email ""
  "Email to use for login.")

;; timeout parameters
(defvar bw/session-timeout "1 hour"
  "Timeout parameter that determines after how much time the session is terminated.")
(defvar bw/vault-timeout "30 minutes"
  "Timeout parameter that determines after how much time the vault is locked.")
(defvar bw/item-timeout "5 minutes"
  "Timeout parameter that determines after how much time the loaded item is removed from memory.")
(defvar bw/clipboard-timeout "1 minutes"
  "Timeout parameter that determines after how much time the system clipboard is cleaned.")

(defvar bw/msg-error "Username or password is incorrect. Try again."
  "Message error used to determine if the login attempt was succesful or not.")
(defvar bw/msg-already-logged "You are already logged in"
  "Message error used to determine if the login attempt was succesful or not.")

;; session data
(defvar bw/token nil
  "Contains token for current session.")
(defvar bw/vault nil
  "Boolean that represent the state of the vault. t for locked, nil for unlocked.") 
(defvar bw/item-names nil
  "List of strings, each of which represent an item in the bitwarden vault.")
(defvar bw/item-curr nil
  "Stores currently loaded item.")

;; --------------------------

(defun bw/cmd-anon-to-string (cmd)
  "Execute a bitwarden command without the session token and return
a string of the output"
  ;; TODO: add validation 
  (shell-command-to-string (format "%s %s" bw/binpath cmd))
  )

(defun bw/cmd-auth-to-string (cmd)
  "Execute a bitwarden command using the session token saved
 'bw/token' and return a string of the output"
  ;; TODO: add validation 
  (with-environment-variables (("BW_SESSION" bw/token))
    (shell-command-to-string (format "%s %s" bw/binpath cmd))
    )
  )

;; --------------------------

(defun bw/login (&optional email password)
  (interactive)

  (defun bw/handle-login (proc output)
    "Handle output from the login procedure."
    (when (not (string-match-p (regexp-quote "Master password") output))
      (if (not (or (string-match-p (regexp-quote bw/msg-error) output)
		   (string-match-p (regexp-quote bw/msg-already-logged) output)))
	  (progn
	    (message "Succesfully logged in!")
	    
	    ;; only set bw token in case of successful
	    (setq bw/token output)
	    (setq bw/item-curr nil)

	    ;; tell wrapper that currently the password manager to use
	    ;; is bitwarden.el. Not sure if I want this variable to be
	    ;; set here, but for now it will do.
	    (setq pm/current-wrapper 'bw)

	    ;; clear state after proper timeouts
	    (run-at-time bw/vault-timeout nil (lambda () (bw/vault-lock) ))
	    (run-at-time bw/session-timeout nil (lambda () (bw/logout) ))      

	    ;; load items names immediately so we save time later on
	    (bw/load-items-names)
	    (message "Loaded item names!")

	    ;; immediately perform a full flow after the first login
	    ;; to speed things up
	    (bw/execute-flow)
	    )
	(progn
	  (message "Could not log: %s" output)
	  )
	)      
      )
    )

  ;; make sure we're logged out to not interfere with
  ;; existing sessions
  (bw/logout)

  ;; Initialize values regardless of login outcome
  (progn
    (setq bw/token nil)
    (setq bw/vault nil)
    (setq bw/item-names nil)
    (setq bw/item-curr nil)
    )

  (let* ((bw-email (if email email
		     (read-from-minibuffer "Email: " (if bw/email bw/email ""))))
	 (bw-password (if password password
			(read-passwd "Master Password: ")))
	 (process-name "bw")
	 (buffer-name "*bw*")
	 (process (start-process process-name buffer-name "bw" "login" bw-email "--raw"))
	 )

    ;; save email for next login
    (setq bw/email bw-email)
    ;; 
    ;; send password without putting it into the cmdline to avoid
    ;; exposure of confidential data
    ;;
    (set-process-filter process #'bw/handle-login)
    (process-send-string process (format "%s\n" bw-password))
    (process-send-eof process)
    )
  )

(defun bw/logout ()
  (interactive)
  (bw/cmd-auth-to-string (format "logout"))
  (setq bw/token nil)
  (setq bw/vault nil)  
  (message "Logged out!")
  )

(defun bw/session-check ()
  (interactive)
  (not (string=
   "You are not logged in."
   (bw/cmd-auth-to-string (format "login --check"))
   ))
  )

(defun bw/vault-lock ()
  (interactive)
  (bw/cmd-auth-to-string (format "lock"))
  (setq bw/vault t)
  (setq bw/item-curr nil)
  (message "Vault locked!")
  )

(defun bw/vault-unlock ()
  (interactive)
  (defun bw/handle-unlock (proc output)
    "Handle output from the unlock procedure."
    (when (not (string-match-p (regexp-quote "Master password") output))
      (if (not (or (string-match-p (regexp-quote bw/msg-error) output)
		   (string-match-p (regexp-quote bw/msg-already-logged) output)))
	  (progn
	    ;; only set bw token in case of successful
	    (message "Vault unlocked!")
	    (setq bw/token output)
	    (setq bw/vault nil)
	    (run-at-time bw/vault-timeout nil (lambda () (bw/vault-lock) ))
	    )
	(message "Could not unlock vault: %s" output)
	)
      )
    )  
  (let* ((bw-password (read-passwd "Unlock vault: "))
	 (process-name "bw")
	 (buffer-name "*bw*")
	 (process (start-process process-name buffer-name "bw" "unlock" "--raw"))
	 )

    (set-process-filter process #'bw/handle-unlock)
    (process-send-string process (format "%s\n" bw-password))
    (process-send-eof process)
    )
  )

;; --------------------------

(defun bw/execute-flow ()
  "Execute specific flow of actions by interactivly asking user what
to do. This is executed as soon as a login is made to speed up
the first flow."
  (let* ((ivy-sort-functions-alist nil)
	 (action (read (ivy-read "Select action: " '(none password uri+password))))
	 )

    (cond
     ;; no action, simply return
     ((eq action 'none)
      nil
      )

     ;; load an item interactively and read password
     ((eq action 'password)
      (progn
	(bw/load-item)
	(bw/read-password)
	)
      )

     ;; load an item interactively and read password
     ((eq action 'uri+password)
      (progn
	(bw/load-item)
	(bw/read-uris)
	)
      )
     )

    )
  )

;; --------------------------

(defun bw/load-items-names ()
  (interactive)
  (let* ((bw-items (json-read-from-string (bw/cmd-auth-to-string (format "list items"))))
	 (bw-names (mapcar (lambda (entry) (assoc-default 'name entry)) bw-items))
	 )
    (setq bw/item-names bw-names)
    )
  )

(defun bw/load-item ()
  (interactive)
  (setq bw/item-curr (bw/select-item))
  (run-at-time bw/item-timeout nil
	       (lambda ()
		 (setq bw/item-curr nil)
		 ))

  (run-at-time bw/item-timeout nil
	       (lambda ()
		 (setq bw/item-curr nil)
		 ))
  
  bw/item-curr
  )

(defun bw/select-item ()
  (interactive)

  ;; if the session is expired, we have to login again
  (when (not (bw/session-check))
    (message "Session is over")
    (bw/login))

  ;; if the vault is locked, we have to unlock it
  (when bw/vault
    (bw/vault-unlock)
    )

  ;; if item list is expired, we have to download it again
  (when (not bw/item-names)
    (bw/load-items-names)
    )

  (let* ((selected-name (ivy-read "Name: " bw/item-names))
	 (output-cmd (bw/cmd-auth-to-string (format "list items --search %s" selected-name)))
	 (selected-item (if (string= "You are not logged in." output-cmd)
			    nil
			  (seq-filter
                           (lambda (e) (string= (assoc-default 'name e) selected-name))
                           (json-read-from-string output-cmd))))
	 )

    (message "Loaded item %s" selected-name)
    selected-item
    )
  )

;; --------------------------

(defun bw/item-name (item)
  "Extract item name from the item structure."
  (if item
      (assoc-default 'name (car item))
    nil)
  )

(defun bw/item-id (item)
  "Extract item id from the item structure."
  (if item
      (assoc-default 'id (car item))
    nil)
  )

(defun bw/item-username (item)
  "Extract username from the item structure."
  (if item
      (assoc-default 'username (assoc-default 'login (car item)))
    nil)
  )

(defun bw/item-password (item)
  "Extract password from the item structure."
  (if item 
      (assoc-default 'password (assoc-default 'login (car item)))
    nil)
  )

(defun bw/item-uris (item)
  "Extract URIs from the item structure."
  (if item
      (cl-coerce
       (assoc-default 'uris (assoc-default 'login (car item)))
       'list)
    nil)
  )

(defun bw/item-label (item)
  "Compute simple string representation for showing the item loaded in the transient UI."
  (format "%s/%s"
	  (bw/item-name item)
	  (bw/item-username item))
  )

(defun bw/copy-to-clipboard (data)
  "Expose data to outside system by copying into the system clipboard."
  (gui-set-selection 'CLIPBOARD data)
  (run-at-time bw/clipboard-timeout nil
	       (lambda ()
		 (gui-set-selection 'CLIPBOARD "")
		 (message "Clipboard cleaned!")
		 ))
  )

;; --------------------------

(defun bw/read-item ()
  "Reads username and password from loaded item and saves them into
the kill-ring. First, the username is saved, then, the password
is saved."
  (interactive)
  (let* ((bw-item (if bw/item-curr bw/item-curr
		    (bw/load-item)))	 
	 (bw-item-username (bw/item-username bw-item))
	 (bw-item-password (bw/item-password bw-item))
	 )
    (kill-new bw-item-username)
    (kill-new bw-item-password)
    (message "Item copied in clipboard")
    )  
  )

(defun bw/read-uris ()
  "Selects a URI from the loaded item URIs and saves it into the kill-ring."
  (interactive)
  (let* ((bw-item (if bw/item-curr bw/item-curr
		    (bw/load-item)))	 
	 (bw-item-uris (bw/item-uris bw-item))
	 (uris (mapcar
		(lambda (entry) (assoc-default 'uri entry))
		bw-item-uris))
	 (bw-uri-selected (ivy-read "Select URI: " uris))
	 )
    (bw/copy-to-clipboard bw-uri-selected)
    (message "URI copied in clipboard")
    )
  )

(defun bw/read-username ()
  "Reads username from loaded item and saves it into the kill-ring."
  (interactive)
  (let* ((bw-item (if bw/item-curr bw/item-curr
		    (bw/load-item)))	 
	 (bw-item-username (bw/item-username bw-item))
	 )
    (bw/copy-to-clipboard bw-item-username)
    (message "Username copied in clipboard")    
    )
  )

(defun bw/read-password ()
  "Reads password from loaded item and saves it into the kill-ring."
  (interactive)
  (let* ((bw-item (if bw/item-curr bw/item-curr
		    (bw/load-item)))
	 (bw-item-password (bw/item-password bw-item))
	 )
    (bw/copy-to-clipboard bw-item-password)
    (message "Password copied in clipboard")
    )  
  )

;; ------------------------------------------------------------------------------------

(transient-define-prefix bw-ui ()
  "Simple transient UI to the wrappers."

  ;; ----------------------------------
  ;; Bitwarden State
  [:class transient-row "Bitwarden -> State\n"
	  ("Logged:" (lambda () (if bw/token "t" "nil")) (lambda () (interactive)))	  
	  ("Vault:" (lambda () (if bw/vault "Locked" "Unlocked")) (lambda () (interactive) ()))	  
	  ]

  [:class transient-row ""
	  ("Items#:"  (lambda () (format "%d" (length bw/item-names))) (lambda () (interactive) ()))
	  ("Item:" (lambda () (if bw/item-curr (bw/item-label bw/item-curr) "nil" )) (lambda () (interactive) ()))
	  ]
  ;; ----------------------------------  

  ;; ----------------------------------
  ;; Session Commands
  [:class transient-row "Bitwarden -> Session\n"
	  ("s" "Login     " bw/login)
	  ("o" "Logout" bw/logout)
	  ]

  [:class transient-row ""
	  ("l" "Lock Vault" bw/vault-lock)
	  ("u" "Unlock Vault" bw/vault-unlock)
	  ]

  [:class transient-row ""
	  ("i" "Load Item" bw/load-item)
	  ]  

  ;; ----------------------------------  
  ;; Item Commands

  [:class transient-row "Bitwarden -> Items\n"
	  ("1" "Read password" bw/read-password)
	  ("2" "Read username" bw/read-username)
	  ("3" "Read URI" bw/read-uris)		  	  
	  ("4" "Read item" bw/read-item)
	  ]
  )

;; ------------------------------------------------------------------------------------

(provide 'bitwarden)
