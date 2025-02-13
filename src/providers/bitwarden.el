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
;; This is an extremely simple Emacs wrapper over 'bitwarden', the official
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

(defvar bitwarden/binpath "bw"
  "Path to the bitwarden CLI binary")

(defvar bitwarden/email ""
  "Email to use for login.")

;; timeout parameters
(defvar bitwarden/session-timeout "1 hour"
  "Timeout parameter that determines after how much time the session is terminated.")
(defvar bitwarden/vault-timeout "30 minutes"
  "Timeout parameter that determines after how much time the vault is locked.")
(defvar bitwarden/item-timeout "5 minutes"
  "Timeout parameter that determines after how much time the loaded item is removed from memory.")
(defvar bitwarden/clipboard-timeout "1 minutes"
  "Timeout parameter that determines after how much time the system clipboard is cleaned.")

(defvar bitwarden/msg-error "Username or password is incorrect. Try again."
  "Message error used to determine if the login attempt was succesful or not.")
(defvar bitwarden/msg-already-logged "You are already logged in"
  "Message error used to determine if the login attempt was succesful or not.")

;; session data
(defvar bitwarden/token nil
  "Contains token for current session.")
(defvar bitwarden/vault nil
  "Boolean that represent the state of the vault. t for locked, nil for unlocked.")
(defvar bitwarden/item-names nil
  "List of strings, each of which represent an item in the bitwarden vault.")
(defvar bitwarden/item-curr nil
  "Stores currently loaded item.")

;; --------------------------

(defun bitwarden/cmd-anon-to-string (cmd)
  "Execute a bitwarden command without the session token and return
a string of the output"
  ;; TODO: add validation
  (shell-command-to-string (format "%s %s" bitwarden/binpath cmd))
  )

(defun bitwarden/cmd-auth-to-string (cmd)
  "Execute a bitwarden command using the session token saved
 'bitwarden/token' and return a string of the output"
  ;; TODO: add validation
  (with-environment-variables (("BW_SESSION" bitwarden/token))
    (shell-command-to-string (format "%s %s" bitwarden/binpath cmd))
    )
  )

;; --------------------------

(defun bitwarden/login (&optional email password)
  (interactive)

  (defun bitwarden/handle-login (proc output)
    "Handle output from the login procedure."
    (when (not (string-match-p (regexp-quote "Master password") output))
      (if (not (or (string-match-p (regexp-quote bitwarden/msg-error) output)
                   (string-match-p (regexp-quote bitwarden/msg-already-logged) output)))
          (progn
            (message "Succesfully logged in!")

            ;; only set bw token in case of successful
            (setq bitwarden/token output)
            (setq bitwarden/item-curr nil)

            ;; tell wrapper that currently the password manager to use
            ;; is bitwarden.el. Not sure if I want this variable to be
            ;; set here, but for now it will do.
            (setq pm/current-wrapper 'bitwarden)

            ;; clear state after proper timeouts
            (run-at-time bitwarden/vault-timeout nil (lambda () (bitwarden/vault-lock) ))
            (run-at-time bitwarden/session-timeout nil (lambda () (bitwarden/logout) ))

            ;; load items names immediately so we save time later on
            (bitwarden/load-items-names)
            (message "Loaded item names!")

            ;; immediately perform a full flow after the first login
            ;; to speed things up
            (bitwarden/execute-flow)
            )
        (progn
          (message "Could not log: %s" output)
          )
        )
      )
    )

  ;; make sure we're logged out to not interfere with
  ;; existing sessions
  (bitwarden/logout)

  ;; Initialize values regardless of login outcome
  (progn
    (setq bitwarden/token nil)
    (setq bitwarden/vault nil)
    (setq bitwarden/item-names nil)
    (setq bitwarden/item-curr nil)
    )

  (let* ((bitwarden-email (if email email
                     (read-from-minibuffer "Email: " (if bitwarden/email bitwarden/email ""))))
         (bitwarden-password (if password password
                        (read-passwd "Master Password: ")))
         (process-name "bitwarden")
         (buffer-name "*bitwarden*")
         (process (start-process process-name buffer-name "bw" "login" bitwarden-email "--raw"))
         )

    ;; save email for next login
    (setq bitwarden/email bitwarden-email)
    ;;
    ;; send password without putting it into the cmdline to avoid
    ;; exposure of confidential data
    ;;
    (set-process-filter process #'bitwarden/handle-login)
    (process-send-string process (format "%s\n" bitwarden-password))
    (process-send-eof process)
    )
  )

(defun bitwarden/logout ()
  (interactive)
  (bitwarden/cmd-auth-to-string (format "logout"))
  (setq bitwarden/token nil)
  (setq bitwarden/vault nil)
  (message "Logged out!")
  )

(defun bitwarden/session-check ()
  (interactive)
  (not (string=
   "You are not logged in."
   (bitwarden/cmd-auth-to-string (format "login --check"))
   ))
  )

(defun bitwarden/vault-lock ()
  (interactive)
  (bitwarden/cmd-auth-to-string (format "lock"))
  (setq bitwarden/vault t)
  (setq bitwarden/item-curr nil)
  (message "Vault locked!")
  )

(defun bitwarden/vault-unlock ()
  (interactive)
  (defun bitwarden/handle-unlock (proc output)
    "Handle output from the unlock procedure."
    (when (not (string-match-p (regexp-quote "Master password") output))
      (if (not (or (string-match-p (regexp-quote bitwarden/msg-error) output)
                   (string-match-p (regexp-quote bitwarden/msg-already-logged) output)))
          (progn
            ;; only set bitwarden token in case of successful
            (message "Vault unlocked!")
            (setq bitwarden/token output)
            (setq bitwarden/vault nil)
            (run-at-time bitwarden/vault-timeout nil (lambda () (bitwarden/vault-lock) ))
            )
        (message "Could not unlock vault: %s" output)
        )
      )
    )
  (let* ((bitwarden-password (read-passwd "Unlock vault: "))
         (process-name "bw")
         (buffer-name "*bitwarden*")
         (process (start-process process-name buffer-name "bw" "unlock" "--raw"))
         )

    (set-process-filter process #'bitwarden/handle-unlock)
    (process-send-string process (format "%s\n" bitwarden-password))
    (process-send-eof process)
    )
  )

;; --------------------------

(defun bitwarden/execute-flow ()
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
        (bitwarden/load-item)
        (bitwarden/read-password)
        )
      )

     ;; load an item interactively and read password
     ((eq action 'uri+password)
      (progn
        (bitwarden/load-item)
        (bitwarden/read-uri)
        )
      )
     )

    )
  )

;; --------------------------

(defun bitwarden/load-items-names ()
  (interactive)
  (let* ((bitwarden-items (json-read-from-string (bitwarden/cmd-auth-to-string (format "list items"))))
         (bitwarden-names (mapcar (lambda (entry) (assoc-default 'name entry)) bitwarden-items))
         )
    (setq bitwarden/item-names bitwarden-names)
    )
  )

(defun bitwarden/load-item ()
  (interactive)
  (setq bitwarden/item-curr (bitwarden/select-item))
  (run-at-time bitwarden/item-timeout nil
               (lambda ()
                 (setq bitwarden/item-curr nil)
                 ))

  (run-at-time bitwarden/item-timeout nil
               (lambda ()
                 (setq bitwarden/item-curr nil)
                 ))

  bitwarden/item-curr
  )

(defun bitwarden/select-item ()
  (interactive)

  ;; if the session is expired, we have to login again
  (when (not (bitwarden/session-check))
    (message "Session is over")
    (bitwarden/login))

  ;; if the vault is locked, we have to unlock it
  (when bitwarden/vault
    (bitwarden/vault-unlock)
    )

  ;; if item list is expired, we have to download it again
  (when (not bitwarden/item-names)
    (bitwarden/load-items-names)
    )

  (let* ((selected-name (ivy-read "Name: " bitwarden/item-names))
         (output-cmd (bitwarden/cmd-auth-to-string (format "list items --search %s" selected-name)))
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

(defun bitwarden/item-name (item)
  "Extract item name from the item structure."
  (if item
      (assoc-default 'name (car item))
    nil)
  )

(defun bitwarden/item-id (item)
  "Extract item id from the item structure."
  (if item
      (assoc-default 'id (car item))
    nil)
  )

(defun bitwarden/item-username (item)
  "Extract username from the item structure."
  (if item
      (assoc-default 'username (assoc-default 'login (car item)))
    nil)
  )

(defun bitwarden/item-password (item)
  "Extract password from the item structure."
  (if item
      (assoc-default 'password (assoc-default 'login (car item)))
    nil)
  )

(defun bitwarden/item-uris (item)
  "Extract URIs from the item structure."
  (if item
      (cl-coerce
       (assoc-default 'uris (assoc-default 'login (car item)))
       'list)
    nil)
  )

(defun bitwarden/item-label (item)
  "Compute simple string representation for showing the item loaded in the transient UI."
  (format "%s/%s"
          (bitwarden/item-name item)
          (bitwarden/item-username item))
  )

(defun bitwarden/copy-to-clipboard (data)
  "Expose data to outside system by copying into the system clipboard."
  (gui-set-selection 'CLIPBOARD data)
  (run-at-time bitwarden/clipboard-timeout nil
               (lambda ()
                 (gui-set-selection 'CLIPBOARD "")
                 (message "Clipboard cleaned!")
                 ))
  )

;; --------------------------

(defun bitwarden/read-item ()
  "Reads username and password from loaded item and saves them into
the kill-ring. First, the username is saved, then, the password
is saved."
  (interactive)
  (let* ((bitwarden-item (if bitwarden/item-curr bitwarden/item-curr
                    (bitwarden/load-item)))
         (bitwarden-item-username (bitwarden/item-username bitwarden-item))
         (bitwarden-item-password (bitwarden/item-password bitwarden-item))
         )
    (kill-new bitwarden-item-username)
    (kill-new bitwarden-item-password)
    (message "Item copied in clipboard")
    )
  )

(defun bitwarden/read-uri ()
  "Selects a URI from the loaded item URIs and saves it into the kill-ring."
  (interactive)
  (let* ((bitwarden-item (if bitwarden/item-curr bitwarden/item-curr
                    (bitwarden/load-item)))
         (bitwarden-item-uris (bitwarden/item-uris bitwarden-item))
         (uris (mapcar
                (lambda (entry) (assoc-default 'uri entry))
                bitwarden-item-uris))
         (bitwarden-uri-selected (ivy-read "Select URI: " uris))
         )
    (bitwarden/copy-to-clipboard bitwarden-uri-selected)
    (message "URI copied in clipboard")
    )
  )

(defun bitwarden/read-username ()
  "Reads username from loaded item and saves it into the kill-ring."
  (interactive)
  (let* ((bitwarden-item (if bitwarden/item-curr bitwarden/item-curr
                    (bitwarden/load-item)))
         (bitwarden-item-username (bitwarden/item-username bitwarden-item))
         )
    (bitwarden/copy-to-clipboard bitwarden-item-username)
    (message "Username copied in clipboard")
    )
  )

(defun bitwarden/read-password ()
  "Reads password from loaded item and saves it into the kill-ring."
  (interactive)
  (let* ((bitwarden-item (if bitwarden/item-curr bitwarden/item-curr
                    (bitwarden/load-item)))
         (bitwarden-item-password (bitwarden/item-password bitwarden-item))
         )
    (bitwarden/copy-to-clipboard bitwarden-item-password)
    (message "Password copied in clipboard")
    )
  )

;; ------------------------------------------------------------------------------------

(transient-define-prefix bitwarden-ui ()
  "Simple transient UI to the wrappers."

  ;; ----------------------------------
  ;; Bitwarden State
  [:class transient-row "Bitwarden -> State\n"
          ("Logged:" (lambda () (if bitwarden/token "t" "nil")) (lambda () (interactive)))
          ("Vault:" (lambda () (if bitwarden/vault "Locked" "Unlocked")) (lambda () (interactive) ()))
          ("Items#:"  (lambda () (format "%d" (length bitwarden/item-names))) (lambda () (interactive) ()))
          ("Item:" (lambda () (if bitwarden/item-curr (bitwarden/item-label bitwarden/item-curr) "nil" )) (lambda () (interactive) ()))
          ]

  ;; ----------------------------------

  ;; ----------------------------------
  ;; Session Commands
  [:class transient-row "Bitwarden -> Session Management\n"
          ("e1" "Login" bitwarden/login)
          ("e2" "Logout" bitwarden/logout)
          ("e3" "Lock Vault" bitwarden/vault-lock)
          ("e4" "Unlock Vault" bitwarden/vault-unlock)
          ("e5" "Load Item" bitwarden/load-item)
          ]

  ;; ----------------------------------
  ;; Item Commands
  [:class transient-row "Bitwarden -> Item\n"
          ("1" "Read password" bitwarden/read-password)
          ("2" "Read username" bitwarden/read-username)
          ("3" "Read URI" bitwarden/read-uri)
          ("4" "Read item" bitwarden/read-item)
          ]
  )

;; ------------------------------------------------------------------------------------

(provide 'bitwarden)
