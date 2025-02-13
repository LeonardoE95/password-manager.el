;;; keepass.el --- Emacs wrapper for the official keepass CLI -*- lexical-binding: t -*-

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
;; ------------------------------------------------------------------------------------

(defvar kp/binpath ""
  "Path to the keepass CLI binary")

(defvar kp/db ""
  "Path to the keepass database file")

;; timeout parameters
(defvar kb/session-timeout "1 hour"
  "Timeout parameter that determines after how much time the session is terminated.")
(defvar kb/item-timeout "5 minutes"
  "Timeout parameter that determines after how much time the loaded item is removed from memory.")
(defvar kb/clipboard-timeout "1 minutes"
  "Timeout parameter that determines after how much time the system clipboard is cleaned.")

(defvar kb/msg-error "Username or password is incorrect. Try again."
  "Message error used to determine if the login attempt was succesful or not.")
(defvar kb/msg-already-logged "You are already logged in"
  "Message error used to determine if the login attempt was succesful or not.")

(defun kp/cmd-to-string (cmd)
  ;; TODO: implement me
  )

;; ------------------------------------------------------------------------------------

(defun kp/open-database (&optional db password)
  ;; TODO: implement me
  )

(defun kp/load-item-names ()
  ;; TODO: implement me
  )

(defun kp/load-item ()
  ;; TODO: implement me
  )

;; ------------------------------------------------------------------------------------

(defun kp/read-password ()
  (interactive)
  ;; TODO: implement me
  )

(defun kp/read-username ()
  (interactive)
  ;; TODO: implement me
  )

(defun kp/read-uris ()
  (interactive)
  ;; TODO: implement me
  )

(defun kp/read-item ()
  (interactive)
  ;; TODO: implement me
  )

;; ------------------------------------------------------------------------------------

(transient-define-prefix kp-ui ()
  "Simple transient UI to the wrappers."

  [:class transient-row "KeePass -> State\n"
          ]

  [:class transient-row "KeePass -> Session\n"
          ]

  [:class transient-row "KeePass -> Items\n"
          ("1" "Read password" kp/read-password)
          ("2" "Read username" kp/read-username)
          ("3" "Read URI" kp/read-uris)
          ("4" "Read item" kp/read-item)
          ]
  )

;; ------------------------------------------------------------------------------------

(provide 'keepass)
