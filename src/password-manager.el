;;; password-manager.el --- Elisp wrappers for different password managers -*- lexical-binding: t -*-

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

;; ------------------------------------------------------------------------------------

;; providers are defined in the providers directory. Each provider is
;; implement in a .el file
(defvar pm/provider-implementation
  (seq-filter
   (lambda (file)
     (and (file-regular-p file) (string-suffix-p ".el" file)))
   (directory-files "/home/leo/projects/GIT-PUBLIC/password-manager.el/src/providers" t)))

;; once the list has been computed, load it in the current Emacs image
(dolist (provider pm/provider-implementation)
  (load provider)
  (require (read (file-name-sans-extension (file-name-nondirectory provider))))
  )

;; extract symbolic name
(defvar pm/providers
  (mapcar
   (lambda (file)
     (read (file-name-sans-extension (file-name-nondirectory file))))
   pm/provider-implementation)
  "List of supported providers.")

(defvar pm/current nil
  "Value for the current wrapper.")

;; --------------------

(defun pm/item-load ()
  (interactive)
  (pm/provider-exec (pm/select-provider) "load-item")
  )

(defun pm/item-read-password ()
  (interactive)
  (pm/provider-exec (pm/select-provider) "read-password")    
  )

(defun pm/item-read-username ()
  (interactive)
  (pm/provider-exec (pm/select-provider) "read-username")
  )

(defun pm/item-read-uri ()
  (interactive)
  (pm/provider-exec (pm/select-provider) "read-uri")
  )

(defun pm/item-new ()
  "Deals with the creation of a new password item. Such item will
contain typically a username, a password, and a URI. Depending on
the provider used, I will have to implement different logics in
order to update the database with the new item."
  (interactive)
  (let* ((config '((:length . 32)))
         ;; (title (ivy-read "Item title: " nil))
         ;; (username (ivy-read "Username: " nil))
         ;; (uri (ivy-read "URI: " nil))
         (password (pm/generate-password config))
         )
    (kill-new password)
    )
  )

(defun pm/provider-change ()
  (interactive)
  (setq pm/current nil)
  (pm/select-provider)
  )

;; --------------------

(defun pm/select-provider ()
  (let* ((ivy-sort-functions-alist nil)
         (pm (if pm/current pm/current
               (ivy-read "Manager: " pm/providers)))
         )
    (setq pm/current pm)
    pm
    )
  )

(defun pm/provider-exec (pm fun)
  (let* ((function (format "(%s/%s)" pm fun))
         )
    (eval (read function))
    )
  )

(defun pm/generate-password (config)
  (let* ((length (alist-get :length config))
         (cmd (format "cat /dev/urandom | tr -cd '\50-\126' | head -c %s" length))
         (password (shell-command-to-string cmd))
         )
    password
    )
  )

;; --------------------

(transient-define-prefix pm-ui ()
  [:class transient-row "Status \n"
          ("Provider:" (lambda () (if pm/current pm/current "nil")) (lambda () (interactive)))
          ("Item:" (lambda () (if bitwarden/item-curr (bitwarden/item-label bitwarden/item-curr) "nil" )) (lambda () (interactive)))
          ]

  [:class transient-row "Quick Action \n"
          ("1" "Password" pm/item-read-password)
          ("2" "Username" pm/item-read-username)
          ("3" "URI" pm/item-read-uri)
          ("4" "Load item" pm/item-load)
          ("5" "New item" pm/item-new)
          ("6" "Change Provider" pm/provider-change)
          ]

  [:class transient-row "Providers \n"
          ("e1" "bitwarden" bitwarden-ui)
          ("e2" "keepass" keepass-ui)
          ]
  )

;; --------------------

(provide 'password-manager)
