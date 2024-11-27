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

;; import the implementation of the different wrappers
(require 'bitwarden)

;; --------------------

(defvar pm/available-wrappers '(bitwarden keepass)
  "Value for the current wrapper")

(defvar pm/current-wrapper nil
  "Value for the current wrapper")

;; --------------------

(transient-define-prefix pm-ui ()
  [:class transient-row "Password Managers \n"
	  ("1" "bitwarden" bw-ui)
	  ;; ("2" "keepass" )
	  ]  
  )

;; --------------------

(provide 'password-manager)
