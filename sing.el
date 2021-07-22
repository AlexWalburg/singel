;;; sing.el --- Custom keywrapper to handle inputs

;; Copyright (C) 2020 Alex Walburg

;; Author: Alex Walburg <ajwalburg@gmail.com>
;; Maintainer: Alex Walburg <ajwalburg@gmail.com>
;; URL:
;; Version: 1.0.0
;; Keywords: ergonomics, evil
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Sing.el provides a wrapper which maps all keychords to collections of "single" inputs.
;; To use this, simply call the singel-parse-streams function, ideally mapping it to something like the space key.
;; Sing.el has some customization, mostly in the form of constants declared before using the package.
;; Singel-escape-key controls what key goes into the escape mode.
;; When in escape mode, pressing any of the keys in singel-keys-to-modifers will add that modifier to the list.
;; Singel-use-which-key controls which key integration, set it to nil if you don't want which-key support enabled.

;;; Code:
(defvar singel-keys-to-modifiers '((?s hyper) 
				     (?d super) 
				     (?f meta)))
(defvar singel-keys-to-paging-commands '((?n next) 
				        (?p previous)))
(defvar singel-escape-key ?\;)
(defvar singel-use-which-key t)
(provide 'sing)
;;;###autoload
(defun singel-parse-commands () 
  "Grabs input and uses it to give commands" 
  (interactive) 
  (let ((current-modifers (list)) 
	(escaped? nil) 
	(intial? t) 
	(curr-command-string (vector)) 
	(curr-input) 
	(modifier-keys '(control))	; initial state
	(append-to-end (lambda () 
			 (setq curr-command-string (vconcat curr-command-string (list
										 (event-convert-list
										  (append
										   modifier-keys
										   (list
										    curr-input)))))) 
			 (setq modifier-keys (list) intial? nil)))) 
    (catch 'exit-parsing 
      (while (not (commandp (key-binding curr-command-string))) 
	(setq curr-input (read-key (concat (key-description curr-command-string) " "
					   (single-key-description (event-convert-list (append
											modifier-keys
											(list ??))))
					   ":"))) 
	(cond ((equal curr-input ?) 
	       (throw 'exit-parsing "stopped input")) 
	      (escaped? (let ((result (assoc curr-input singel-keys-to-modifiers))
			      (paging-result (assoc curr-input singel-keys-to-paging-commands))) 
			  (if result
			      (progn
				(when intial? 
				  (setq modifier-keys (list) initial? nil))
				(push (car (cdr result)) modifier-keys)))
			  (if (and paging-result singel-use-which-key)
			      (cond ((eq paging-result 'next)
				     (which-key-show-next-page-cycle))
				    ((eq paging-result 'previous)
				     (which-key-show-previous-page-cycle))))
			  (if (not (or paging-result (and result singel-use-which-key)))
			      (funcall append-to-end))
			  (setq escaped? nil))) 
	      ((equal curr-input singel-escape-key) ; has to not be escaped as escaped was before
	       (setq escaped? t)) 
	      ((equal curr-input ? ) 
	       (if (not (member 'control modifier-keys)) 
		   (push 'control modifier-keys) 
		 (funcall append-to-end))) 
	      ((characterp curr-input) 
	       (funcall append-to-end))
	      (t (throw 'exit-parsing "invalid input"))) 
	(if singel-use-which-key (which-key--create-buffer-and-show curr-command-string)))) 
    (if singel-use-which-key (which-key--hide-popup-ignore-command)) 
    (if (commandp (key-binding curr-command-string))
	(call-interactively (key-binding curr-command-string))
      (message "Exited singel mode"))))
