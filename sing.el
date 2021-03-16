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
(defconst singel-keys-to-modifiers '((?s "H-") 
				 (?d "s-") 
				 (?f "M-")))
(defconst singel-escape-key ?\;)
(defconst singel-use-which-key t)

;;;###autoload
(defun singel-parse-streams () 
  "Grabs input and uses it to give commands" 
  (interactive) 
  (let ((current-modifers (list)) 
	(escaped? nil) 
	(intial? t) 
	(curr-command-string "") 
	(curr-input "") 
	(modifier-keys '("C-"))		; initial state
	(append-to-end (lambda () 
			 (setq curr-command-string (concat curr-command-string (apply 'concat
										      modifier-keys) 
							   (char-to-string curr-input) " ")) 
			 (setq modifier-keys (list) intial? nil)))) 
    (catch 'exit-parsing 
      (while (not (commandp (key-binding (kbd curr-command-string)))) 
	(setq curr-input (read-key (concat curr-command-string " " (apply 'concat modifier-keys)
					   ":"))) 
	(cond ((equal curr-input ?) 
	       (throw 'exit-parsing "stopped input")) 
	      (escaped? (let ((result (assoc curr-input singel-keys-to-modifiers))) 
			  (when intial? 
			    (setq modifier-keys (list) initial? nil)) 
			  (if result
			      (push (car (cdr result)) modifier-keys) 
			    (funcall append-to-end)) 
			  (setq escaped? nil))) 
	      ((equal curr-input singel-escape-key) ; has to not be escaped as escaped was before
		      (setq escaped? t)) 
	       ((equal curr-input ? ) 
		(if (not (member "C-" modifier-keys)) 
		    (push "C-" modifier-keys) 
		  (funcall append-to-end))) 
	       ((characterp curr-input) 
		(funcall append-to-end)) 
	       (t 
		(setq unread-command-events (list curr-input)))) 
	(if singel-use-which-key (which-key--create-buffer-and-show (kbd curr-command-string)))))
    (if singel-use-which-key (which-key--hide-popup-ignore-command))
    (call-interactively (key-binding (kbd curr-command-string)))))
