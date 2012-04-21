;;; jabber.el --- add personal message support in emacs-jabber MUC.

;; Copyright (C) 2010  Merinov Nikolay

;; Author: Merinov Nikolay <kim.roader@gmail.com>
;; Keywords: jabber

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; This file containt two alternative realisation for support personal messages
;; in MUC in emacs-jabber. First realisation change code of
;; `jabber-muc-print-prompt' for change prompt style. This variant, probably
;; must be rewrite through `defadvice'. Second realisation just add "* " before
;; prompt. This variant commented now.
;;
;; Installation:
;;
;; Call (load "/path/to/jabber.el") after (require 'jabber) in your init file.
;;
;; Usage:
;;

;;; Code:

;; common variable
(defcustom jabber-muc-colorize-personal t
  "Colorize MUC messages for you."
  :type 'boolean
  :group 'jabber-chat)


;; First variant of realisation
(defcustom jabber-muc-colorize-personal-list '(:box (:line-width 2 :color "grey75" :style released-button))
  "Colorize MUC messages for you with face attributes."
  :type 'cons
  :group 'jabber-chat)

(defun jabber-muc-print-prompt (xml-data &optional local dont-print-nick-p)
  "Print MUC prompt for message in XML-DATA."
  (let ((nick (jabber-jid-resource (jabber-xml-get-attribute xml-data 'from)))
	(timestamp (car (delq nil (mapcar 'jabber-x-delay (jabber-xml-get-children xml-data 'x))))))
    (if (stringp nick)
	(insert (jabber-propertize
		 (format-spec jabber-groupchat-prompt-format
			      (list
			       (cons ?t (format-time-string 
					 (if timestamp
					     jabber-chat-delayed-time-format
					   jabber-chat-time-format)
					 timestamp))
			       (cons ?n (if dont-print-nick-p "" nick))
			       (cons ?u nick)
			       (cons ?r nick)
			       (cons ?j (concat jabber-group "/" nick))))
		 'face (if local        ;Message from you.
                           (if jabber-muc-colorize-local ;; If colorization enable...
                               ;; ...colorize nick
                               (list ':foreground (jabber-muc-nick-get-color nick))
			     ;; otherwise, use default face.
			     'jabber-chat-prompt-local)
			 ;; Message from other participant.
                         (let* ((group (jabber-jid-user (jabber-xml-get-attribute xml-data 'from)))
				(body-text (car (jabber-xml-node-children
						 (car (jabber-xml-get-children
						       xml-data 'body)))))
				(to-me (when (and jabber-muc-colorize-personal
						  (jabber-muc-looks-like-personal-p body-text group))
					 jabber-muc-colorize-personal-list)))
			   (if jabber-muc-colorize-foreign ;If colorization enable...
			       ;; ... colorize nick
			       (append to-me (list ':foreground (jabber-muc-nick-get-color nick)))
			     ;; otherwise, use default face.
			     (append to-me (jabber-face-to-plist 'jabber-chat-prompt-foreign)))))
 		 'help-echo (concat (format-time-string "On %Y-%m-%d %H:%M:%S" timestamp) " from " nick " in " jabber-group)))
      (jabber-muc-system-prompt))))

;; (defun jabber-face-to-plist (face)
;;   (apply 'append
;; 	 (mapcar #'(lambda (a)
;; 		     (when (not (eq (cdr a) 'unspecified))
;; 		       (list (car a) (cdr a))))
;; 		 (face-all-attributes face))))

(defun jabber-face-to-plist (face) 
  (apply 'append
	 (mapcar #'(lambda (a)
		     (let ((face-attr (face-attribute face a)))
		       (when (not (eq face-attr 'unspecified))
			 (list a face-attr))))
		 (list :family :foundry :width :height :weight :slant
		       :foreground :background :underline :overline
		       :strike-through :box :inverse-video :stipple
		       :font :inherit))))


;; Second variant of realisation

;; (defun jabber-muc-print-personal (xml-data who mode)
;;   (let ((group (jabber-jid-user (jabber-xml-get-attribute xml-data 'from)))
;; 	(body (car (jabber-xml-node-children
;; 		    (car (jabber-xml-get-children xml-data 'body))))))
;;     (when (and body
;; 	       (eql mode :insert)
;; 	       (eql who :muc-foreign)
;; 	       (jabber-muc-looks-like-personal-p body group))
;;       (print who t)
;;       (insert (jabber-propertize 
;; 	       "* "
;; 	       'face 'bold)))))

;; (setq jabber-muc-printers (cons #'jabber-muc-print-personal jabber-muc-printers))
