;;; vhdl-capf.el --- Completion at point function (capf) for vhdl-mode.

;; Copyright (C) 2015 sh-ow
;;
;; Author: sh-ow <sh-ow@users.noreply.github.com>
;; URL: https://github.com/sh-ow/vhdl-capf
;; Version: 0.1
;; Keywords: convenience, usability, vhdl, completion

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author or from the Free Software Foundation, Inc., 675 Mass Ave,
;; Cambridge, MA 02139, USA.

;;; Code:

(defconst vhdl-search-completion-buffers 3
  "If t, search in _all_ other vhdl-buffers for completions, when number,
search in the last opened (number+1) vhdl-buffers.")

(defvar vhdl-completion-cache nil
  "Cache for completion candidates per vhdl-buffer: alist with form (buffername . candidates).")

(defconst exclude-common-vhdl-syntax '("signal" "variable" "downto" "to" "if" "then"
				       "begin" "end" "in" "out" "std_logic" "std_logic_vector")
  "Some often occuring VHDL syntax constructs to exclude from the possible
completions-list _before_ trying to fuzzy match (performance).")

(defun flatten (L)
  "Convert a list of lists into a single list."
  (if (null L)
      nil
    (if (atom (first L))
	(cons (first L) (flatten (rest L)))
      (append (flatten (first L)) (flatten (rest L))))))

(defun get-vhdl-buffers (&optional nfirst)
  "Returns a list with all buffers that are in vhdl major mode."
  (let ((vhdl-buffers ())
	(cnt 0))
    (dolist (name (buffer-list))
      (with-current-buffer name
	(when (and (eq major-mode 'vhdl-mode)
		   (or (not nfirst) (<= cnt nfirst)))
	  (setq vhdl-buffers (append vhdl-buffers (list name)))
	  (setq cnt (+ cnt 1)))))
    vhdl-buffers))

(defun line-is-comment ()
  "Returns t if current line contains nothing but a comment."
  (save-excursion
    (beginning-of-line 1)
    (looking-at (concat "^[\s-]*" comment-start-skip))))

(defun get-vhdl-symbols (&optional limit buffer)
  "Get all (vhdl-) symbols  of a certain buffer."
  (let ((complist ())
	(regpat "\\<[A-Za-z]+\\(\\sw\\|\\s_\\)+")
	(whichbuffer (if (eq buffer nil) (current-buffer) buffer)))
    (with-current-buffer whichbuffer
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward regpat limit t)
	  (let ((result (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
	    ;; exclude: vhdl syntax-stuff, stuff that is in a comment, already captured stuff
	    (when (and (not (or (member result exclude-common-vhdl-syntax) (line-is-comment)))
		       (not (member result complist)))
	      (push result complist))))))
    complist))

;;;###autoload
(defun vhdl-completion-at-point ()
  "Handling the completion at point for vhdl mode."
  (if (eq major-mode 'vhdl-mode)
      (let* ((pos (point))
	     ;; find the word boundary (vhdl-expressions can only follow on the chars in following regexp)
	     (beg (if (re-search-backward "[=(,+-/\*\s-]" (line-beginning-position) t)
		      (match-end 0)
		    (line-beginning-position)))
	     (end (goto-char pos)) ;; goto on purpose: search for 'beg' eventually moves cursor backwards!
	     (table-etc (list nil
			      (completion-table-merge
			       vhdl-abbrev-list
			       (let* ((vhdl-abbrevs ())
				      (didchanges nil)
				      (vhdl-buffers (if (eq vhdl-search-completion-buffers t)
							(get-vhdl-buffers)
						      (get-vhdl-buffers vhdl-search-completion-buffers))))
				 (dotimes (idx (length vhdl-buffers))
				   (if (not (equal (car (nth idx vhdl-completion-cache)) (nth idx vhdl-buffers)))
				       (progn
					 (add-to-ordered-list 'vhdl-completion-cache
							      (cons (nth idx vhdl-buffers)
								    (delete (buffer-substring-no-properties beg end)
									    (get-vhdl-symbols (point-max) (nth idx vhdl-buffers))))
							      idx)
					 (setq didchanges t))))
				 ;; cut the cache list, do save ram (the now deleted elements would have been updated anyway)
				 (when (> (length vhdl-completion-cache) (length vhdl-buffers))
				   (nbutlast vhdl-completion-cache (- (length vhdl-completion-cache) (length vhdl-buffers))))
				 ;; if the active buffer is still the same, just do the cache update for this buffer
				 (when (eq didchanges nil)
				   (setcdr (car vhdl-completion-cache) (delete (buffer-substring-no-properties beg end)
									       (get-vhdl-symbols (point-max) (nth 0 vhdl-buffers)))))
				 (flatten
				  (dolist (bufcomps vhdl-completion-cache vhdl-abbrevs)
				    (push (cdr bufcomps) vhdl-abbrevs))))))))
	(when end
	  (let ((tail (if (null (car table-etc))
			  (cdr table-etc)
			(cons
			 (if (memq (char-syntax (or (char-after end) ?\s))
				   '(?\s ?>))
			     (cadr table-etc)
			   (apply-partially 'completion-table-with-terminator
					    " " (cadr table-etc)))
			 (cddr table-etc)))))
	    `(,beg ,end ,@tail))))
    nil))

;;;###autoload
(defun vhdl-capf-enable ()
  "Add vhdl-completion-at-point function to capf's when visiting a vhdl-file."
  (add-hook 'vhdl-mode-hook (lambda () (make-local-variable 'completion-at-point-functions)
			      (add-to-list 'completion-at-point-functions 'vhdl-completion-at-point))))

(provide 'vhdl-capf)

;;; vhdl-capf.el ends here
