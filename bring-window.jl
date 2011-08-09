;;; bring-window.jl -- Functions for pulling a window to the pointer,
;;; and sending it back.
;; Copyright 2000-2003 by Adrian Hosey <alh@warhound.org>

;; bring-window.jl is free software distributed under the terms of the
;; GNU General Public License, version 2. For details please see
;; <http://www.gnu.org/copyleft/gpl.html>
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;;; Commentary
;;
;; Place the bring-window.jl file somewhere in your sawfish
;; load-path. Put (require 'bring-window) in your .sawfishrc, reload
;; it, then go to sawfish's "Bindings" menu and bind "Popup bring
;; window list" and "Bring window undo" to your chosen keys.
;;
;; "Popup bring window list" (the function popup-bring-window-list)
;; brings up a menu of your managed windows. The window you select
;; from the menu will reappear under your pointer. Hitting the key for
;; "Bring window undo" (bring-window-undo) will return the most
;; recently brought window to its previous location.
;;
;; bring-window.jl also provides the function bring-window-to-pointer
;; which takes a window name or window object as its argument and
;; brings it to the pointer.
;;

(provide 'bring-window)

(defvar bring-window-last-pos nil)

(defun bring-window-to-pointer (winglob)
  "Bring a window to the current pointer location"
  ; TODO - this takes a window obj or name, should also take an XID.
  (let ((pos (query-pointer))
	(w (if (windowp winglob) 
	       winglob
	     (get-window-by-name winglob))))
    (setq bring-window-last-pos (cons w (window-position w)))
    (move-window-to w (car pos) (cdr pos))
    (uniconify-window w)
    (set-input-focus w)))

(defun bring-window-undo ()
  "Return the last 'brought' window to its previous position"
  (interactive)
  (move-window-to (car bring-window-last-pos) (cadr bring-window-last-pos)
		  (cddr bring-window-last-pos)))

(defun popup-bring-window-list ()
  "Display the menu of windows to bring to the pointer."
  (interactive)
  (popup-menu (window-menu-paramed 'bring-window-to-pointer)))

(defun window-menu-paramed (action-function)
  (let*
      ((make-label (lambda (w)
		     (let
			 ((name (window-name w)))
		       (concat (and (window-get w 'iconified) ?\[)
			       (if (> (length name) 20)
				   (concat
				    (substring name 0 20) "...")
				 name)
			       (and (window-get w 'iconified)  ?\])
			       (and (eq (input-focus) w) " *")))))
       (limits (workspace-limits))
       (windows (managed-windows))
       (i (car limits))
       menu)
    (while (<= i (cdr limits))
      (mapc (lambda (w)
	      (when (and (window-in-workspace-p w i)
			 (window-mapped-p w)
			 (or (not (window-get w 'ignored))
			     (window-get w 'iconified)))
		(setq menu (cons (list (make-label w)
				       `(,action-function
					 (get-window-by-id ,(window-id w)) ,i))
				 menu))))
	    windows)
      (unless (or (= i (cdr limits)) (null (car menu)))
	(setq menu (cons nil menu)))
      (setq i (1+ i)))
    ;; search for any iconified windows that aren't anywhere else in the menu
    (let
	(extra)
      (mapc (lambda (w)
	      (when (and (window-get w 'iconified) (window-get w 'sticky))
		(setq extra (cons (list (make-label w)
					`(,action-function
					  (get-window-by-id ,(window-id w))))
				  extra))))
	    windows)
      (when extra
	(setq menu (if menu (nconc extra (list nil) menu) extra))))
    (nreverse menu)))
