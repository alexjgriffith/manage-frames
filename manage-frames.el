;;; manage-frames.el --- Manage frames accross multiple screens -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Alexander Griffith

;; Author: Alexander Griffith <griffita@gmail.com>
;; keyword: frames screens multiple
;; Created: Nov 22 2015
;; version: 0.0.3

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

;; The current code is sufficent to manage one frame on a single screen
;; i.e. monitor or display. Future renditions aim to allow for smooth
;; transitions between several frames on different displays. 

;;; Code:

;;;###autoload
(require 'cl)

(defvar manage-frames-frame-p 'nil
  "The current frame style, e.g. half-frame max-frame")

(defvar manage-frames-other-buff 'nil
  "The buffer that is closed when durring toggle-frame-size")

(defun manage-frames-set-other-buff (&optional value)
  (interactive)
  (setq manage-frames-other-buff value))

(defun default-font-width () 
  "Return the width in pixels of a character in the current
frame's default font.  More precisely, this returns the
width of the letter ‘m’.  If the font is mono-spaced, this
will also be the width of all other printable characters."
  (let ((window (selected-window))
        (remapping face-remapping-alist))
    (with-temp-buffer
      (make-local-variable 'face-remapping-alist)
      (setq face-remapping-alist remapping)
      (set-window-buffer window (current-buffer))
      (insert "m")
      (aref (aref (font-get-glyphs (font-at 1) 1 2) 0) 4))))

(defun get-current-frame()
  "return the currently active frame"
  (caadr (current-frame-configuration)))

(defun get-this-screen-info()
  "get the display monitor attributes for the current frame"
  (let ((cf (get-current-frame)))
    (car (remove-if-not (lambda (obj) (eq cf (cadr (assoc 'frames  obj)))) (display-monitor-attributes-list)))))

(defun get-screen-size()
  "Get the size of the the current display(monitor)"
  (let ((c (rest (assoc 'workarea (get-this-screen-info)))))
    (list ( - (floor (/ (third c) (default-font-width))) 2)
          ( - (floor (/ (fourth c) (default-font-height)))1 ))))

(defun get-screen-offset()
  "Find how far off the current monitor is off from 0,0"
  (let* ((cf (get-current-frame))
         (dims (assoc 'workarea (get-this-screen-info))))
         (list (second dims) (third dims))))

(defun max-full-off()
  "Turn off maximized and fullscreen"
  (when (eq (cdr (assoc 'fullscreen (frame-parameters))) 'maximized)
      (toggle-frame-maximized))
  (when (eq (cdr (assoc 'fullscreen (frame-parameters))) 'fullboth)
      (toggle-frame-fullscreen)))

(defun windmove-left-p ()
  "Move a window left and if cannot move any further return nil"
    (condition-case nil
	(progn (windmove-left) t) 
      (error nil)))

(defun windmove-right-p ()
  "Move a window right and if cannot move any further return nil"
    (condition-case nil
	(progn (windmove-right) t) 
      (error nil)))

(defun leftmost-aux()
  "Recursive portion of leftmost"
    (when (windmove-left-p)
      (leftmost-aux)))

(defun rightmost-aux()
  "Recursive portion of rightmost"
    (when (windmove-right-p)
      (rightmost-aux)))

(defun window-move-rightmost()
  "Shift the focus to the screen that is furthest right"
  (interactive)
  (rightmost-aux))

(defun window-move-leftmost()
  "Shift the focus to the screen that is furthest left"
  (interactive)
  (leftmost-aux))

(defun save-other(&optional left)
  "Set the value manage-frames-other-buff to the furthest left or right screen. 
If no arguments are provided the rightmost buffer is chosen, if any non nil value
is provided the leftmost argument is chosen."
  (if left
      (window-move-leftmost)
    (window-move-rightmost))
  (setq manage-frames-other-buff (current-buffer)))


(defmacro make-frame-function(name width height off-x off-y set-frame-p &optional function)
  `(defun ,name (&optional other-buff)
     ,(format "Frame function %s, defined using the make-frame-function macro.
Calling this fucntion %s change the value of manage-frames-frame-p." name (if set-frame-p "will" "will not"))
     (interactive)
     (when ,function
       (funcall ,function))
     (let* ((so (get-screen-offset))
            (fs (get-screen-size))
            (height (- (second fs) 1))  ; - 1 was to fix an artifact i was having where when half-frame
					; was called twice it would shift the top line of the frame out
					; of the drawing range of emacs
         (width (first fs))
         (off-y (second so))
         (off-x (first so)))
       (let ((myh ,height)(myw ,width))
	 (set-frame-position (selected-frame)  ,off-x ,off-y)
	 (set-frame-size (selected-frame) (min myw width) (min myh height))
     (if ,set-frame-p
         (setq manage-frames-frame-p ',name))))))

(defun small-frame-fun ()
  "Helper function for frame formats with a single frame"
  (window-move-leftmost)
  (delete-other-windows)
  (max-full-off))

(defun max-frame-fun()
  "Helper function for frame formats with two frames. The second frame
is set to the value of other-buffer"
  (delete-other-windows)
  (when (< (length (mapcar #'window-buffer (window-list))) 2)
    (split-window-right))
  (max-full-off)
  (when (and manage-frames-other-buff (buffer-live-p manage-frames-other-buff))
    (progn (windmove-right)
           (switch-to-buffer manage-frames-other-buff)
           (windmove-left)
           )))

(make-frame-function frame-make-narrow
                      80
                      height
                      off-x
                      off-y
                      t
                      #'small-frame-fun)

(make-frame-function frame-move-center
                      (frame-width)
                      (frame-height) 
                      (+ off-x  (* (default-font-width)
                                   (/(- width (frame-width))2)))
                      off-y
                      nil)

(make-frame-function frame-move-left
                      (frame-width)
                      (frame-height)
                      off-x
                      off-y
                      nil)

(make-frame-function frame-move-right
                      (frame-width)
                      (frame-height)
                      (+ off-x  (* (default-font-width)
                                   (- width (frame-width))))
                      off-y
                      nil)

(make-frame-function frame-make-max-y
                      (frame-width)
                      height
                      off-x
                      off-y
                      nil)


(make-frame-function frame-make-half
                      (floor (/ width 2))
                      height
                      off-x
                      off-y
                      t
                      #'small-frame-fun)

(make-frame-function frame-make-small
                      80
                      30
                      off-x
                      off-y
                      t
                      #'small-frame-fun)
  
(make-frame-function frame-make-max
                      width
                      height
                      off-x
                      off-y
                      t
                      #'max-frame-fun)
                     
(defun frame-make-half-centered()
  "Combining both half and center. This is an example of the capacity 
for the manage-frame functions to be composed."
  (interactive)
  (frame-make-half)
  (frame-move-center))

(defun frame-make-full()
  "Make the current frame full screen. This function departs from the 
generated function since it relies on the toggle-frame-fullscreen function
rather than manualy editiont the frame dimensions"
  (interactive)
  (delete-other-windows)
  (max-full-off)
  (toggle-frame-fullscreen)
  (setq manage-frames-frame-p 'frame-make-full))

(defun toggle-frame-size(&optional alt-frame)
  "A quick function to switch between max-frame and half frame"
  (interactive)
  (if (eq manage-frames-frame-p 'frame-make-max)
      (progn
	(save-other)
        (if alt-frame
            (funcall alt-frame)
          (frame-make-half)))
    (frame-make-max manage-frames-other-buff)))

;;(global-set-key (kbd "M-1") #'toggle-frame-size)
(provide 'manage-frames)
;;; manage-frames.el ends here

