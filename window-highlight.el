;; -*- lexical-binding: t -*-
;; Copyright (C) 2018 Daniel Colascione

;; Author: Daniel Colascione <dancol@dancol.org>
;; Version: 1.0
;; Keywords: faces

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

;; Uses new Emacs 27 face filters to apply custom face tweaks to the
;; Emacs window with keyboard focus.
;;
;; Example use:
;;
;; (require 'window-highlight)
;; (apply
;;  #'custom-set-faces
;;  (let ((inactive-color "#ffffe2")
;;        (where '((type x w32 ns))))
;;    `((default
;;        ((,where
;;          :background ,inactive-color)))
;;      (fringe
;;        ((,where
;;          :background ,inactive-color)))
;;      (window-highlight-focused-window
;;       ((,where
;;         :background "white"))))))
;; (window-highlight-mode 1)
;;

;;; Code:

(require 'face-remap)

(defgroup window-highlight nil "Window highlight"
  :prefix "window-highlight-"
  :group 'faces)

(defface window-highlight-focused-window
  nil
  "Face used to highlight the focused window"
  :group 'window-highlight)

(defvar window-highlight--selected-window nil
  "Cache of last window we knew to be selected")

(defvar window-highlight--need-rescan nil
  "True when we need post-command-hook to refresh internal data")

(defvar window-highlight--focused-frame nil
  "Currently focused frame")

(defvar-local window-highlight--face-remap-cookies nil
  "Face-remapping cookies applied to current buffer")

(defvar window-highlight-mode)

(defun window-highlight--has-keyboard-focus-p (window)
  (and (eq (window-frame window) window-highlight--focused-frame)
       (eq window (selected-window))))

(defun window-highlight--rescan-windows ()
  (setf window-highlight--need-rescan nil)
  (let ((focused-buffer nil))
    (walk-windows
     (lambda (window)
       (let ((has-keyboard-focus
              (and window-highlight-mode
                   (window-highlight--has-keyboard-focus-p window))))
         (set-window-parameter window 'has-keyboard-focus has-keyboard-focus)
         (when has-keyboard-focus
           (setf focused-buffer (window-buffer window)))))
     t t)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (cond ((eq buffer focused-buffer)
               (unless window-highlight--face-remap-cookies
                 (setf window-highlight--face-remap-cookies
                       (mapcar (lambda (face)
                                 (face-remap-add-relative
                                  face '(:filtered (:window has-keyboard-focus t)
                                         window-highlight-focused-window)))
                               '(default fringe)))))
              (t
               (when window-highlight--face-remap-cookies
                 (mapc #'face-remap-remove-relative
                       window-highlight--face-remap-cookies)
                 (setf window-highlight--face-remap-cookies nil))))))
    (redraw-display)))

(defun window-highlight--post-command-hook ()
  (when window-highlight--need-rescan
    (window-highlight--rescan-windows)))

(defun window-highlight--window-configuration-change-hook ()
  (setf window-highlight--need-rescan t))

(defun window-highlight--buffer-list-update-hook ()
  (let ((old-window window-highlight--selected-window))
    (unless (eq old-window (selected-window))
      (setf window-highlight--selected-window (selected-window))
      (setf window-highlight--need-rescan t))))

(defun window-highlight--focus-in-hook ()
  (setf window-highlight--focused-frame (selected-frame))
  (window-highlight--rescan-windows))

(defun window-highlight--focus-out-hook ()
  (setf window-highlight--focused-frame nil)
  (window-highlight--rescan-windows))

;;;###autoload
(define-minor-mode window-highlight-mode
  "Apply a face to the window with keyboard focus" nil nil nil
  :group 'window-highlight
  :global t
  (cond (window-highlight-mode
         (setf window-highlight--focused-frame (selected-frame))
         (add-hook 'post-command-hook
           #'window-highlight--post-command-hook)
         (add-hook 'window-configuration-change-hook
           #'window-highlight--window-configuration-change-hook)
         (add-hook 'buffer-list-update-hook
           #'window-highlight--buffer-list-update-hook)
         (add-hook 'focus-in-hook
           #'window-highlight--focus-in-hook)
         (add-hook 'focus-out-hook
           #'window-highlight--focus-out-hook))
        (t
         (setf window-highlight--focused-frame nil)
         (remove-hook 'post-command-hook
                      #'window-highlight--post-command-hook)
         (remove-hook 'window-configuration-change-hook
                      #'window-highlight--window-configuration-change-hook)
         (remove-hook 'buffer-list-update-hook
                      #'window-highlight--buffer-list-update-hook)
         (remove-hook 'focus-in-hook
                      #'window-highlight--focus-in-hook)
         (remove-hook 'focus-out-hook
                      #'window-highlight--focus-out-hook)))
  (window-highlight--rescan-windows))

(provide 'window-highlight)

;;; window-highlight.el ends here
