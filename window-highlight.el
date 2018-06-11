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

(defvar-local window-highlight--face-remap-cookies nil
  "Face-remapping cookies applied to current buffer")

(defvar window-highlight-mode)

(defun window-highlight--has-keyboard-focus-p (window)
  (and (frame-focus-state (window-frame window))
       (eq window (frame-selected-window window))))

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

(defun window-highlight--pre-redisplay (_window)
  (when window-highlight--need-rescan
    (window-highlight--rescan-windows)))

(defun window-highlight--window-configuration-change-hook ()
  (setf window-highlight--need-rescan t))

(defun window-highlight--buffer-list-update-hook ()
  (let ((old-window window-highlight--selected-window))
    (unless (eq old-window (selected-window))
      (setf window-highlight--selected-window (selected-window))
      (setf window-highlight--need-rescan t))))

(defun window-highlight--make-frame-function (frame)
  (unless (display-graphic-p frame)
    (setf window-highlight--need-rescan t)))

(defun window-highlight--after-focus-change-function ()
  (setf window-highlight--need-rescan t))

(defun window-highlight--change-major-mode ()
  (setf window-highlight--need-rescan t))

;;;###autoload
(define-minor-mode window-highlight-mode
  "Apply a face to the window with keyboard focus" nil nil nil
  :group 'window-highlight
  :global t
  (cond (window-highlight-mode
         (set-frame-parameter nil 'wh--have-input-focus t)
         (add-hook 'window-configuration-change-hook
           #'window-highlight--window-configuration-change-hook)
         (add-hook 'buffer-list-update-hook
           #'window-highlight--buffer-list-update-hook)
         (add-hook 'after-make-frame-functions
           #'window-highlight--make-frame-function)
         (add-hook 'change-major-mode-hook
           #'window-highlight--change-major-mode)
         (add-function :after after-focus-change-function
                       #'window-highlight--after-focus-change-function)
         (add-function :after pre-redisplay-function
                       #'window-highlight--pre-redisplay)
         )
        (t
         (mapc (lambda (frame)
                 (set-frame-parameter frame 'wh--have-input-focus nil))
               (frame-list))
         (remove-hook 'window-configuration-change-hook
                      #'window-highlight--window-configuration-change-hook)
         (remove-hook 'buffer-list-update-hook
                      #'window-highlight--buffer-list-update-hook)
         (remove-hook 'after-make-frame-functions
                      #'window-highlight--make-frame-function)
         (remove-hook 'change-major-mode-hook
                      #'window-highlight--change-major-mode)
         (remove-function after-focus-change-function
                          #'window-highlight--after-focus-change-function)
         (remove-function pre-redisplay-function
                          #'window-highlight--pre-redisplay)))
  (window-highlight--rescan-windows))

(provide 'window-highlight)

;;; window-highlight.el ends here
