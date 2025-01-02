;; -*- lexical-binding: t -*-
;; Copyright (C) 2018 Daniel Colascione

;; Author: Daniel Colascione <dancol@dancol.org>
;; Version: 2.0
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

(defvar-local window-highlight--face-remap-cookies nil
  "Face-remapping cookies applied to current buffer")

(defvar window-highlight-mode)
(defvar window-highlight--current-buffer)
(defvar window-highlight--selected-window)

(defconst window-highlight--has-focus-property
  'window-highlight-has-focus)

(defun window-highlight--has-keyboard-focus-p (window)
  ;; The AND below forces the value to be EQ t if it's non-nil, which
  ;; makes the tty case (for which we report unknown focus)
  ;; work correctly.
  (and (frame-focus-state (window-frame window)) t))

(defun window-highlight--scan-for-focus ()
  (let ((focused-buffer nil))
    (walk-windows
     (lambda (window)
       (let ((has-keyboard-focus
              (and window-highlight-mode
                   (eq (selected-window) window)
                   (window-highlight--has-keyboard-focus-p window))))
         (set-window-parameter
          window window-highlight--has-focus-property has-keyboard-focus)
         (when has-keyboard-focus
           (setf focused-buffer (window-buffer window)))))
     t t)
    focused-buffer))

(defun window-highlight--update-remappings (focused-buffer)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond ((eq buffer focused-buffer)
             (unless window-highlight--face-remap-cookies
               (setf window-highlight--face-remap-cookies
                     (mapcar
                      (lambda (face)
                        (face-remap-add-relative
                         face `(:filtered
                                (:window
                                 ,window-highlight--has-focus-property
                                 t)
                                window-highlight-focused-window)))
                      '(default fringe)))))
            (t
             (when window-highlight--face-remap-cookies
               (mapc #'face-remap-remove-relative
                     window-highlight--face-remap-cookies)
               (setf window-highlight--face-remap-cookies nil)))))))

(defun window-highlight--rescan-windows ()
  (let ((focused-buffer (window-highlight--scan-for-focus)))
    (window-highlight--update-remappings focused-buffer)
    (let ((sb (current-buffer)) (sw (selected-window)))
      (unless (and (eq window-highlight--selected-window sw)
                   (eq window-highlight--current-buffer sb))
        ;; Work around problem with fringe not updating.
        ;; Somehow, forcing this redraw fixes the problem.
        (when nil (redraw-display))
        (setf window-highlight--selected-window sw)
        (setq window-highlight--current-buffer sb)))))

(defun window-highlight--pre-redisplay (_window)
  (window-highlight--rescan-windows))

;;;###autoload
(define-minor-mode window-highlight-mode
  "Apply a face to the window with keyboard focus"
  :lighter nil
  :group 'window-highlight
  :global t
  (progn
    (cond (window-highlight-mode
           (setf window-highlight--current-buffer nil)
           (setf window-highlight--selected-window nil)
           (add-function :after pre-redisplay-function
                         #'window-highlight--pre-redisplay))
          (t
           (remove-function pre-redisplay-function
                            #'window-highlight--pre-redisplay)))
    (window-highlight--rescan-windows)))

(provide 'window-highlight)

;;; window-highlight.el ends here
