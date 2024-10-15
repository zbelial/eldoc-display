;;; eldoc-posframe.el --- Display eldoc in a posframe -*- lexical-binding: t -*-

;; Copyright (C) 2024 zbelial <zjyzhaojiyang@gmail.com>

;; Author: zbelial <zjyzhaojiyang@gmail.com>

;; URL: https://github.com/zbelial/eldoc-posframe
;; Version: 0.1.0
;; Package-Requires: ((posframe "1.0.0") pb)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'posframe)

(defgroup eldoc-display nil
  "Show eldoc info."
  :group 'treesit)

(defcustom eldoc-display-posframe-height-adjust 4
  "Adjust posframe height in order not to hide mode line and minibuffer."
  :type 'integer
  :safe 'integerp
  :group 'eldoc-display)

(defcustom eldoc-display-doc-separator "\n\n"
  "The separator between documentation from different sources."
  :type 'string)

(defcustom eldoc-display-posframe-autohide-timeout 15
  "Child frame will hide itself after this seconds."
  :type 'integer
  :safe 'integerp
  :group 'eldoc-display)

(defcustom eldoc-display-frontend nil
  "Display eldoc in which frontend, posframe or side-window.
If it's nil, use posframe if `display-graphic-p' returns t and posframe is usable,
otherwise, try to use side window.
If none is usable, it will not modify `eldoc-display-functions'."
  :type 'symbol
  :group 'eldoc-display)

(defcustom eldoc-display-side-window-side 'right
  "Which side the side window should be displayed, right or bottom.
When nil, use right."
  :type 'symbol
  :group 'eldoc-display)

(defcustom eldoc-display-side-window-fraction 0.20
  "The height(when `eldoc-display-side-window-fraction' is bottom)
or width(when `eldoc-display-side-window-side' is right) of the side window,
propotion of the frame's geomatric size respectively."
  :type 'float
  :safe 'floatp
  :group 'eldoc-display)

(defvar eldoc-display--posframe-buffer-name " *eldoc-posframe*"
  "Name of buffer used to show context.")

(defvar eldoc-displya-posframe-background-color "#000000"
  "Background color for eldoc-posframe posframe")

(defvar eldoc-display-posframe-border-color "#FFFFFF"
  "Border color for eldoc-posframe posframe")

(defvar eldoc-display-posframe-border-width 1
  "Border width for eldoc-posframe posframe")

(defvar eldoc-display--posframe-frame nil
  "Child frame showing eldoc.")

(defvar-local eldoc-display--old-eldoc-functions nil
  "The original value of ‘eldoc-display-functions’
before enabling eldoc-display.")

(defvar-local eldoc-display--frontend nil
  "Remember which frontend is used actually in current buffer.")

(defun eldoc-display--enable ()
  "Enable eldoc-posframe."
  (setq eldoc-display--frontend nil)
  (cond
   ((and (eq eldoc-display-frontend 'posframe)
         (display-graphic-p)
         (require 'posframe nil t))
    (setq eldoc-display--frontend 'posframe))
   ((and (eq eldoc-display-frontend 'side-window)
         (require 'pb nil t))
    (setq eldoc-display--frontend 'side-window))
   (t
    (cond
     ((and (display-graphic-p)
           (require 'posframe nil t))
      (setq eldoc-display--frontend 'posframe))
     ((require 'pb nil t)
      (setq eldoc-display--frontend 'side-window)))))
  (when eldoc-display--frontend
    (setq-local eldoc-display--old-eldoc-functions
                eldoc-display-functions)
    (setq-local eldoc-display-functions
                (cons 'eldoc-display--display-function
                      (remq 'eldoc-display-in-echo-area
                            eldoc-display-functions)))))

(defun eldoc-display--clear-side-window (&optional main)
  (if (require 'pb nil t)
      (let* ((main (or main (current-buffer)))
             (follower (pb-follower-buffer main)))
        (pb-unpair-two-buffers main follower)
        (kill-buffer follower))))

(defun eldoc-display--disable ()
  "Disable eldoc-posframe."
  (setq-local eldoc-display-functions
              (remq 'eldoc-display--display-function
                    eldoc-display-functions))
  ;; If we removed `eldoc-display-in-echo-area' when enabling
  ;; eldoc-display, add it back.
  (when (memq 'eldoc-display-in-echo-area
              eldoc-display--old-eldoc-functions)
    (setq-local eldoc-display-functions
                (cons 'eldoc-display-in-echo-area
                      eldoc-display-functions)))
  (cond
   ((eq eldoc-display--frontend 'posframe)
    (when eldoc-display--posframe-frame
      (delete-frame eldoc-display--posframe-frame)
      (setq eldoc-display--posframe-frame nil)))
   ((eq eldoc-display--frontend 'side-window)
    (eldoc-display--clear-side-window (current-buffer)))
   (t
    ;; nothing
    )))

(defun eldoc-display--compose-doc (doc)
  "Compose a doc passed from eldoc.

DOC has the form of (TEXT :KEY VAL...), and KEY can be ‘:thing’
and ‘:face’, among other things. If ‘:thing’ exists, it is put at
the start of the doc followed by a colon. If ‘:face’ exists, it
is applied to the thing.

Return the composed string."
  (let ((thing (plist-get (cdr doc) :thing))
        (face (plist-get (cdr doc) :face)))
    (concat (if thing
                (if face
                    (concat (propertize (format "%s" thing) 'face face) ": ")
                  (concat thing ": "))
              "")
            (car doc))))

(defun eldoc-display--posframe-hidehandler-when-buffer-change (info)
  "Posframe hidehandler function.

This function let posframe hide when user switch buffer/kill buffer.
See `posframe-show' for more infor about hidehandler and INFO ."
  (let ((parent-buffer (cdr (plist-get info :posframe-parent-buffer))))
    (or (not (buffer-live-p parent-buffer))
        (and (buffer-live-p parent-buffer)
             (not (equal parent-buffer (current-buffer)))))))

(defun eldoc-display-posframe-poshandler-window-top-right-corner (info)
  "Posframe's position handler.

This poshandler function let top right corner of posframe align to
top left right of window.

The structure of INFO can be found in docstring of
`posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-width (plist-get info :parent-window-width))
         (posframe-width (plist-get info :posframe-width))
         (header-line-height (plist-get info :header-line-height))
         (tab-line-height (plist-get info :tab-line-height)))
    (cons (+ window-left window-width
             (- 0 posframe-width))
          (+ window-top
             header-line-height
             tab-line-height))))

(defun eldoc-display--in-posframe (str)
  "Display eldoc in a posframe."
  (if (require 'posframe nil t)
      (let* ((buffer (get-buffer-create eldoc-display--posframe-buffer-name))
             first-line-p)
        ;; FIXME make text scale configurable
        (with-current-buffer buffer
          (setq text-scale-mode-amount -1)
          (erase-buffer)
          (goto-char (point-min))
          (insert str)
          (text-scale-mode +1))
        (setq eldoc-display--posframe-frame
              (posframe-show buffer
                             :poshandler
                             #'eldoc-display-posframe-poshandler-window-top-right-corner
                             :border-width eldoc-display-posframe-border-width
                             :background-color eldoc-displya-posframe-background-color
                             :internal-border-color eldoc-display-posframe-border-color
                             :internal-border-width eldoc-display-posframe-border-width
                             :width (/ (window-width) 5)
                             :height (- (window-height) eldoc-display-posframe-height-adjust)
                             :accept-focus nil
                             :hidehandler
                             #'eldoc-display--posframe-hidehandler-when-buffer-change
                             :timeout eldoc-display-posframe-autohide-timeout)))
    (message "posframe is unavailable, install it first.")))

;; FIXME after hiding posframe, it won't show automatically again.
(defun eldoc-display-window-size-change-function (_f)
  "Hide posframe."
  (when (and eldoc-display--posframe-frame
             eldoc-display--posframe-buffer-name
             (buffer-live-p (get-buffer eldoc-display--posframe-buffer-name)))
    (posframe-hide eldoc-display--posframe-buffer-name)))
(add-hook 'window-size-change-functions #'eldoc-display-window-size-change-function)

(defun eldoc-display--in-side-window (str)
  "Display eldoc in a side window."
  (if (require 'pb nil t)
      (let ((follower (pb-follower-buffer (current-buffer)))
            (split-type (if (eq eldoc-display-side-window-side 'bottom) "v" "h")))
        (unless follower
          (setq follower (get-buffer-create (concat " *eldoc-display " (buffer-name))))
          ;; FIXME make text scale configurable
          (with-current-buffer follower
            (setq text-scale-mode-amount -1)
            (text-scale-mode +1))
          (pb-pair-two-buffers (current-buffer) follower split-type eldoc-display-side-window-fraction)
          (pb-sync-window))
        (with-current-buffer follower
          (setq buffer-read-only nil)
          (setq truncate-lines nil)
          (setq word-wrap t)
          (erase-buffer)
          (goto-char (point-min))
          (insert str)
          (setq buffer-read-only t)))
    (message "pb is unavailable, install it first from https://github.com/zbelial/pb.el.")))

(defun eldoc-display--display-function (docs interactive)
  "Display DOCS in a posframe or a buffer.
For DOCS and INTERACTIVE see ‘eldoc-display-functions’."
  (let ((doc (string-trim (string-join
                           (mapcar #'eldoc-display--compose-doc docs)
                           eldoc-display-doc-separator))))
    (when (length> (or doc "") 0)
      (cond
       ((and (eq eldoc-display--frontend 'posframe)
             (display-graphic-p))
        (eldoc-display--in-posframe doc))
       ((eq eldoc-display--frontend 'side-window)
        (eldoc-display--in-side-window doc))
       (t
        ;; nothing
        )))))

;;;###autoload
(define-minor-mode eldoc-display-mode
  "Display eldoc in a posframe. "
  :lighter eldoc-posframe-lighter
  (if eldoc-display-mode
      (eldoc-display--enable)
    (eldoc-display--disable)))

(provide 'eldoc-display)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; eldoc-display.el ends here


