;;; eldoc-posframe.el --- Display eldoc in a posframe -*- lexical-binding: t -*-

;; Copyright (C) 2024 zbelial <zjyzhaojiyang@gmail.com>

;; Author: zbelial <zjyzhaojiyang@gmail.com>

;; URL: https://github.com/zbelial/eldoc-posframe
;; Version: 0.1.0
;; Package-Requires: ()

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

(defgroup eldoc-display nil
  "Show eldoc info."
  :group 'treesit)

(defcustom eldoc-display-posframe-height-adjust 0
  "Adjust posframe height in lines in order not to hide mode line and minibuffer."
  :type 'integer
  :group 'eldoc-display)

(defcustom eldoc-display-posframe-x-adjust 0
  "Adjust posframe's x position in pixel."
  :type 'integer
  :group 'eldoc-display)

(defcustom eldoc-display-doc-separator "\n\n"
  "The separator between documentation from different sources."
  :type 'string)

(defcustom eldoc-display-posframe-min-width nil
  "Minimal width of the child frame.
When nil, use `window-width' divided by 5."
  :type 'integer
  :group 'eldoc-display)

(defcustom eldoc-display-posframe-max-width nil
  "Maximum width of the child frame.
When nil, use `window-width' divided by 5."
  :type 'integer
  :group 'eldoc-display)

(defcustom eldoc-display-posframe-min-height 5
  "Minimal height of the child frame."
  :type 'integer
  :group 'eldoc-display)

(defcustom eldoc-display-posframe-max-height nil
  "Maximum height of the child frame.
When nil, use `window-height' minus `eldoc-display-posframe-height-adjust'."
  :type 'integer
  :group 'eldoc-display)

(defcustom eldoc-display-posframe-autohide-timeout 15
  "Child frame will hide itself after this seconds."
  :type 'integer
  :group 'eldoc-display)

(defcustom eldoc-display-posframe-parameters nil
  "The frame parameters used by posframe."
  :type 'string)

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
  :group 'eldoc-display)

(defvar eldoc-display--side-window-buffer-name " *eldoc display side window*"
  "Name of buffer used to show eldoc.")

(defvar eldoc-display--posframe-buffer-name " *eldoc display posframe*"
  "Name of buffer used to show eldoc.")

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

(defvar eldoc-display--side-window-parameters
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t))))

(defun eldoc-display--enable ()
  "Enable eldoc-posframe."
  (setq eldoc-display--frontend nil)
  (cond
   ((and (eq eldoc-display-frontend 'posframe)
         (display-graphic-p)
         (require 'posframe nil t)
         (posframe-workable-p))
    (setq eldoc-display--frontend 'posframe))
   ((eq eldoc-display-frontend 'side-window)
    (setq eldoc-display--frontend 'side-window))
   (t
    (cond
     ((and (display-graphic-p)
           (require 'posframe nil t)
           (posframe-workable-p))
      (setq eldoc-display--frontend 'posframe))
     (t
      (setq eldoc-display--frontend 'side-window)))))
  (when eldoc-display--frontend
    (setq-local eldoc-display--old-eldoc-functions
                eldoc-display-functions)
    (setq-local eldoc-display-functions
                (cons 'eldoc-display--display-function
                      (remq 'eldoc-display-in-echo-area
                            eldoc-display-functions)))))

(defun eldoc-display--clear-posframe ()
  (when eldoc-display--posframe-frame
    (delete-frame eldoc-display--posframe-frame)
    (setq eldoc-display--posframe-frame nil)))

(defun eldoc-display--clear-side-window ()
  (ignore-errors (delete-window (get-buffer-window eldoc-display--side-window-buffer-name))))

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
  (eldoc-display--clear-posframe)
  (eldoc-display--clear-side-window))

(defun eldoc-display-toggle-frontend ()
  "Toggle front end between 'posframe and 'side-windows temporarily in current session."
  (interactive)
  (when eldoc-display-mode
    (cond
     ((and (eq eldoc-display--frontend 'side-window)
           (and (display-graphic-p)
                (require 'posframe nil t)
                (posframe-workable-p)))
      (setq eldoc-display--prev-doc "")
      (eldoc-display--clear-side-window)
      (setq eldoc-display--frontend 'posframe))
     ((eq eldoc-display--frontend 'posframe)
      (setq eldoc-display--prev-doc "")
      (eldoc-display--clear-posframe)
      (setq eldoc-display--frontend 'side-window)))))

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
             (- 0 posframe-width)
             (or eldoc-display-posframe-x-adjust 0))
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
                             :min-width
                             (or eldoc-display-posframe-min-width
                                 (/ (window-width) 5))
                             :max-width
                             (or eldoc-display-posframe-max-width
                                 (/ (window-width) 5))
                             :min-height (or eldoc-display-posframe-min-height 5)
                             :max-height
                             (or eldoc-display-posframe-max-height
                                 (- (window-height)
                                    eldoc-display-posframe-height-adjust))
                             ;; :width (/ (window-width) 5)
                             ;; :height (- (window-height) eldoc-display-posframe-height-adjust)
                             :accept-focus nil
                             :override-parameters eldoc-display-posframe-parameters
                             :hidehandler
                             #'eldoc-display--posframe-hidehandler-when-buffer-change
                             :timeout eldoc-display-posframe-autohide-timeout)))
    (message "posframe is unavailable, install it first.")))

;; FIXME after hiding posframe, it won't show automatically again.
;; Also, after window-size-change event, it won't show for the first time.
;; It seems that it showes and hides immediately. Have no idea why.
(defun eldoc-display-window-size-change-function (_f)
  "Hide posframe."
  (when (and eldoc-display--posframe-frame
             eldoc-display--posframe-buffer-name
             (buffer-live-p (get-buffer eldoc-display--posframe-buffer-name)))
    (posframe-hide eldoc-display--posframe-buffer-name)))
;; (add-hook 'window-size-change-functions #'eldoc-display-window-size-change-function)
;; (remove-hook 'window-size-change-functions #'eldoc-display-window-size-change-function)

(defun eldoc-display--in-side-window (str)
  "Display eldoc in a side window."
  (with-current-buffer (get-buffer-create eldoc-display--side-window-buffer-name)
    (setq text-scale-mode-amount -1)
    (erase-buffer)
    (goto-char (point-min))
    (insert str)
    (text-scale-mode +1))
  (display-buffer (get-buffer-create eldoc-display--side-window-buffer-name)))

(defvar-local eldoc-display--prev-doc nil)
(defun eldoc-display--display-function (docs interactive)
  "Display DOCS in a posframe or a buffer.
For DOCS and INTERACTIVE see ‘eldoc-display-functions’."
  (let ((doc (string-trim (string-join
                           (mapcar #'eldoc-display--compose-doc docs)
                           eldoc-display-doc-separator))))
    (when (and (length> (or doc "") 0)
               (or (null eldoc-display--prev-doc)
                   (not (string-equal (substring-no-properties eldoc-display--prev-doc)
                                      (substring-no-properties doc)))))
      (setq eldoc-display--prev-doc doc)
      (cond
       ((eq eldoc-display--frontend 'posframe)
        (eldoc-display--in-posframe doc))
       ((eq eldoc-display--frontend 'side-window)
        (eldoc-display--in-side-window doc))))))

(add-to-list 'display-buffer-alist
             `(,eldoc-display--side-window-buffer-name
               display-buffer-in-side-window
               (side . ,eldoc-display-side-window-side)
               (slot . -1)
               (window-width . ,eldoc-display-side-window-fraction)
               ;; ,eldoc-display--side-window-parameters
               ))

;;;###autoload
(define-minor-mode eldoc-display-mode
  "Display eldoc in a posframe or a side window."
  :global nil
  (if eldoc-display-mode
      (eldoc-display--enable)
    (eldoc-display--disable)))

(provide 'eldoc-display)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; eldoc-display.el ends here


