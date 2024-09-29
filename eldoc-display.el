;;; eldoc-posframe.el --- Display eldoc in a posframe -*- lexical-binding: t -*-

;; Copyright (C) 2024 zbelial <zjyzhaojiyang@gmail.com>

;; Author: zbelial <zjyzhaojiyang@gmail.com>

;; URL: https://github.com/zbelial/eldoc-posframe
;; Version: 0.1.0
;; Package-Requires: ((posframe "1.0.0"))

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

(defcustom eldoc-display-min-width 60
  "Minimal width of the child frame."
  :type 'integer
  :safe 'integerp
  :group 'eldoc-display)

(defcustom eldoc-display-max-width 90
  "Maximum width of the child frame."
  :type 'integer
  :safe 'integerp
  :group 'eldoc-display)

(defcustom eldoc-display-max-height 18
  "Maximum height of the child frame."
  :type 'integer
  :safe 'integerp
  :group 'eldoc-display)

(defcustom eldoc-display-min-height 5
  "Minimal height of the child frame."
  :type 'integer
  :safe 'integerp
  :group 'eldoc-display)

(defcustom eldoc-display-posframe-frame-font nil
  "Font of the child frame."
  :type 'string
  :group 'eldoc-display)

(defcustom eldoc-display-posframe-frame-font-fraction nil
  "Fraction of font height in the child frame. Prefer this to `eldoc-display-posframe-frame-font'."
  :type 'float
  :safe 'floatp
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
  "Display eldoc in which frontend, 'posframe, 'window.
If it's null, use posframe if `display-graphic-p' returns t, otherwise, use window."
  :type 'boolean
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

(defun eldoc-display--enable ()
  "Enable eldoc-posframe."
  (setq-local eldoc-display--old-eldoc-functions
              eldoc-display-functions)
  (setq-local eldoc-display-functions
              (cons 'eldoc-display--display-function
                    (remq 'eldoc-display-in-echo-area
                          eldoc-display-functions))))

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

  (when eldoc-display--posframe-frame
    (delete-frame eldoc-display--posframe-frame)
    (setq eldoc-display--posframe-frame nil)))

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
  (if (require 'posframe nil t)
      (let* ((buffer (get-buffer-create eldoc-display--posframe-buffer-name))
             (font-height (face-attribute 'default :height))
             (frame-font eldoc-display-posframe-frame-font)
             first-line-p)
        (with-current-buffer buffer
          (erase-buffer)
          (goto-char (point-min))
          (insert str))
        (when (and font-height
                   eldoc-display-posframe-frame-font-fraction
                   (> eldoc-display-posframe-frame-font-fraction 0.0))
          (setq frame-font nil)
          (setq font-height (round
                             (* font-height eldoc-display-posframe-frame-font-fraction))))
        (setq eldoc-display--posframe-frame
              (posframe-show buffer
                             :poshandler
                             #'eldoc-display-posframe-poshandler-window-top-right-corner
                             :font frame-font
                             :border-width eldoc-display-posframe-border-width
                             :background-color eldoc-displya-posframe-background-color
                             :internal-border-color eldoc-display-posframe-border-color
                             :internal-border-width eldoc-display-posframe-border-width
                             :min-width
                             (min (max
                                   eldoc-display-min-width
                                   (/ (window-width) 3))
                                  (window-width))
                             :min-height eldoc-display-min-height
                             :max-width
                             (min eldoc-display-max-width (/ (window-width) 2))
                             :max-height
                             (min eldoc-display-max-height (* 2 (/ (window-height) 3)))
                             :accept-focus nil
                             :hidehandler
                             #'eldoc-display--posframe-hidehandler-when-buffer-change
                             :timeout eldoc-display-posframe-autohide-timeout))
        (when font-height
          (set-face-attribute 'default eldoc-display--posframe-frame :height font-height)))
    (message "posframe is unavailable, install it first.")))

(defvar eldoc-display--popup nil)

(defun eldoc-display--in-popup (str)
  (if (require 'popup nil t)
      (let* ((parent-window (selected-window))
             (parent-window-start (window-start parent-window))
             (parent-window-end (window-end parent-window))
             (parent-window-top (window-pixel-top parent-window))
             (parent-window-left (window-pixel-left parent-window))
             (parent-window-width (window-pixel-width parent-window))
             (parent-window-height (window-pixel-height parent-window))
             (mode-line-height (window-mode-line-height))
             (minibuffer-height (window-pixel-height (minibuffer-window)))
             (header-line-height (window-header-line-height parent-window))
             (tab-line-height (if (functionp 'window-tab-line-height)
                                  (window-tab-line-height)
                                0))
             (mouse-position (cdr (mouse-pixel-position))))
        )
    (message "popup is unavailable, install it first.")))

(defun eldoc-display--display-function (docs interactive)
  "Display DOCS in a posframe or a popup.
For DOCS and INTERACTIVE see ‘eldoc-display-functions’."
  (let ((doc (string-trim (string-join
                           (mapcar #'eldoc-display--compose-doc docs)
                           eldoc-display-doc-separator))))
    (when (length> (or doc "") 0)
      (cond
       ((eq eldoc-display-frontend 'posframe)
        (eldoc-display--in-posframe doc))
       ((eq eldoc-display-frontend 'popup)
        (eldoc-display--in-popup doc))
       (t
        (cond
         ((display-graphic-p)
          (eldoc-display--in-posframe doc))
         (t
          (eldoc-display--in-popup doc))))))))

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


