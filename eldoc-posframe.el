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

(defgroup eldoc-posframe nil
  "Show context information around current point."
  :group 'treesit)

(defcustom eldoc-posframe-frame-min-width 60
  "Minimal width of the child frame."
  :type 'integer
  :safe 'integerp
  :group 'eldoc-posframe)

(defcustom eldoc-posframe-frame-max-width 90
  "Maximum width of the child frame."
  :type 'integer
  :safe 'integerp
  :group 'eldoc-posframe)

(defcustom eldoc-posframe-frame-min-height 5
  "Minimal height of the child frame."
  :type 'integer
  :safe 'integerp
  :group 'eldoc-posframe)

(defcustom eldoc-posframe-frame-font nil
  "Font of the child frame."
  :type 'string
  :group 'eldoc-posframe)

(defcustom eldoc-posframe-frame-font-fraction nil
  "Fraction of font height in the child frame. Prefer this to `eldoc-posframe-frame-font'."
  :type 'float
  :safe 'floatp
  :group 'eldoc-posframe)

(defcustom eldoc-posframe-doc-separator "\n\n"
  "The separator between documentation from different sources."
  :type 'string)

(defcustom eldoc-posframe-frame-autohide-timeout 15
  "Child frame will hide itself after this seconds."
  :type 'integer
  :safe 'integerp
  :group 'eldoc-posframe)

(defvar eldoc-posframe--buffer-name " *eldoc-posframe*"
  "Name of buffer used to show context.")

(defvar eldoc-posframe-background-color "#000000"
  "Background color for eldoc-posframe posframe")

(defvar eldoc-posframe-border-color "#FFFFFF"
  "Border color for eldoc-posframe posframe")

(defvar eldoc-posframe-border-width 1
  "Border width for eldoc-posframe posframe")

(defvar eldoc-posframe--frame nil
  "Child frame showing eldoc.")

(defvar-local eldoc-posframe--old-eldoc-functions nil
  "The original value of ‘eldoc-display-functions’ before enabling eldoc-posframe.")

(defun eldoc-posframe--enable ()
  "Enable eldoc-posframe."
  (setq-local eldoc-posframe--old-eldoc-functions
              eldoc-display-functions)
  (setq-local eldoc-display-functions
              (cons 'eldoc-posframe--display-function
                    (remq 'eldoc-display-in-echo-area
                          eldoc-display-functions))))

(defun eldoc-posframe--disable ()
  "Disable eldoc-posframe."
  (setq-local eldoc-display-functions
              (remq 'eldoc-posframe--display-function
                    eldoc-display-functions))
  ;; If we removed eldoc-display-in-echo-area when enabling
  ;; eldoc-posframe, add it back.
  (when (memq 'eldoc-display-in-echo-area
              eldoc-posframe--old-eldoc-functions)
    (setq-local eldoc-display-functions
                (cons 'eldoc-display-in-echo-area
                      eldoc-display-functions)))

  (when eldoc-posframe--frame
    (delete-frame eldoc-posframe--frame)
    (setq eldoc-posframe--frame nil)))

(defun eldoc-posframe--compose-doc (doc)
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

(defun eldoc-posframe--hidehandler-when-buffer-change (info)
  "Posframe hidehandler function.

This function let posframe hide when user switch buffer/kill buffer.
See `posframe-show' for more infor about hidehandler and INFO ."
  (let ((parent-buffer (cdr (plist-get info :posframe-parent-buffer))))
    (or (not (buffer-live-p parent-buffer))
        (and (buffer-live-p parent-buffer)
             (not (equal parent-buffer (current-buffer)))))))

(defun eldoc-posframe--display (str)
  (let* ((buffer (get-buffer-create eldoc-posframe--buffer-name))
         (bg-mode (frame-parameter nil 'background-mode))
         (max-line-no 0)
         (prefix-len 0)
         (line-no-prefix "")
         (blank-prefix "")
         (padding "  ")
         (font-height (face-attribute 'default :height))
         (frame-font eldoc-posframe-frame-font)
         first-line-p)
    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-min))
      (insert str))
    (when (and font-height
               eldoc-posframe-frame-font-fraction
               (> eldoc-posframe-frame-font-fraction 0.0))
      (setq frame-font nil)
      (setq font-height (round (* font-height eldoc-posframe-frame-font-fraction))))
    (setq eldoc-posframe--frame (posframe-show buffer
                                               :poshandler #'posframe-poshandler-window-top-right-corner
                                               :font frame-font
                                               :border-width eldoc-posframe-border-width
                                               :background-color eldoc-posframe-background-color
                                               :internal-border-color eldoc-posframe-border-color
                                               :internal-border-width eldoc-posframe-border-width
                                               :min-width (min (max eldoc-posframe-frame-min-width (/ (window-width) 3)) (window-width))
                                               :min-height eldoc-posframe-frame-min-height
                                               :max-width (min eldoc-posframe-frame-max-width (/ (window-width) 2))
                                               :accept-focus nil
                                               :hidehandler #'eldoc-posframe--hidehandler-when-buffer-change
                                               :timeout eldoc-posframe-frame-autohide-timeout))
    (when font-height
      (set-face-attribute 'default eldoc-posframe--frame :height font-height))))

(defun eldoc-posframe--eldoc-display-function (str &rest args)
  "Front-end for eldoc.
Display STR in a posframe and ARGS works like `message'."
  (when (stringp str)
    (let* ((doc (string-trim-right (apply #'format str args))))
      (when (not (equal doc ""))
        (eldoc-posframe--display doc)))))

(defun eldoc-posframe--display-function (docs interactive)
  "Display DOCS in a posframe.
For DOCS and INTERACTIVE see ‘eldoc-display-functions’."
  (let ((doc (string-trim (string-join
                           (mapcar #'eldoc-posframe--compose-doc docs)
                           eldoc-posframe-doc-separator))))
    (eldoc-posframe--eldoc-display-function "%s" doc)))

;;;###autoload
(define-minor-mode eldoc-posframe-mode
  "Display eldoc in a posframe. "
  :lighter eldoc-posframe-lighter
  (if eldoc-posframe-mode
      (eldoc-posframe--enable)
    (eldoc-posframe--disable)))

(provide 'eldoc-posframe)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; eldoc-posframe.el ends here


