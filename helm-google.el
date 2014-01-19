;;; helm-google.el --- Emacs Helm Interface for quick Google searches

;; Copyright (C) 2014, Steckerhalter

;; Author: steckerhalter
;; Package-Requires: ((helm "0"))
;; URL: https://github.com/steckerhalter/helm-google
;; Keywords: helm google search browse

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Emacs Helm Interface for quick Google searches

;;; Code:

(require 'helm)
(require 'google)

(defvar helm-google-input-history nil)

(defun helm-google-search ()
  (let* ((results (google-search helm-pattern))
         (responseData (google-result-field 'responseData results))
         (records (google-result-field 'results responseData)))
    (mapcar (lambda (record)
              (concat
               (propertize
                (google-result-field 'titleNoFormatting record)
                'face 'font-lock-variable-name-face)
               "\n"
               (replace-regexp-in-string
                "\n" ""
                (with-temp-buffer
                  (insert (google-result-field 'content record))
                  (html2text)
                  (buffer-substring-no-properties (point-min) (point-max))))
               "\n"
               (propertize
                (google-result-field 'url record)
                'face 'link)))
            records)))

(defun helm-google-display-to-real (candidate)
  (caddr (split-string candidate "[\n]+")))

(defvar helm-source-google
  `((name . "Google")
    (init . (lambda () (require 'google)))
    (action ("Browse URL" . browse-url))
    (display-to-real . helm-google-display-to-real)
    (candidates . helm-google-search)
    (requires-pattern . 3)
    (nohighlight)
    (multiline)
    (volatile)))

;;;###autoload
(defun helm-google ()
  "Preconfigured `helm' : Google search."
  (interactive)
  (let ((google-referer "https://github.com/steckerhalter/helm-google")
        (region (when (use-region-p)
                  (buffer-substring-no-properties
                   (region-beginning)
                   (region-end)))))
    (helm :sources 'helm-source-google
          :prompt "Google: "
          :input region
          :buffer "*helm google*"
          :history 'helm-google-input-history)))

(provide 'helm-google)

;;; helm-google.el ends here
