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
              (format "%s\n%s\n\%s"
                      (google-result-field 'titleNoFormatting record)
                      (replace-regexp-in-string
                       "\n" ""
                       (with-temp-buffer
                         (insert (google-result-field 'content record))
                         (html2text)
                         (buffer-substring-no-properties (point-min) (point-max))))
                      (google-result-field 'url record)))
            records)))

(defvar helm-source-google
  `((name . "Google")
    (init . (lambda () (require 'google)))
    (candidates . helm-google-search)
    (action ("Browse URL" . browse-url))
    (multiline)
    (delayed . 0.5)
    (volatile)))

(defun helm-google ()
  "Preconfigured `helm' : Google search."
  (interactive)
  (let ((query (read-string "Google: " nil 'helm-google-input-history))
        (google-referer "https://github.com/steckerhalter/helm-google"))
    (helm :sources 'helm-source-google
          :prompt "Google: "
          :buffer "*helm google*"
          :input query
          :history 'helm-google-input-history)))

(provide 'helm-google)

;;; helm-google.el ends here
