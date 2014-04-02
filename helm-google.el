;;; helm-google.el --- Emacs Helm Interface for quick Google searches

;; Copyright (C) 2014, Steckerhalter

;; Author: steckerhalter
;; Package-Requires: ((helm "0") (google "0"))
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

(defgroup helm-google '()
  "Customization group for `helm-google'."
  :link '(url-link "http://github.com/steckerhalter/helm-google")
  :group 'convenience
  :group 'comm)

(defcustom helm-google-tld "com"
  "The TLD of the google url to be used (com, de, fr, co.uk etc.)."
  :type 'string
  :group 'helm-google)

(defvar helm-google-input-history nil)

(defun helm-google-url () "URL to google searches."
       (concat "https://www.google." helm-google-tld "/search?ion=1&q=%s"))

(defun helm-google--process-html (html)
  (replace-regexp-in-string
   "\n" ""
   (with-temp-buffer
     (insert html)
     (html2text)
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun helm-google--parse (buf)
  "Extract the search results from BUF."
  (with-current-buffer buf
    (setq case-fold-search nil)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "charset=utf-8" nil t)
        (set-buffer-multibyte t)))
    (goto-char url-http-end-of-headers)
    (prog1 (let (results result)
             (while (re-search-forward "class=\"r\"><a href=\"/url\\?q=\\(.*?\\)&amp;" nil t)
               (setq result (plist-put result :url (match-string-no-properties 1)))
               (re-search-forward "<b>\\(.*?\\)</b>" nil t)
               (setq result (plist-put result :title (match-string-no-properties 1)))
               (re-search-forward "class=\"st\">\\([\0-\377[:nonascii:]]*?\\)</span>" nil t)
               (setq result (plist-put result :content (helm-google--process-html (match-string-no-properties 1))))
               (add-to-list 'results result)
               (setq result nil))
             results)
      (kill-buffer buf))))

(defun helm-google--search (text)
  (let* ((url (message (format (helm-google-url) (url-hexify-string text))))
         (buf (url-retrieve-synchronously url t t))
         (results (helm-google--parse buf)))
    results))

(defun helm-google-search ()
  (let* ((results (helm-google--search helm-pattern)))
    (mapcar (lambda (result)
              (concat
               (propertize
                (plist-get result :title)
                'face 'font-lock-variable-name-face)
               "\n"
               (plist-get result :content)
               "\n"
               (propertize
                (plist-get result :url)
                'face 'link)))
            results)))

(defun helm-google-display-to-real (candidate)
  (caddr (split-string candidate "[\n]+")))

(defvar helm-source-google
  `((name . "Google")
    (init . (lambda () (require 'google)))
    (action ("Browse URL" . browse-url))
    (display-to-real . helm-google-display-to-real)
    (candidates . helm-google-search)
    (requires-pattern)
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
                   (region-end))))
        (helm-input-idle-delay 0.3))
    (helm :sources 'helm-source-google
          :prompt "Google: "
          :input region
          :buffer "*helm google*"
          :history 'helm-google-input-history)))

(provide 'helm-google)

;;; helm-google.el ends here
