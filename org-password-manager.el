;;; org-password-manager.el --- Minimal password manager for Emacs Org Mode.

;; Copyright (C) 2015 - Leandro Facchinetti <me@leafac.com>

;; Author: Leandro Facchinetti <me@leafac.com>
;; Version: 0.0.1
;; Keywords: password
;; URL: https://github.com/leafac/org-password-manager
;; Package-Requires: ((org "8.2.10") (s "1.9.0"))

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

;; Refer to `README.md' at https://github.com/leafac/org-password-manager.

;;; Code:

(require 'org)
(require 's)

(defvar org-password-manager-history ()
  "The history of headers that were chosen for `org-password-manager'.")

(defvar org-password-manager-default-pwgen-command "pwgen --secure --symbols --capitalize --numerals 25 1"
  "The default `pwgen' command to use when generating passwords.")

(defun org-password-manager-get-property (property-name &optional ask-for-input?)
  "Get PROPERTY-NAME.

If ASK-FOR-INPUT? is t, will ask for input even if point is on a
heading that contains the property."
  (let ((heading nil)
        (property (org-entry-get (point) property-name t))
        (success-message nil))
    (if (and property (not ask-for-input?))
        (setq heading (org-link-display-format (org-get-heading t t)))
      (let* ((property-entries
              (org-map-entries
               (lambda ()
                 (list
                  (org-link-display-format (org-get-heading t t))
                  (org-entry-get (point) property-name)))
               (concat property-name "={.+}") 'agenda))
             (header-property-list (assoc (ido-completing-read (concat property-name " for: ")
                                                               property-entries
                                                               nil
                                                               nil
                                                               nil
                                                               'org-password-manager-history
                                                               (car org-password-manager-history))
                                          property-entries)))
        (setq heading (nth 0 header-property-list)
              property (nth 1 header-property-list))))
    (if (string= property-name "PASSWORD")
        (progn
          (funcall interprogram-cut-function property)
          (run-at-time "30 sec" nil (lambda () (funcall interprogram-cut-function "")))
          (setq success-message
                (concat property-name " for `" heading "' securely copied to system's clipboard avoiding kill ring and will be removed in 30 seconds.")))
      (progn (kill-new property)
             (setq success-message
                   (concat property-name " for `" heading "' copied to clipboard."))))
    (add-to-history 'org-password-manager-history heading)
    (message success-message)))

(defun org-password-manager-get-username (&optional ask-for-input?)
  "Get username.

If ASK-FOR-INPUT? is t, will ask for input even if point is on a
heading that contains the username property."
  (interactive "P")
  (org-password-manager-get-property "USERNAME" ask-for-input?))

(defun org-password-manager-get-password (&optional ask-for-input?)
  "Get password.

If ASK-FOR-INPUT? is t, will ask for input even if point is on a
heading that contains the password property."
  (interactive "P")
  (org-password-manager-get-property "PASSWORD" ask-for-input?))

(defun org-password-manager-generate-password (&optional edit-pwgen-string?)
  "Generate password.

If EDIT-PWGEN-STRING? is t, let the user edit the pwgen command
line before running it."
  (interactive "P")
  (let* ((pwgen-string (if edit-pwgen-string?
                           (read-from-minibuffer "pwgen command to run: " org-password-manager-default-pwgen-command)
                         org-password-manager-default-pwgen-command))
         (generated-password (s-trim (shell-command-to-string pwgen-string))))
    (insert generated-password)
    (funcall interprogram-cut-function generated-password)
    (message "Generated password inserted on buffer and securely copied to system's clipboard avoiding kill ring.")))

;; Key bindings.

(defun org-password-manager-key-bindings ()
  "Binds keys for org-password-manager."
  (local-set-key (kbd "C-c C-p u") 'org-password-manager-get-username)
  (local-set-key (kbd "C-c C-p p") 'org-password-manager-get-password)
  (local-set-key (kbd "C-c C-p g") 'org-password-manager-generate-password))

(provide 'org-password-manager)

;;; org-password-manager.el ends here
