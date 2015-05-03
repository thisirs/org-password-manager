;;; org-password-manager.el --- Minimal password manager for Emacs Org Mode.

;; Copyright (C) 2015 - Leandro Facchinetti <me@leafac.com>

;; Author: Leandro Facchinetti <me@leafac.com>
;; Created: 2015-05-03
;; Version: 0.0.1
;; Keywords: password passwords
;; Homepage: https://github.com/leafac/org-password-manager
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

;; org-password-manager
;; ====================

;; Your Passwords in Plain Text
;; ----------------------------

;; [org-password-manager][org-password-manager] is a minimal password manager for
;; [Emacs][emacs] [Org mode][org-mode].

;; Philosophy
;; ----------

;; 1. Easy to learn and use.
;; 2. Don't recreate features that already exist elsewhere (e.g. secure password
;;    generation).
;; 3. Don't do fancy security measures.
;; 4. Restrict the users the least possible.

;; Features
;; --------

;; 1. Use [Org mode][org mode] files as password manager.
;; 2. Retrieve passwords in a practical and secure manner.
;; 3. Generate secure passwords.
;; 4. No configuration required.

;; Installation
;; ------------

;; Currently, this package is still in development and is **NOT READY FOR PUBLIC
;; USE**. Thus, I didn't make it available on [MELPA][melpa] yet.

;; If you're a developer and you're considering joining the effort to build this
;; tool, you should at least know how to clone this repository and require to
;; appropriate files.

;; One requirement developers should be aware is that
;; [org-password-manager][org-password-manager] uses [pwgen][pwgen] to generate
;; passwords. So you need [pwgen][pwgen] installed to use that feature.

;; Usage
;; -----

;; ### Store passwords in [Org mode][org-mode] files

;; Follow the example:

;;     * My favorite website
;;       :PROPERTIES:
;;       :USERNAME: leandro
;;       :PASSWORD: chunky-tempeh
;;       :END:

;; ### Get username

;; Run `M-x org-password-manager-get-username` and search for the title of the
;; entry containing the `USERNAME` property (e.g. "My favorite website").

;; ### Get password

;; Run `M-x org-password-manager-get-password` and search for the title of the
;; entry containing the `USERNAME` property (e.g. "My favorite website").

;; ### Generate password

;; Run `M-x org-password-manager-generate-password` and the generated password will
;; be inserted under the point on the buffer.

;; Configuration
;; -------------

;; # TODO: Suggested key bindings.
;; # TODO: Explain file lookup.

;; References
;; ----------

;; This work was first inspired by [Emacs][emacs] and [Org mode][org-mode],
;; obviously.

;; But I also want to cite two other projects that are similar in spirit to
;; [org-password-manager][org-password-manager]. They aim to accomplish the same
;; goal - i.e. using [Emacs][emacs] [Org mode][org-mode] as a password
;; manager. Though they differ on design from each other and from
;; [org-password-manager][org-password-manager]. Thus, the effort to create
;; [org-password-manager][org-password-manager] is still justified.

;; Those related projects are both called org-passwords. One was created by
;; [Jorge Alfaro-Murillo][jorge-alfaro-murillo] and the other by
;; [Andrea Crotti][andrea-crotti].

;; [Jorge Alfaro-Murillo's org-passwords][jorge-alfaro-murillo] has lots of
;; features, way more than [org-password-manager][org-password-manager] plans to
;; have. For example, it implements its own password generator, requires
;; configuration pointing to a password file that should only contain passwords and
;; opens that file in read-only mode with a timeout.

;; [org-password-manager][org-password-manager], on the other hand, uses
;; [pwgen][pwgen] to generate passwords, handles passwords stored on the middle of
;; any [Org mode][org-mode] file with other contents and doesn't open those files
;; in any special way.

;; [Andrea Crotti's org-passwords][andrea-crotti] is more minimal than
;; [org-password-manager][org-password-manager] aims to be. It only retrieves
;; passwords for the entry under the point, generates passwords by calling
;; [pwgen][pwge] and has almost no documentation, requiring the user to read the
;; source.

;; I appreciate the mentioned works and thank its authors.


;; [emacs]: https://www.gnu.org/software/emacs/
;; [org-password-manager]: https://github.com/leafac/org-password-manager
;; [melpa]: http://melpa.org/
;; [jorge-alfaro-murillo]: https://bitbucket.org/alfaromurillo/org-passwords.el
;; [andrea-crotti]: https://github.com/AndreaCrotti/org-passwords/
;; [pwgen]: http://pwgen.sourceforge.net/

;;; Code:

(require 'org)
(require 's)

(defvar org-password-manager-history ()
  "The history of headers that were chosen for org-password-manager.")

(defvar org-password-manager-default-pwgen-command "pwgen --secure --symbols --capitalize --numerals 25 1"
  "The default pwgen command to use when generating passwords.")

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
