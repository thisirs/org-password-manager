
This work was first inspired by [Emacs][emacs] and [Org mode][org-mode],
obviously.

But I also want to cite two other projects that are similar in spirit to
[org-password-manager][org-password-manager]. They aim to accomplish the same
goal - i.e. using [Emacs][emacs] [Org mode][org-mode] as a password
manager. Though they differ on design from each other and from
[org-password-manager][org-password-manager]. Thus, the effort to create
[org-password-manager][org-password-manager] is still justified.

Those related projects are both called org-passwords. One was created by
[Jorge Alfaro-Murillo][jorge-alfaro-murillo] and the other by
[Andrea Crotti][andrea-crotti].

[Jorge Alfaro-Murillo's org-passwords][jorge-alfaro-murillo] has lots of
features, way more than [org-password-manager][org-password-manager] plans to
have. For example, it implements its own password generator, requires
configuration for pointing to a password file that should only contain passwords
and opens that file in read-only mode with a timeout. It's so complete that it's
in the official distribution of [Org mode][org-mode] under
[org-contrib][jorge-alfaro-murillo-org-contrib].

[org-password-manager][org-password-manager], on the other hand, uses
[pwgen][pwgen] to generate passwords, handles passwords stored on the middle of
any [Org mode][org-mode] file with other contents and doesn't open those files
in any special way.

[Andrea Crotti's org-passwords][andrea-crotti] is more minimal than
[org-password-manager][org-password-manager] aims to be. It only retrieves
passwords for the entry under the point, generates passwords by calling
[pwgen][pwgen] and has almost no documentation, requiring the user to read the
source.

I appreciate the mentioned works and thank its authors.


[org-mode]: http://orgmode.org/
[emacs]: https://www.gnu.org/software/emacs/
[org-password-manager]: https://github.com/leafac/org-password-manager
[melpa]: http://melpa.org/
[jorge-alfaro-murillo]: https://bitbucket.org/alfaromurillo/org-passwords.el
[andrea-crotti]: https://github.com/AndreaCrotti/org-passwords/
[pwgen]: http://pwgen.sourceforge.net/
[jorge-alfaro-murillo-org-contrib]: http://orgmode.org/cgit.cgi/org-mode.git/tree/contrib/lisp/org-passwords.el
[ido]: https://www.gnu.org/software/emacs/manual/ido.html
