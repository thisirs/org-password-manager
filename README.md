org-password-manager
====================

**Note**: This project is still in early draft version.

Your Passwords in Plain Text
----------------------------

[org-password-manager][org-password-manager] is a minimal password manager for
[Emacs][emacs] [Org mode][org-mode].

Philosophy
----------

1. Easy to learn and use.
2. Don't recreate features that already exist elsewhere (e.g. secure password
   generation).
3. Don't do fancy security measures.
4. Restrict the users the least possible.

Features
--------

1. Use [Org mode][org mode] files as password manager.
2. Retrieve passwords in a practical and secure manner.
3. Generate secure passwords.
4. No configuration required.

Installation
------------

Currently, this package is still in development and is **NOT READY FOR PUBLIC
USE**. Thus, I didn't make it available on [MELPA][melpa] yet.

If you're a developer and you're considering joining the effort to build this
tool, you should at least know how to clone this repository and require to
appropriate files.

One requirement developers should be aware is that
[org-password-manager][org-password-manager] uses [pwgen][pwgen] to generate
passwords. So you need [pwgen][pwgen] installed to use that feature.

Usage
-----

### Store passwords in [Org mode][org-mode] files

Follow the example:

    * My favorite website
      :PROPERTIES:
      :USERNAME: leandro
      :PASSWORD: chunky-tempeh
      :END:

### Get username

Run `M-x org-password-manager-get-username` and search for the title of the
entry containing the `USERNAME` property (e.g. "My favorite website").

### Get password

Run `M-x org-password-manager-get-password` and search for the title of the
entry containing the `USERNAME` property (e.g. "My favorite website").

### Generate password

Run `M-x org-password-manager-generate-password` and the generated password will
be inserted under the point on the buffer.

Configuration
-------------

# TODO: Suggested key bindings.
# TODO: Explain file lookup.

References
----------

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
configuration pointing to a password file that should only contain passwords and
opens that file in read-only mode with a timeout.

[org-password-manager][org-password-manager], on the other hand, uses
[pwgen][pwgen] to generate passwords, handles passwords stored on the middle of
any [Org mode][org-mode] file with other contents and doesn't open those files
in any special way.

[Andrea Crotti's org-passwords][andrea-crotti] is more minimal than
[org-password-manager][org-password-manager] aims to be. It only retrieves
passwords for the entry under the point, generates passwords by calling
[pwgen][pwge] and has almost no documentation, requiring the user to read the
source.

I appreciate the mentioned works and thank its authors.


[emacs]: https://www.gnu.org/software/emacs/
[org-password-manager]: https://github.com/leafac/org-password-manager
[melpa]: http://melpa.org/
[jorge-alfaro-murillo]: https://bitbucket.org/alfaromurillo/org-passwords.el
[andrea-crotti]: https://github.com/AndreaCrotti/org-passwords/
[pwgen]: http://pwgen.sourceforge.net/
