#lang scribble/manual

@; NOTE: The contents of this manual should be rendered to text and included in “org-password-manager.el” as comments, so they show up in MELPA and in “M-x list-packages”.

@title{Org Password Manager}
@author{@author+email["Leandro Facchinetti" "me@leafac.com"]}

@emph{Password manager for Org Mode.}

@tabular[#:style 'boxed
         #:sep @hspace[1]
         #:row-properties '(bottom-border)
         `((, @bold{Version} , @seclink["changelog/0.0.1"]{0.0.1})
           (, @bold{Documentation} , @hyperlink["https://www.leafac.com/software/org-password-manager"]{https://www.leafac.com/software/org-password-manager})
           (, @bold{License} , @hyperlink["https://gnu.org/licenses/gpl-3.0.txt"]{GNU General Public License Version 3})
           (, @bold{Code of Conduct} , @hyperlink["http://contributor-covenant.org/version/1/4/"]{Contributor Covenant v1.4.0})
           (, @bold{Distribution} , @hyperlink["http://melpa.org/#/org-password-manager"]{MELPA})
           (, @bold{Source} , @hyperlink["https://git.leafac.com/org-password-manager"]{https://git.leafac.com/org-password-manager})
           (, @bold{Bug Reports} , @para{Write emails to @hyperlink["mailto:org-password-manager@leafac.com"]|{org-password-manager@leafac.com}|.})
           (, @bold{Contributions} , @para{Send @hyperlink["https://git-scm.com/docs/git-format-patch"]{patches} and @hyperlink["https://git-scm.com/docs/git-request-pull"]{pull requests} via email to @hyperlink["mailto:org-password-manager@leafac.com"]|{org-password-manager@leafac.com}|.}))]

@section[#:tag "overview"]{Overview}

@margin-note{Use @hyperlink["https://gnupg.org/"]{GnuPG} to encrypt the Org Mode files that contains credentials instead of storing sensitive information in plain text.}

Use @hyperlink["http://orgmode.org/"]{Org Mode} files to store credentials and retrieve them securely. Integrate with @hyperlink["https://sourceforge.net/projects/pwgen/"]{pwgen} to generate passwords.

@section[#:tag "installation"]{Installation}

Available from @hyperlink["http://melpa.org/#/org-password-manager"]{MELPA}, add the repository to Emacs and install with @tt{M-x package-install}. Password creation requires @hyperlink["https://sourceforge.net/projects/pwgen/"]{pwgen}.

@section[#:tag "usage"]{Usage}

@margin-note{This section assumes the @seclink["configuration"]{default configuration}.}

Add credentials as properties named @tt{USERNAME} and @tt{PASSWORD} to headings in Org Mode files. For example:

@nested[#:style 'code-inset @verbatim{
* [[http://example.com][My favorite website]]
  :PROPERTIES:
  :USERNAME: leandro
  :PASSWORD: chunky-tempeh
  :END:

* SSH key
  :PROPERTIES:
  :PASSWORD: tofu
  :END:
                                      }]

@margin-note{Passwords are cleared from the clipboard after 30 seconds.}

Retrieve usernames with @tt{C-c C-p u} (@tt{org-password-manager-get-username}) and passwords with @tt{C-c C-p p} (@tt{org-password-manager-get-password}). If point is not under a heading that contains credentials, Org Password Manager asks for a heading. To force this behavior even when the point is under a heading that contains credentials, use the @tt{C-u} argument (for example, @tt{C-u C-c C-p u}).

Generate passwords with @tt{C-c C-p g} (@tt{org-password-manager-generate-password}). To customize the parameters to pwgen, use the @tt{C-u} argument (@tt{C-u C-c C-p g}).

@section[#:tag "configuration"]{Configuration}

For the default configuration with the keybindings covered in the @seclink["usage"]{Usage section}, add the following to the Emacs configuration:

@racketblock[
 (add-hook 'org-mode-hook 'org-password-manager-key-bindings)
 ]

To customize the key bindings, start with the following code:

@racketblock[
 (defun org-password-manager-key-bindings ()
   "Binds keys for org-password-manager."
   (local-set-key (kbd "C-c C-p u") 'org-password-manager-get-username)
   (local-set-key (kbd "C-c C-p p") 'org-password-manager-get-password)
   (local-set-key (kbd "C-c C-p g") 'org-password-manager-generate-password))
 ]

For @hyperlink["https://www.gnu.org/software/emacs/manual/ido.html"]{Interactive Do (ido)} support, add the following to the Emacs configuration:

@racketblock[
 (setq org-completion-use-ido t)
 ]

For advanced configuration, refer to @tt{M-x customize-group org-password-manager}.

@section[#:tag "changelog"]{Changelog}

This section documents all notable changes to Org Password Manager. It follows recommendations from @hyperlink["http://keepachangelog.com/"]{Keep a CHANGELOG} and uses @hyperlink["http://semver.org/"]{Semantic Versioning}. Each released version is a Git tag.

@;{
 @subsection[#:tag "changelog/unreleased"]{Unreleased} @; 0.0.1 · 2016-02-23

 @subsubsection[#:tag "changelog/unreleased/added"]{Added}

 @subsubsection[#:tag "changelog/unreleased/changed"]{Changed}

 @subsubsection[#:tag "changelog/unreleased/deprecated"]{Deprecated}

 @subsubsection[#:tag "changelog/unreleased/removed"]{Removed}

 @subsubsection[#:tag "changelog/unreleased/fixed"]{Fixed}

 @subsubsection[#:tag "changelog/unreleased/security"]{Security}
}
@subsection[#:tag "changelog/0.0.1"]{0.0.1 · 2015-07-29}

@subsubsection[#:tag "changelog/0.0.1/added"]{Added}

@itemlist[
 @item{Core functionality.}]
