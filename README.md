# vhdl-capf
Emacs completion at point function (capf) for vhdl-mode.

## Description
This file provides a completion at point function (capf) for the
vhdl major mode. To use this correctly, the vhdl major mode has to be enabled
and working (which is by default the case for newer Emacs installations). The
basic intention for this minor mode is completion with fuzzy matching. If you
don't want this better ignore this package and stick with the vhdl major-mode's
own completion. To get auto-completion with fuzzy matching, some completion
toolkit has to be enabled that is able to do fuzzy matching and uses
cap-functions. This could be achieved by company-mode with enabled company-flx
package. So install and activate those two first (or another completion
framework like auto-complete) then use this vhdl completion at point function.

## Installation
If you don't use a package managing system like melpa, you have to include
this file by hand, using `(load-path "/path/to/where/this/file/resides")`.
After this (and if installing by melpa) you can enable the capf by the following
in your .emacs file:
```elisp
(when (require 'vhdl-capf)
  (vhdl-capf-enable))
```

## Customization
There is mainly one parameter that can be set by the user, according to his
needs: _vhdl-search-completion-buffers_. As stated in the belonging comment,
the value of this variable controls the amount of other buffers that is being
searched for completion candidates. In principle there are two settings: t
and a number. If this is t ALL available buffers with the vhdl major-mode
will be searched. If the value is a number, the (number+1) last opened
buffers with vhdl major-mode will be searched. But: the search for completion
candidates is cached. This means the **active** vhdl-buffer is searched for
candidates every time the capf is invoked. All other vhdl-buffers that shall
be searched are just scanned once and after that the cached word list is
taken.
