# Arvid Google Translate
Arvid Google Translate is a minor mode for Emacs that allows the user
to interactively translate words and sentences using Google Translate.

## Installation

Download **arvid-google-translate.el**,
**arvid-google-translate-languages.el** and **json.el** and put them
in Emacs load path. Then require it:

     $ git clone git@github.com:arvixx/arvid-google-translate.git ~/.emacs.d

     (add-to-list 'load-path "~/.emacs.d/arvid-google-translate")
     (require 'arvid-google-translate)

## Usage
 To start arvid-google-translate:
    M-x agt

arvid-google-translate will open two windows in the bottom part of
your current window. If this is the first time agt is used, it will
ask you about which language you which to translate from and to. This
is easy to change later.

   The left window is the source window. The text written in this window
will be automatically translated as you write and the translations
appear in the right window.

To switch to another language pair, use M-x agt-read-languages (bound
to C-c C-l). To only change the source or target language, use M-x
agt-read-language-source (C-c C-s) or agt-read-language-target (C-c
C-t) respectively.

To swap the language pair, use M-x agt-swap-languages (C-c C-w). This
means, that if you are translating from Swedish to English, calling
agt-swap-languages will make you translate from English to
Swedish. This function also switches the text of the source and target
buffers.

## Known problems

* A lot of hidden http-buffers are created that are not disposed of
  properly.
* When I write too fast, I get error messages like this:

	   error in process filter: progn: Selecting deleted buffer
	   error in process filter: Selecting deleted buffer
