# Emacs Settings on Windows

## Synopsys

This Emacs is gnupack.

<http://sourceforge.jp/projects/gnupack/>

## Add-On

### auto-install

    mkdir ~/.emacs.d/elisp
    cd ~/.emacs.d/elisp
    wget  http://www.emacswiki.org/emacs/download/auto-install.el
    M-x byte-compile-file auto-install.el

### anything

    (auto-install-batch "anything")

### anything-c-moccur

    M-x install-elisp-from-emacswiki color-moccur.el
    (install-elisp "http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el")

### anything-for-tags

    M-x install-elisp-from-emacswiki anything-gtags.el
    M-x install-elisp-from-emacswiki anything-exuberant-ctags.el

### redo+

    M-x install-elisp-from-emacswiki redo+.el

### auto-save-buffers

    (install-elisp "http://homepage3.nifty.com/oatu/emacs/archives/auto-save-buffers.el")

### text-adjust-buffer

    (install-elisp "http://taiyaki.org/elisp/mell/src/mell.el")
    (install-elisp "http://taiyaki.org/elisp/text-adjust/src/text-adjust.el")

#### Note
     taiyaki.org が応答しない？
