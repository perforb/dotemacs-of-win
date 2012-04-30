# Emacs Setting on Windows

This project customizes the setting of Emacs which included in gnupack.

See <http://sourceforge.jp/projects/gnupack/>

# About `init.el`

本プロジェクトにおける `init.el` は gnupack_basic-8.00 に付属のファイルを基にカスタマイズしています.
なお, gnupack のバージョンが異なる場合は `init.el` 内の `Customize Area` 以前の部分をそのバージョンに準拠する内容に置き換えてください.

# git on mintty

> gnupack のインストール先は `c:\` であるものとします.

git は gnupack 付属の mintty からインストールおよび利用します.
eshell 上から git を利用する場合は, .ssh/config の permission が 600 とならないので push のみできません.
なお, mintty 自体のカスタマイズは ~/.minttyrc ではなく c:/gnupack_basic-8.00/config.ini で設定します.
以下, ターミナル上での具体的な手順です.

    # ミラーサイトのプロトコルを変更
    $ vi c:/gnupack_basic-8.00/app/script/apt-cyg
  
        # ftp ではつながらないので http に変更
        # mirror=ftp://mirror.mcs.anl.gov/pub/cygwin
        mirror=http://mirror.mcs.anl.gov/pub/cygwin
  
    # `apt-cyg` で git をインストールおよび初期設定
    $ bash apt-cyg install git
    $ git config --global user.name "your_name"
    $ git config --global user.email "your_email"
    $ git config --global color.ui auto

    # github に接続するための ssh の設定
    $ cd ~/
    $ mkdir .ssh
    $ cd .ssh/
    $ ssh-keygen -t rsa -f id_rsa

    github に `id_rsa.pub` の内容を登録する.

    $ vi ~/.ssh/config
  
        Host github.com
        User your_name
        Port 22
        Hostname github.com
        IdentityFile ~/.ssh/id_rsa
        TCPKeepAlive yes
        IdentitiesOnly yes

# Install

    $ mv .emacs.d _.emacs.d
    $ git clone https://github.com/perforb/gnupack-settings.git .emacs.d
    $ rm -rf _.emacs.d

# Submodule

## yasnippet
See <https://github.com/capitaomorte/yasnippet>

    $ cd ~/.emacs.d/plugins 
    $ git clone https://github.com/capitaomorte/yasnippet

# Add-On

## auto-install

    $ mkdir ~/.emacs.d/elisp
    $ cd ~/.emacs.d/elisp
    $ wget http://www.emacswiki.org/emacs/download/auto-install.el

    M-x byte-compile-file auto-install.el

## ELPA

    $ mkdir ~/.emacs.d/elpa

    (install-elisp "http://bit.ly/pkg-el23")

## auto-async-byte-compile

    (install-elisp-from-emacswiki "auto-async-byte-compile.el")

## anything

    (auto-install-batch "anything")

## anything-c-moccur

    (install-elisp-from-emacswiki "color-moccur.el")
    (install-elisp "http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el")

## anything-for-tags

    (install-elisp-from-emacswiki "anything-gtags.el")
    (install-elisp-from-emacswiki "anything-exuberant-ctags.el")

## redo+

    (install-elisp-from-emacswiki "redo+.el")

## auto-save-buffers

    (install-elisp "http://homepage3.nifty.com/oatu/emacs/archives/auto-save-buffers.el")

## text-adjust-buffer

    (install-elisp "http://taiyaki.org/elisp/mell/src/mell.el")
    (install-elisp "http://taiyaki.org/elisp/text-adjust/src/text-adjust.el")

### Note
     This domain is not currently being answered.

## igrep

    (install-elisp-from-emacswiki "igrep.el")
    (install-elisp-from-emacswiki "grep-edit.el")

## smartchr

    (install-elisp "https://raw.github.com/imakado/emacs-smartchr/master/smartchr.el")

## sequential-command

    (auto-install-batch "sequential-command")

## auto-complete

    ;; company
    (package-install 'company)

    ;; ac-company
    (install-elisp "https://raw.github.com/buzztaiki/auto-complete/master/ac-company.el")

    ;; auto-complete
    (package-install 'auto-complete)

## other ELPA packages

    (package-install 'ctags)
    (package-install 'js2-mode)
    (package-install 'haml-mode)
    (package-install 'php-mode)
    (package-install 'python-mode)
    (package-install 'yaml-mode)

## perl-completion

    (install-elisp "http://www.emacswiki.org/emacs/download/perl-completion.el")

## CakePHP

    (install-elisp "https://raw.github.com/k1LoW/emacs-historyf/master/historyf.el")
    (install-elisp "https://raw.github.com/k1LoW/emacs-cake/master/cake-inflector.el")
    (install-elisp "https://raw.github.com/k1LoW/emacs-cake/master/cake.el")
    (install-elisp "https://raw.github.com/k1LoW/emacs-cake/master/ac-cake.el")
    (install-elisp "https://raw.github.com/k1LoW/emacs-cake2/master/cake2.el")
    (install-elisp "https://raw.github.com/k1LoW/emacs-cake2/master/ac-cake2.el")

## Ruby

    (install-elisp "https://raw.github.com/ruby/ruby/trunk/misc/ruby-electric.el")
    (install-elisp-from-emacswiki "ruby-block.el")
    (install-elisp "https://raw.github.com/ruby/ruby/trunk/misc/inf-ruby.el")

## Markdown

    (install-elisp "http://jblevins.org/projects/markdown-mode/markdown-mode.el")

## multi-term

    (package-install 'multi-term)
