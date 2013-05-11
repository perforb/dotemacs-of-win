# gnupack settings

This project customizes the setting of Emacs which included in [gnupack][gnupack].

# gnupack's VERSION

11.00

# byte compile of elisp files
M-x byte-recompile-directory

# Installation of git

- 使用するターミナルは gnupack 付属の mintty
- mintty 自体のカスタマイズは、 ~/.minttyrc ではなく %GNUPACK_HOME%/config.ini で設定可能

以下、ターミナル上での具体的な手順。

    # apt-cyg で git をインストールおよび初期設定

    $ apt-cyg install git
    $ git config --global user.name "your_name"
    $ git config --global user.email "your_email"
    $ git config --global color.ui auto

# Ref
<http://blog.roundrop.jp/show/34>

[gnupack]:http://sourceforge.jp/projects/gnupack/
