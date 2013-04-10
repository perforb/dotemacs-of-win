# gnupack settings

This project customizes the setting of Emacs which included in gnupack.

See <http://sourceforge.jp/projects/gnupack/>

# gnupack's VERSION

11.00

# byte compile
M-x byte-recompile-directory

# Installation of git for mintty

git は gnupack 付属の mintty からインストールおよび利用します。なお、gnupack のインストール先は `c:\` であるものとします。
また、mintty 自体のカスタマイズは ~/.minttyrc ではなく c:/gnupack_basic-11.00/config.ini で設定可能です。
ちなみに eshell 上から git を利用してみたところ、.ssh/config の permission が 600 とならないので push のみできませんでした。

以下、ターミナル上での具体的な手順。

    # プロンプトの表示をシンプルにするために .bashrc の custom_prompt_command を以下のように変更

    function custom_prompt_command {
        typeset _Retv=$?
        typeset _PromptColor=""
        if [[ ${_Retv} -eq 0 ]] ; then
            _PromptColor=$BASH_PROMPT_OK
        else 
            _PromptColor=$BASH_PROMPT_NG
        fi
        export PS1="\[${_PromptColor}\] \w# \[\e[0m\]"
    }

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

    github に `id_rsa.pub` の内容を登録する。

    $ vi ~/.ssh/config

        Host github.com
        User your_name
        Port 22
        Hostname github.com
        IdentityFile ~/.ssh/id_rsa
        TCPKeepAlive yes
        IdentitiesOnly yes
