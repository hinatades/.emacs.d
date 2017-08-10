# .emacs.d

僕のemacsの設定です。基本的な設定は(この記事)[http://qiita.com/bussorenre/items/bbe757ef87e16c3d31ff]を参考にさせていただきました。

### macにemacsをインストール

Homebrewを使ってインストールします。
```
$ brew install emacs
```
公式ページ等からダウンロードせずにこの一行だけに留めると、CUIでのみemacsが使えます。

インストール後、ターミナルを再起動し
```
$ emacs --version
GNU Emacs 25.2.1
Copyright (C) 2017 Free Software Foundation, Inc.
GNU Emacs comes with ABSOLUTELY NO WARRANTY.
You may redistribute copies of GNU Emacs
under the terms of the GNU General Public License.
For more information about these matters, see the file named COPYING.
```

と出たら完了です。

パッケージ管理にはcaskを使っています
```
$ brew install cask
$ cd ~/.emacs.d
$ cask
```
でインストールしてください