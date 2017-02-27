;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "~/.emacs.d/.cask/25.1/elpa/cask-20161024.1205/cask.el")
(cask-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; setting company
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る

(add-to-list 'custom-theme-load-path "~/.emacs.d/atom-one-dark-theme/")
(global-set-key [f8] 'neotree-toggle)

;; theme
(load-theme 'atom-one-dark t)
;;(load-theme 'manoj-dark t)

;;; 現在行に色をつける
(setq hl-line-face 'underline) ; 下線
(global-hl-line-mode)

;;(custom-set-faceshl-line ((t (:bakground "color-236"))))

;;; 履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;;; ファイル内のカーソル位置を保存する
(setq-default save-place t)

;;; 対応する括弧を表示させる
(show-paren-mode 1)

;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;; バックアップファイルを作成させない
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;; タブにスペースを使用する
(setq-default tab-width 4 indent-tabs-mode nil)

;; 改行コードを表示する
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; 複数ウィンドウを禁止する
;;(setq ns-pop-up-frames nil)

;; ウィンドウを透明にする
;; アクティブウィンドウ／非アクティブウィンドウ（alphaの値で透明度を指定）
(add-to-list 'default-frame-alist '(alpha . (0.93 0.93)))

;; メニューバーを消す
(menu-bar-mode -1)

;; ツールバーを消す
(tool-bar-mode -1)

;;スクロールバーを消す
(scroll-bar-mode 0)

;; 列数を表示する
(column-number-mode t)

;; 行数を表示する
(global-linum-mode t)

;; カーソルの点滅をやめる
;;(blink-cursor-mode 0)

;; カーソル行をハイライトする
;;(global-hl-line-mode t)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; ウィンドウ内に収まらないときだけ、カッコ内も光らせる
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "grey")
(set-face-foreground 'show-paren-match-face "black")

;; スペース、タブなどを可視化する
;;(global-whitespace-mode 1)

;; スクロールは１行ごとに
(setq scroll-conservatively 1)

;; シフト＋矢印で範囲選択
;;(setq pc-select-selection-keys-only t)
;;(pc-selection-mode 1)

;; C-kで行全体を削除する
(setq kill-whole-line t)

;;; dired設定
(require 'dired-x)

;;"yes or no" の選択を "y or n" にする
(fset 'yes-or-no-p 'y-or-n-p)

;; beep音を消す
(defun my-bell-function ()
  (unless (memq this-command
        '(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;;Ricityフォント
(set-frame-font "ricty-12")

;;; モードラインに時刻を表示する
(display-time)

;; 起動時のウィンドウサイズ、色などを設定
;;(if (boundp 'window-system)
;;    (setq default-frame-alist
;;          (append (list
;;                   '(foreground-color . "black")  ; 文字色
;;                 '(background-color . "white")  ; 背景色
;;               '(border-color     . "black")  ; ボーダー色
;;                   '(mouse-color      . "black")  ; マウスカーソルの色
;;                   '(cursor-color     . "black")  ; カーソルの色
;;                   '(cursor-type      . box)      ; カーソルの形状
;;                   '(top . 0) ; ウィンドウの表示位置（Y座標）
;;                   '(left . 200) ; ウィンドウの表示位置（X座標）
;;                   '(width . 120) ; ウィンドウの幅（文字数）
;;                   '(height . 53) ; ウィンドウの高さ（文字数）
;;                   )
;;                  default-frame-alist)))
;;(setq initial-frame-alist default-frame-alist )

;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)

;;Markdown preview
(setq markdown-command "multimarkdown")

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
