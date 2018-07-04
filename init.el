
;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

;;; Code:

;;; Emacs 23より前のバージョンを利用している方は
;;; user-emacs-directory変数が未設定のため以下の設定を追加

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Common Lisp
(require 'cl)

(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacsd/"))


;;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;;; ディレクトリをload-pathに追加
(add-to-load-path "elisp")

;; 読み込んだディレクトリ内のelファイルをロード
;;(require 'col-highlight)
;; 常にハイライト
;; (column-highlight-mode 1)
;; 動作のないときにハイライト(秒数を指定)
;;(toggle-highlight-column-when-idle 1)
;;(col-highlight-set-interval 3)
;; col-highlightの色を変える
;;(custom-set-faces
;; '(col-highlight((t (:background "color-236")))))
;; crosshairs
;;(require 'crosshairs)
;;(crosshairs-mode 1)

(require 'package) ; package.elを有効可


;; ;; Elscreenのプレフィックスキーを変更する (初期値はC-z)
;; (setq elscreen-prefix-key (kbd "C-t"))
;; (when (require 'elscreen nil t)
;; (elscreen-start)
;; ;;C-z C-z をタイプした時にデフォルトのC-zを利用する
;; (if window-system
;;  (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
;; (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))

;; パッケージリポジトリにMarmaladeとMELPAを追加
(add-to-list
 'package-archives
 '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))


;; ;; el-get
;; ;; load-path で ~/.emacs.d とか書かなくてよくなる
;; (when load-file-name
;;   (setq user-emacs-directory (file-name-directory load-file-name)))


;; ;; el-get
;; (add-to-list 'load-path (locate-user-emacs-file "el-get"))
;; (require 'el-get)
;; ;; el-getでダウンロードしたパッケージは ~/.emacs.d/ に入るようにする
;; (setq el-get-dir (locate-user-emacs-file ""))


;; ------------------------------------------------------------------------
;; auto-install
;; http://www.emacswiki.org/emacs/download/auto-install.el
;; ------------------------------------------------------------------------

;;; auto-installの設定
;; (load "~/.emacs.d/elisp/auto-install.el")
;; (add-to-list 'load-path "~/.emacs.d/elisp/")
;;(load-file "~/.emacs.d/elisp/auto-install.el")

(require 'auto-install)

;;インストールディレクトリを設定する 初期値は ~/.emacs.d/auto-install/
;;(setq auto-install-directory "~/.emacs.d/elisp/")

;; 起動時にEmacsWikiのページ名を補完候補に加える
;;(auto-install-update-emacswiki-package-name t)
;; ediff関連のバッファを1つのフレームにまとめる
;;(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; install-elisp の関数を利用可能にする
;;(auto-install-compatibility-setup)

;; Macの場合
(require 'cask)
;; Linuxの場合
;; (require 'cask "~/.cask/cask.el")
(cask-initialize)


;; multi-term
(define-key global-map (kbd "C-x C-m") 'multi-term)
;; ターミナル以外はツールバー、スクロールバーを非表示
(when window-system
  ;; tool-barを非表示
  (tool-bar-mode 0)
  ;; scroll-bar を非表示
  (scroll-bar-mode 0 ))


;; C-hを<DEL>(バックスペース)に置き換える
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))


;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)


;;C-tでウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)


;; multi-term
(setenv "LANG" "ja_JP.UTF-8")
(setq multi-term-program "/usr/local/bin/zsh")
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-t") 'other-window)
            (define-key term-raw-map (kbd "C-n") 'term-send-down)
            (define-key term-raw-map (kbd "C-p") 'term-send-up)))


;; elscreen
;; (elscreen-start)

;; (require 'tabbar)
;; (tabbar-mode)

;; (tabbar-mwheel-mode nil)                  ;; マウスホイール無効
;; (setq tabbar-buffer-groups-function nil)  ;; グループ無効
;; (setq tabbar-use-images nil)              ;; 画像を使わない


;; ;;----- キーに割り当てる
;; (global-set-key (kbd "<C-l>") 'tabbar-forward-tab)
;; (global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)


;; ;;----- 左側のボタンを消す
;; (dolist (btn '(tabbar-buffer-home-button
;;                tabbar-scroll-left-button
;;                tabbar-scroll-right-button))
;;   (set btn (cons (cons "" nil)
;;                  (cons "" nil))))


;; ;;----- タブのセパレーターの長さ
;; (setq tabbar-separator '(2.0))


;; ;;----- タブの色（CUIの時。GUIの時は後でカラーテーマが適用）
;; (set-face-attribute
;;  'tabbar-default nil
;;  :background "brightblue"
;;  :foreground "white"
;;  )
;; (set-face-attribute
;;  'tabbar-selected nil
;;  :background "#ff5f00"
;;  :foreground "brightwhite"
;;  :box nil
;;  )
;; (set-face-attribute
;;  'tabbar-modified nil
;;  :background "brightred"
;;  :foreground "brightwhite"
;;  :box nil
;;  )

;; ;;----- 表示するバッファ
;; (defun my-tabbar-buffer-list ()
;;   (delq nil
;;         (mapcar #'(lambda (b)
;;                     (cond
;;                      ;; Always include the current buffer.
;;                      ((eq (current-buffer) b) b)
;;                      ((buffer-file-name b) b)
;;                      ((char-equal ?\  (aref (buffer-name b) 0)) nil)
;;                      ((equal "*scratch*" (buffer-name b)) b) ; *scratch*バッファは表示する
;;                      ((char-equal ?* (aref (buffer-name b) 0)) nil) ; それ以外の * で始まるバッファは表示しない
;;                      ((buffer-live-p b) b)))
;;                 (buffer-list))))
;; (setq tabbar-buffer-list-function 'my-tabbar-buffer-list)


;; TABの表示幅。初期値は8
(setq-default tab-winth 4)


;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)


;; テーマを設定する
;;(load-theme 'arjen-theme t)
;;(set-face-background 'default "#282c34")
;;(load-theme 'arjen t t)
;; (enable-theme 'arjen)
;; atom-one-dark
(require 'atom-one-dark-bg-black-theme)
(load-theme 'atom-one-dark-bg-black t)
;; 背景色を設定します。
;;(add-to-list 'default-frame-alist '(background-color . "black"))
;;(load-theme 'atom-one-dark t)

;; defalt font color
;; (custom-set-faces
;;   '(default ((t (:foreground "#cccccc" :slant italic)))))

;; 選択中のリージョンの色
(set-face-background 'region "#5f00d7")


;; ファイルツリーのキーバインド
(global-set-key [f8] 'neotree-toggle)


;; Auto Complete
(when (require 'auto-complete-config nil t)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-ignore-case nil))
(set-language-environment 'Japanese)    ; 日本語環境
(set-default-coding-systems 'utf-8-unix)    ; UTF-8 が基本
(set-terminal-coding-system 'utf-8-unix)    ; emacs -nw も文字化けしない
(setq default-file-name-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(prefer-coding-system 'utf-8-unix)


;; ロックファイル / バックアップファイルを作成しない
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq delete-auto-save-files t)


;; 対応するカッコを強調表示
(show-paren-mode t)


;; 時間も表示させる。
;;(display-time)


;; 行番号を常に表示させる
(global-linum-mode)
(setq linum-format "%4d ")
;;(set-face-attribute 'linum nil
;;	    :background "#282c34"
;;            :height 0.9)


;; 現在行を目立たせる
(global-hl-line-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "color-234")))))

;; auto-complete-config の設定ファイルを読み込む。
(require 'auto-complete-config)


;; よくわからない
(ac-config-default)


;; TABキーで自動補完を有効にする
(ac-set-trigger-key "TAB")


;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)


;; auto-complete-mode を起動時に有効にする
(global-auto-complete-mode t)


;; powerlineを設定する
(defun powerline-my-theme ()
  "Setup the my mode-line."
  (interactive)
  (setq powerline-current-separator 'utf-8)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'mode-line-1-fg 'mode-line-2-fg))
                          (face2 (if active 'mode-line-1-arrow 'mode-line-2-arrow))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (lhs (list (powerline-raw " " face1)
                                     (powerline-major-mode face1)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-buffer-id nil )
                                     (powerline-raw " [ ")
                                     (powerline-raw mode-line-mule-info nil)
                                     (powerline-raw "%*")
                                     (powerline-raw " |")
                                     (powerline-process nil)
                                     (powerline-vc)
                                     (powerline-raw " ]")
                                     ))
                          (rhs (list (powerline-raw "%4l")
                                     (powerline-raw ":")
                                     (powerline-raw "%2c")
                                     (powerline-raw " | ")
                                     (powerline-raw "%6p")
                                     (powerline-raw " ")
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill nil (powerline-width rhs))
                             (powerline-render rhs)))))))

(defun make/set-face (face-name fg-color bg-color weight)
  (make-face face-name)
  (set-face-attribute face-name nil
                      :foreground fg-color :background bg-color :box nil :weight weight))
(make/set-face 'mode-line-1-fg "#282C34" "#EF8300" 'bold)
(make/set-face 'mode-line-2-fg "#AAAAAA" "#2F343D" 'bold)
(make/set-face 'mode-line-1-arrow  "#AAAAAA" "#3E4451" 'bold)
(make/set-face 'mode-line-2-arrow  "#AAAAAA" "#3E4451" 'bold)
(powerline-my-theme)


;; web-mode
;; .erb の色付け
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;; magit
(global-set-key (kbd "C-x g") 'magit-status)
;; 色変更
;; (set-face-foreground 'magit-diff-add "#b9ca4a") ; 追加した部分を緑に
;; (set-face-foreground 'magit-diff-del "#d54e53")  ; 削除した 部分を赤に
;; (set-face-background 'magit-item-highlight "#000000") ; 選択項目ハイライトがうっとうしいので背景色と同化


;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))))

(setq js2-include-browser-externs nil)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-highlight-external-variables nil)
(setq js2-include-jslint-globals nil)


;; rjsx-mode
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("containers\\/.*\\.js\\'" . rjsx-mode))

(add-hook 'rjsx-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ;;インデントはタブではなくスペース
            (setq js-indent-level 2) ;;スペースは２つ、デフォルトは4
            ;; (setq js2-strict-missing-semi-warning nil) ;;行末のセミコロンの警告はオフ
            ))
;; rjsx-mode時にauto-completeを有効可
(add-hook 'rjsx-mode-hook '(lambda ()
                             (require 'auto-complete)
                             (auto-complete-mode t)
                             ))
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'rjsx-mode)


;;rjsx-modeでflycheckを有効可
(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
(add-hook 'rjsx-mode-hook 'flycheck-mode)


;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))


;; typescript-mode
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))


;; htmlタグの自動補完をオン
(setq web-mode-auto-close-style 1)
(setq web-mode-tag-auto-close-style t)


;; <% %> を自動で閉じる
(setq web-mode-enable-auto-pairing t)


;; カッコを自動で閉じる
(electric-pair-mode 1)


;; バックアップの無効化
(setq make-backup-files nil)
(setq auto-save-default nil)


;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacsd/backups/") )))

;; undo-treeの設定
(when (require 'undo-tree nil t)
  ;;C-'にredoを割り当てる)
  ;; (define-key global-map (kbd "C-'") 'undo-tree-redo)
  (global-undo-tree-mode))


(require 'point-undo)

;; 全行一括インデント
(defun all-indent ()
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning)(region-end))
  (point-undo))
(global-set-key (kbd  "C-x C-]") 'all-indent)


;; ------------------------------------------------------------------------
;; Helm
;; https://github.com/emacs-helm/helm
;; ------------------------------------------------------------------------
(helm-mode 1)
;; helm-buffers-list
(define-key global-map (kbd "C-x C-b")   'helm-buffers-list)

;; helm-find-fileにキーバインド
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)


;; helm-for-fileにキーバインド
;;(define-key global-map (kbd "C-x C-f") 'helm-for-files)

;; helm-swoop
;; ファイル内文字列検索
(global-set-key (kbd "C-s") 'helm-swoop)

;; helm-git-grep
;; プロジェクト内文字列検索
(require 'helm-git-grep) ;; Not necessary if installed by package.el
(global-set-key (kbd "C-x C-g") 'helm-git-grep)
;; Invoke `helm-git-grep' from isearch.
(define-key isearch-mode-map (kbd "C-x C-g") 'helm-git-grep-from-isearch)
;; Invoke `helm-git-grep' from other helm.
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-x C-g") 'helm-git-grep-from-helm))


;; buffer
(define-key global-map (kbd "C-x b") 'list-buffers)


;; git-gutter
(global-git-gutter-mode t)

;; smart-newline
(global-set-key (kbd "C-m") 'smart-newline)
(add-hook 'ruby-mode-hook 'smart-newline-mode)
(add-hook 'emacs-lisp-mode-hook 'smart-newline-mode)
(add-hook 'org-mode-hook 'smart-newline-mode)


;; highlight-symbol
;;; 1秒後自動ハイライトされるようになる
(setq highlight-symbol-idle-delay 0.5)
;;; 自動ハイライトをしたいならば
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
;;; ソースコードにおいてM-p/M-nでシンボル間を移動
(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
;;; シンボル置換
(global-set-key (kbd "M-s M-r") 'highlight-symbol-query-replace)
;; 色の設定
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))


;; whitespace-mode
(global-whitespace-mode 1)
;; スペースの定義は全角スペースとする。
(setq whitespace-space-regexp "\x3000+")
;; 改行の色を変更
(set-face-foreground 'whitespace-newline "gray40")
;; 半角スペースと改行を除外
(dolist (d '((space-mark ?\ ) (newline-mark ?\n)))
  (setq whitespace-display-mappings
        (delete-if
         '(lambda (e) (and (eq (car d) (car e))
                           (eq (cadr d) (cadr e))))
         whitespace-display-mappings)))
;; 全角スペースと改行を追加
(dolist (e '((space-mark   ?\x3000 [?\□])
             ;;(newline-mark ?\n     [?\u21B5 ?\n] [?$ ?\n])
             ))
  (add-to-list 'whitespace-display-mappings e))
;; 強調したくない要素を削除
(dolist (d '(face lines space-before-tab
                  indentation empty space-after-tab tab-mark))
  (setq whitespace-style (delq d whitespace-style)))


;; rainbow-delimiters を使うための設定
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; 括弧の色を強調する設定
(require 'cl-lib)
(require 'color)
(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)

;; python-mode
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "\C-m") 'newline-and-indent)
            (define-key python-mode-map (kbd "RET") 'newline-and-indent)))


;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; dockerfile-mode
(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; lua-mode
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(package-selected-packages
   (quote
    (lua-mode dockerfile-mode yaml-mode tabbar elscreen-multi-term package-utils elscreen go-mode yasnippet web-mode use-package undo-tree typescript-mode swap-buffers smex smartparens smart-newline rjsx-mode rainbow-delimiters projectile prodigy powerline popwin pallet nyan-mode neotree multiple-cursors markdown-mode magit idle-highlight-mode htmlize highlight-symbol helm-swoop helm-git-grep helm-descbinds git-gutter git-gutter+ flycheck-pos-tip flycheck-cask expand-region exec-path-from-shell drag-stuff atom-one-dark-theme))))
