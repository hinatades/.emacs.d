;; Emacs 23より前のバージョンを利用している方は
;; user-emacs-directory変数が未設定のため実の設定を追加

(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacsd/"))

;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest pahts)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-to-load-path)
            (nomarl-top-level-add-subdirs-to-load-path))))))

(require 'package) ; package.elを有効可

;; errorが直らない。。。
;; Wrong type argument: consp, nil 

;; Elscreenのプレフィックスキーを変更する (初期値はC-z)
;;(setq elscreen-prefix-key (kbd "C-t"))
;;(when (require 'elscreen nil t)
;;(elscreen-start)
;; C-z C-z をタイプした時にデフォルトのC-zを利用する
;;(if window-system
;;  (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
;;(define-key elscreen-map (kbd "C-z") 'suspend-emacs)))

;; パッケージリポジトリにMarmaladeとMELPAを追加
(add-to-list
 'package-archives
 '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))
(package-initialize) ; インストール済みのElispを読み込む

;; ------------------------------------------------------------------------
;; auto-install
;; http://www.emacswiki.org/emacs/download/auto-install.el
;; ------------------------------------------------------------------------

;;; auto-installの設定
(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'auto-install)
;;インストールディレクトリを設定する 初期値は ~/.emacs.d/auto-install/
(setq auto-install-directory "~/.emacs.d/elisp/")

;; 起動時にEmacsWikiのページ名を補完候補に加える
;;(auto-install-update-emacswiki-package-name t)
;; ediff関連のバッファを1つのフレームにまとめる
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; install-elisp の関数を利用可能にする
(auto-install-compatibility-setup)

;; Helm
(require 'helm-config)
;; helm-for-fileにキーバインド
(define-key global-map (kbd "C-x C-f") 'helm-for-files)

;; helm-for-fileにキーバインド
(define-key global-map (kbd "C-x C-g") 'helm-for-files)

;; multi-term
(define-key global-map (kbd "C-x C-m") 'multi-term)

;; ターミナル以外はツールバー、スクロールバーを非表示
(when window-system
  ;; tool-barを非表示
  (tool-bar-mode 0)
  ;; scroll-bar を非表示
  (scroll-bar-mode 0 ))

;; C-mにnew-line-and-indentを割り当てる
(global-set-key (kbd "C-m") 'newline-and-indent)

;; C-hを<DEL>(バックスペース)に置き換える
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;;C-tでウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; multi-term

(setenv "LANG" "ja_JP.UTF-8")

;; (setq ansi-term-color-vector                                                
;;       [term                                                                 
;;        term-color-black                                                     
;;        term-color-red                                                       
;;        term-color-green                                                     
;;        term-color-yellow                                                    
;;        term-color-blue                                                      
;;        term-color-magenta                                                   
;;        term-color-cyan                                                      
;;        term-color-white                                                     
;;        term-color-black                                                     
;;        term-color-red                                                       
;;        term-color-green                                                     
;;        term-color-yellow                                                    
;;        term-color-blue                                                      
;;        term-color-magenta                                                   
;;        term-color-cyan                                                      
;;        term-color-white])                              

;; (add-hook 'term-mode-hook
;;           '(lambda ()
;;              (let* ((key-and-func
;;                      `(
;;                        ("\C-p"           previous-line)
;;                        ("\C-n"           "\e[1;9B")
;;                        ("\C-b"           term-send-backward-char)
;;                        ("\C-f"           term-send-forward-char)
;;                        (,(kbd "C-h")     term-send-backspace)
;;                        (,(kbd "C-y")     term-paste)
;;                        (,(kbd "ESC ESC") term-send-raw)
;;                        (,(kbd "C-S-p")   multi-term-prev)
;;                        (,(kbd "C-S-n")   multi-term-next)
;;                        (,(kbd "C-t")     other-window)                       
;;                        利用する場合は
;;                        helm-shell-historyの記事を参照してください
;;                        ("\C-r"           helm-shell-history)
;;                        )))
;;                (loop for (keybind function) in key-and-func do
;;                      (define-key term-raw-map keybind function)))))

(add-hook 'term-mode-hook
  (lambda () 
    (define-key term-raw-map (kbd "C-t") 'other-window)
    (define-key term-raw-map (kbd "C-n") 'next-line)))


;; ファイルサイズを表示
;;(size-indication-mode t)
;; 時計を表示
;; (setq display-time-day-and-date t) ; 曜日・月・日を表示
;; (setq display-time-24hr-format t) ; 24時表示
;; (display-time-mode t)
;; バッテリー残量を表示
(display-battery-mode t)

;; 5.5 インデントの設定
;; TABの表示幅。初期値は8
(setq-default tab-winth 4)
;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)

;; Macの場合
(require 'cask)
;; Linuxの場合
;; (require 'cask "~/.cask/cask.el")

(cask-initialize)

;; ファイルツリーのキーバインド
(global-set-key [f8] 'neotree-toggle)

;; テーマを設定する
;;(load-theme 'arjen-theme t)
;;(set-face-background 'default "#282c34")

(load-theme 'arjen t t)
(enable-theme 'arjen)

;; defalt font color
;; (custom-set-faces
;;   '(default ((t (:foreground "#cccccc" :slant italic)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(package-selected-packages
   (quote
    (helm-gtags smart-newline git-gutter git-gutter+ helm-git-grep ## point-undo rjsx-mode package-utils elscreen helm-c-moccur typescript-mode helm-descbinds helm markdown-mode projectile-rails undo-tree auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "color-233")))))

;;
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
(display-time)

;; 行番号を常に表示させる
(global-linum-mode)
(setq linum-format "%4d ")
;;(set-face-attribute 'linum nil
;;	    :background "#282c34"
;;            :height 0.9)


;; 現在行を目立たせる
(global-hl-line-mode t)

;; auto-complete-config の設定ファイルを読み込む。
(require 'auto-complete-config)

;; よくわからない
(ac-config-default)

;; TABの表示幅。初期値は8
(setq-default tab-width 4)

;; TABキーで自動補完を有効にする
(ac-set-trigger-key "TAB")

;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)

;; auto-complete-mode を起動時に有効にする
(global-auto-complete-mode t)

;; powerlineを設定する
(require 'powerline)

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


;; Ruby on Rails
(require 'projectile)
(projectile-global-mode)
(require 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

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

;; flycheck
(require 'flycheck)
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
(require 'rjsx-mode)
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

;; rjsx-modeでflycheckを有効可
;;(require 'flycheck)
;;(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
;;(add-hook 'rjsx-mode-hook 'flycheck-mode)

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

;; point-undo
(require 'point-undo)
(define-key global-map [f7] 'point-undo)
(define-key global-map [S-f7] 'point-redo)

;;　全行一括インデント
(defun all-indent ()
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning)(region-end))
  (point-undo))
(global-set-key (kbd  "C-x C-]") 'all-indent)

;; helm-git-grep
(require 'helm-git-grep) ;; Not necessary if installed by package.el
(global-set-key (kbd "C-x C-g") 'helm-git-grep)
;; Invoke `helm-git-grep' from isearch.
(define-key isearch-mode-map (kbd "C-x C-g") 'helm-git-grep-from-isearch)
;; Invoke `helm-git-grep' from other helm.
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-x C-g") 'helm-git-grep-from-helm))

;; git-gutter
(require 'git-gutter)
(global-git-gutter-mode t)

;; smart-newline
(require 'smart-newline)
(global-set-key (kbd "C-m") 'smart-newline)
(add-hook 'ruby-mode-hook 'smart-newline-mode)
(add-hook 'emacs-lisp-mode-hook 'smart-newline-mode)
(add-hook 'org-mode-hook 'smart-newline-mode)

(defadvice smart-newline (around C-u activate)
;;  "C-uを押したら元のC-mの挙動をするようにした。org-modeなどで活用。"
  (if (not current-prefix-arg)
      ad-do-it
    (let (current-prefix-arg)
      (let (smart-newline-mode)
        (call-interactively (key-binding (kbd "C-m")))))))

