(package-initialize)

;; Macの場合
(require 'cask)
;; Linuxの場合
;; (require 'cask "~/.cask/cask.el")

(cask-initialize)


;; ファイルツリーのキーバインド
(global-set-key [f8] 'neotree-toggle)


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)


;;C-hをbackspaceに
(keyboard-translate ?\C-h ?\C-?)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (undo-tree auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
;; Auto Complete

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

;; 現在行を目立たせる
(setq hl-line-face 'underline)
(global-hl-line-mode)

;; テーマを設定する
(load-theme 'manoj-dark t)


;;
;; auto-complete-config の設定ファイルを読み込む。
(require 'auto-complete-config)

;; よくわからない
(ac-config-default)

;; TABキーで自動補完を有効にする
(ac-set-trigger-key "TAB")

;; auto-complete-mode を起動時に有効にする
(global-auto-complete-mode t)

;; 
;; undo-tree
;;
;; undo-tree を読み込む
(require 'undo-tree)

;; undo-tree を起動時に有効にする
(global-undo-tree-mode t)

;; M-/ をredo に設定する。
(global-set-key (kbd "M-/") 'undo-tree-redo)


;;
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))


(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))
(define-key global-map [?\s-¥] [?\\])
(define-key key-translation-map [?\s-¥] [?¥])
(define-key key-translation-map [?¥] [?\s-¥])

