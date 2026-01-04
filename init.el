;; Basic
(setq inhibit-startup-screen t)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(setq column-number-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq make-backup-files nil)

(setopt use-short-answer t)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(xterm-mouse-mode)
(setq mouse-auto-select-window t)

;; Shortcuts
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)

(global-set-key (kbd "C-x 4") 'transpose-frame)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(defun yf/nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows)
)
(global-set-key (kbd "C-x C-k") 'yf/nuke-all-buffers)

(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)

(defun yf/toggle-relative-lines ()
  (interactive)
  (if (eq display-line-numbers 'relative)
      (setq display-line-numbers t)
    (setq display-line-numbers 'relative)))
(global-set-key (kbd "<f5>") 'yf/toggle-relative-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; for package-install-selected-packages / package-autoremove
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company emmet-mode exec-path-from-shell magit markdown-toc
	     multiple-cursors pinentry prettier-js transpose-frame
	     xclip)))

;; Magit
(require 'use-package)
(use-package magit
  :bind (:map magit-file-section-map
	      ("RET" . magit-diff-visit-file-other-window)
              :map magit-hunk-section-map
              ("RET" . magit-diff-visit-file-other-window)))

;; Pinentry
(setq epg-pinentry-mode 'loopback)
(pinentry-start)

;; ido and fake one too
(require 'ido)
(ido-mode t)
(fido-vertical-mode t)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-x ;") 'mc/edit-lines)
(global-set-key (kbd "C-x f") 'mc/mark-next-word-like-this)

;; eglot
(use-package eglot
  :bind (:map eglot-mode-map
	      ("C-c r" . eglot-rename)
	      ("C-c o" . eglot-code-actions)
	      ("C-c h" . eldoc))
  :custom
  (eglot-ignored-server-capabilities '(:inlayHintProvider)))

;; company-mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; flymake
(use-package flymake
  :bind (:map prog-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;; html
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; markdown
(use-package markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-o" . markdown-toc-follow-link-at-point)))

;; markdown-toc
(require 'dash)
(setq markdown-toc-user-toc-structure-manipulation-fn
      (lambda (toc-structure)
	(-filter (lambda (l)
		   (let ((index (car l)))
		     (<= 1 index)))
		 toc-structure)))

;; C++
(setq c-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVuSansM Nerd Font Mono" :foundry "nil" :slant normal :weight regular :height 120 :width normal)))))
