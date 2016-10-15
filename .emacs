;;; package --- foo

;;; Commentary:

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(custom-enabled-themes (quote (wheatgrass)))
 '(js2-global-externs (list "Footprint" "SC"))
 '(js2-highlight-level 3)
 '(js2-strict-trailing-comma-warning nil)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/"))))
 '(safe-local-variable-values
   (quote
    ((eval setq default-directory
           (locate-dominating-file buffer-file-name ".dir-locals.el")))))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path (expand-file-name "~/.elisp"))
(setq x-select-enable-clipboard t)
(setq-default indent-tabs-mode nil)
(when window-system
  (global-unset-key "\C-x\C-z")
  (global-unset-key "\C-z"))
(desktop-save-mode 1)

(when window-system
  (defun dont-kill-emacs ()
    (interactive)
    (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

  (global-set-key "\C-x\C-c" 'dont-kill-emacs))


(global-auto-revert-mode 1)

;; MELPA is a nice package repository with newer packages like flycheck
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


;; From http://www.emacswiki.org/emacs/Ack
(defvar ack-history nil
  "History for the `ack' command.")

(defun ack (command-args)
  (interactive
   (let ((ack-command "ack-grep --nogroup --with-filename "))
     (list (read-shell-command "Run ack (like this): "
                               ack-command
                               'ack-history))))
  (let ((compilation-disable-input t))
    (compilation-start (concat command-args " < " null-device)
                       'grep-mode)))
;; end ack

;; web-mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))
;; end web-mode

;; js2-mode
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;

;; whitespace stuff
(add-hook 'c-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'python-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'js2-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'js2-mode-hook
          (lambda() (add-to-list 'exec-path (concat (locate-dominating-file buffer-file-name "node_modules") "node_modules/.bin/"))))

;; (set-fontset-font t 'unicode "Symbola" nil 'prepend)
;; (set-fontset-font t 'cyrillic "Droid Sans Mono")

(put 'downcase-region 'disabled nil)


;; begin flycheck - mostly copied from http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
(require 'flycheck)
(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'exec-path (concat dotfiles-dir "node_modules/.bin/"))
;; and this from http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(javascript-jshint)))

;; use eslint with web-mode to get inline JS (also supposedly for jsx..)
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; end flycheck

;; via http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
; already customized above: (setq backup-directory-alist `(("." . "~/.saves")))

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; end backups

;; supposedly this will let you use cask? not sure..
;; (source gnu)
;; (source melpa-stable)

;; end MELPA
(put 'upcase-region 'disabled nil)

;; turn off bell completely
(setq ring-bell-function 'ignore)

;; web-mode seems to be better?
;;(require 'handlebars-mode)

(require 'unicode-fonts)

(provide '.emacs)
;;; .emacs ends here
(require 'magit)
