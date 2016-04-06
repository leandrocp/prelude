;;; leandrocp.el --- custom emacs config
;;; Commentary:
;;; Code:

;; shell
(setq shell-file-name "/bin/zsh"
  explicit-shell-file-name "/bin/zsh")

;; me
(setq user-full-name "Leandro Cesquini Pereira"
  user-mail-address "leandro.cesquini@gmail.com")

;; copy & paste
(prelude-require-package 'pbcopy)
(turn-on-pbcopy)

;; company & snippets
(prelude-require-packages '(yasnippet elixir-yasnippets ember-yasnippets angular-snippets))
(yas-global-mode 1)
(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
(setq company-transformers '(company-sort-by-occurrence))
(setq company-show-numbers t)
(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "C-k") 'company-select-previous)

;; projectile
(add-to-list 'projectile-globally-ignored-directories "node_modules")
(add-to-list 'projectile-globally-ignored-directories "bower_components")
(add-to-list 'projectile-globally-ignored-directories "tmp")
(add-to-list 'projectile-globally-ignored-directories "dist")
(add-to-list 'projectile-globally-ignored-directories "build")
(add-to-list 'projectile-globally-ignored-directories ".git")

;; display
(menu-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(global-hl-line-mode t)
(set-default 'truncate-lines t)
(setq linum-format " %3d ")
(mapc
 (lambda (mode-hook)
   (add-hook mode-hook 'linum-mode))
 '(text-mode-hook
   prog-mode-hook
   conf-mode-hook
   css-mode-hook))
(setq default-cursor-type 'box)
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

(require 'saveplace)
(setq-default save-place t)
(if (fboundp #'save-place-mode)
  (save-place-mode +1)
  (setq-default save-place t))

(require 'whitespace)
(setq whitespace-line-column 120)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(defun set-my-margins ()
  (interactive)
  (setq left-margin-width 1)
  (setq left-fringe-width 5)
  (setq right-fringe-width 10))
(add-hook 'text-mode-hook 'set-my-margins)
(add-hook 'prog-mode-hook 'set-my-margins)

;; theme
(prelude-require-package 'base16-theme)
(load-theme 'base16-eighties-dark t)

;; window
(prelude-require-packages '(windmove switch-window))
(global-set-key (kbd "C-x -") 'split-window-below)
(global-set-key (kbd "C-x \\") 'split-window-right)
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x h") 'windmove-left)
(global-set-key (kbd "C-x l") 'windmove-right)
(global-set-key (kbd "C-x j") 'windmove-down)
(global-set-key (kbd "C-x k") 'windmove-up)

;; buffer
(global-set-key (kbd "M-q") 'kill-this-buffer)
(global-set-key (kbd "M-l") 'prelude-switch-to-previous-buffer)

;; imenu
(global-set-key (kbd "M-b") 'helm-imenu-anywhere)
(global-set-key (kbd "M-r") 'helm-resume)

;; tabbar
(prelude-require-package 'tabbar)
(setq tabbar-background-color nil)
(setq tabbar-separator (quote (1.0)))
(setq tool-bar-mode nil)
(define-key evil-normal-state-map "L" 'tabbar-forward-tab)
(define-key evil-normal-state-map "H" 'tabbar-backward-tab)
(setq tabbar-background-color "#393939")
(custom-set-faces
 '(tabbar-default ((t (:inherit variable-pitch :background "#393939" :foreground "black" :weight bold))))
 '(tabbar-button ((t (:inherit tabbar-default :foreground "#f2f0ec"))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default))))
 '(tabbar-highlight ((t (:underline t))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "#393939" :foreground "#ffcc66"))))
 '(tabbar-separator ((t (:inherit tabbar-default :background "#393939"))))
 '(tabbar-unselected ((t (:inherit tabbar-default)))))
(tabbar-mode 1)

;; helm
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-j") 'helm-next-line)
(define-key helm-find-files-map (kbd "C-k") 'helm-previous-line)

;; neotree
(prelude-require-package 'neotree)
(global-set-key (kbd "M-t") 'neotree-find)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "M-c") 'neotree-create-node)
            (define-key evil-normal-state-local-map (kbd "M-r") 'neotree-rename-node)
            (define-key evil-normal-state-local-map (kbd "M-d") 'neotree-delete-node)))
(setq neo-smart-open t)
(setq neo-window-position 'right)
(setq neo-window-width 40)

;; which-key
(which-key-setup-side-window-right)

;; magit
(prelude-require-package 'evil-magit)

;; editorconfig
(prelude-require-package 'editorconfig)
(editorconfig-mode 1)

;; commenter
(prelude-require-package 'evil-nerd-commenter)
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

;; expand-region
(prelude-require-package 'expand-region)
(global-set-key (kbd "M-e") 'er/expand-region)

;; drag-stuff
(prelude-require-package 'drag-stuff)
(add-hook 'prog-mode-hook (lambda () (drag-stuff-mode t)))
(global-set-key (kbd "M-k") 'drag-stuff-up)
(global-set-key (kbd "M-j") 'drag-stuff-down)

;; beautify code
(prelude-require-package 'web-beautify)
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;; dash
(prelude-require-package 'dash-at-point)
(global-set-key (kbd "M-d") 'dash-at-point)

;; web
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

;; elixir
(prelude-require-package 'alchemist)
(setq alchemist-goto-elixir-source-dir "~/code/github/elixir/elixir")

(defun my-elixir-do-end-close-action (id action context)
  (when (eq action 'insert)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(sp-with-modes '(elixir-mode)
  (sp-local-pair "->" "end"
                 :when '(("RET"))
                 :post-handlers '(:add my-elixir-do-end-close-action)
                 :actions '(insert)))

(sp-with-modes '(elixir-mode)
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(:add my-elixir-do-end-close-action)
                 :actions '(insert)))

(add-to-list 'display-buffer-alist
             `(,(rx bos (or "*alchemist test report*"
                            "*alchemist mix*"
                            "*alchemist help*"
                            "*alchemist elixir*"
                            "*alchemist elixirc*"))
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . right)
               (window-width   . 0.5)))

(defun t-alchemist-custom-keybindings ()
  (define-key alchemist-mode-map (kbd "M-]") 'alchemist-goto-definition-at-point)
  (define-key alchemist-mode-map (kbd "M-[") 'alchemist-goto-jump-back)
  (define-key alchemist-mode-map (kbd "M-/") 'alchemist-goto-list-symbol-definitions))

(add-hook 'elixir-mode-hook (lambda () (alchemist-mode)))
(add-hook 'alchemist-mode-hook 't-alchemist-custom-keybindings)

; Web
(prelude-require-package 'haml-mode)
(prelude-require-packages '(ember-mode handlebars-mode))
(add-hook 'js-mode-hook (lambda () (ember-mode)))
(add-hook 'web-mode-hook (lambda () (ember-mode)))
(prelude-require-package 'angular-mode)

;; modeline
(diminish 'company-mode)
(diminish 'helm-mode)
(diminish 'guru-mode)
(diminish 'drag-stuff-mode)
(diminish 'yas-global-mode)
(diminish 'yas-minor-mode)
(diminish 'whitespace-mode)
(diminish 'smartparens-mode)

;; indent
(add-hook 'prog-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))
(prelude-require-package 'aggressive-indent)
(global-aggressive-indent-mode 1)
(add-to-list
 'aggressive-indent-dont-indent-if
 '(and (derived-mode-p 'elixir-mode)
       (string-match "\\b\\#\\b"
		     (thing-at-point 'line))))
