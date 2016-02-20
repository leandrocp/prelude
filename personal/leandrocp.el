;; shell
(setq shell-file-name "/bin/zsh")
(setq explicit-shell-file-name "/bin/zsh")

;; me
(setq user-full-name "Leandro Cesquini Pereira"
      user-mail-address "leandro.cesquini@gmail.com")

;; company
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

;; display
(menu-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(global-hl-line-mode t)
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

(require 'whitespace)
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

;; magit
(prelude-require-package 'evil-magit)

;; git-gutter
; (prelude-require-package 'diff-hl)
; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
; (add-hook 'prog-mode-hook 'diff-hl-mode)
; (add-hook 'html-mode-hook 'diff-hl-mode)
; (add-hook 'text-mode-hook 'diff-hl-mode)
; (add-hook 'diff-hl-mode-hook 'diff-hl-margin-mode)
; (add-hook 'diff-hl-mode-hook 'diff-hl-flydiff-mode)

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

;; elixir
(defun t-elixir-mode-hook ()
  (yas/minor-mode +1)
  (smartparens-mode +1)
  (tester-init-test-run #'alchemist-mix-test-file "_test.exs$")
  (tester-init-test-suite-run #'alchemist-mix-test))

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

(defun t-erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

(defun t-alchemist-custom-keybindings ()
  (define-key alchemist-mode-map (kbd "M-w") 'alchemist-goto-list-symbol-definitions))

(defun alchemist-my-iex-keys ()
  (define-key alchemist-iex-mode-map (kbd "C-d") 'windmove-right))

(add-hook 'alchemist-iex-mode-hook 'alchemist-my-iex-keys)
(add-hook 'alchemist-mode-hook 't-alchemist-custom-keybindings)
(add-hook 'elixir-mode-hook  't-elixir-mode-hook)
(add-hook 'erlang-mode-hook 't-erlang-mode-hook)

;; Display alchemist buffers always at the bottom
;; Source: http://www.lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
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

(prelude-require-package 'haml-mode)

(prelude-require-package 'ember-mode)
(add-hook 'js-mode-hook (lambda () (ember-mode t)))
(add-hook 'web-mode-hook (lambda () (ember-mode t)))

(prelude-require-package 'angular-mode)
