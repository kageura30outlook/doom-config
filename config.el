;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; (add-to-list 'load-path (expand-file-name "lisp" doom-user-dir))
(setq evil-respect-visual-line-mode t)

;; Load comprehensive org-persist blocking solution
;; IMPORTANT: Disable org-persist completely
;; Note: Additional blocking is also done in early-init.el
(setq org-element-cache-persistent nil)
(setq org-persist-directory nil)
(setq org-persist-default-directory nil)

;; Block any attempts to load org-persist after startup
(when (not (featurep 'org-persist))
  ;; Override org-persist functions to prevent any persistence
  (defun org-persist-write (&rest _) nil)
  (defun org-persist-read (&rest _) nil)
  (defun org-persist-load (&rest _) nil)
  (defun org-persist-register (&rest _) nil)
  (defun org-persist-unregister (&rest _) nil)
  (defun org-persist-gc (&rest _) nil)
  (defun org-persist-clear-storage (&rest _) nil)
  
  ;; Mark as provided to prevent loading
  (provide 'org-persist))

;; Additional safety for late-loading scenarios
(with-eval-after-load 'org
  (setq org-element-cache-persistent nil)
  (setq org-persist-directory nil))

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-monokai-ristretto)
;; 設定を適用するためにテーマを再読み込み
(when doom-theme
  (load-theme doom-theme t))



;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(use-package doom-themes
    :custom
    (doom-themes-enable-italic t)
    (doom-themes-enable-bold t)
    :custom-face
    (doom-modeline-bar ((t (:background "#6272a4"))))
    :config
    (doom-themes-neotree-config)
    (doom-themes-org-config))

;; テーマ設定（例: doom-dracula おしゃれで人気）

;; モデルライン設定
(use-package! doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t))

;; 不要なモードラインを隠す
(use-package! hide-mode-line
  :hook (prog-mode . hide-mode-line-mode))

;; ミニマップ表示
(use-package! minimap
  :commands (minimap-mode)
  :init
  (setq minimap-window-location 'right
        minimap-width-fraction 0.15))

;; Ivy/Swiper/Counselによる検索・補完UIの強化
(use-package! ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t))

(use-package! counsel
  :after ivy
  :config (counsel-mode 1))

(use-package! ivy-rich
  :after ivy
  :config (ivy-rich-mode 1))

;; キーバインド表示補助 which-key
(use-package! which-key
  :defer 0
  :diminish which-key-mode
  :config (which-key-mode))

;; Hydraで便利なプレフィックスメニュー
(use-package! hydra
  :commands hydra)

;; ファイルツリー表示 neotree or treemacs
(use-package! neotree
  :bind ([f8] . neotree-toggle)
  :config (setq neo-smart-open t))

;; 括弧の色分け rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; インデントガイド表示
(use-package! highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))

;; カーソル位置を光らせる beacon
(use-package! beacon
  :config (beacon-mode 1))

(use-package! auto-dim-other-buffers
  :hook (after-init . auto-dim-other-buffers-mode)
  :config
  (setq auto-dim-other-buffers-fraction 0.15))

;; Golden Ratioでアクティブウィンドウを拡大
(use-package! golden-ratio
  :hook (after-init . golden-ratio-mode))

;; カーソルはアクティブウィンドウのみに表示
(setq-default cursor-in-non-selected-windows nil)

;; doom-modelineの色差設定例（必要に応じてカスタム）
(custom-set-faces!
 '(mode-line ((t (:background "#44475a" :foreground "#f8f8f2" :box nil))))
 '(mode-line-inactive ((t (:background "#282a36" :foreground "#6272a4" :box nil)))))
;; カスタムフォント例（環境に合わせて変更してください）

;; [[file:config.org::*Color Coding][Color Coding:1]]
;;; Color Coding — Doom Monokai Ristretto for Python

;; -------------------------
;; Cursor Colors per Evil Mode
;; -------------------------
(after! evil
  (setq evil-normal-state-cursor `(,(doom-color 'orange) box)        ;; Normal mode: pink/red box
        evil-insert-state-cursor `(,(doom-color 'orange) bar)      ;; Insert mode: green bar
        evil-visual-state-cursor `(,(doom-color 'green) hbar))) ;; Visual mode: purple horizontal
;; -------------------------
;; Python Syntax Highlighting Overrides
;; -------------------------
(custom-set-faces!
  ;; Keywords: def, class, return
  '(font-lock-keyword-face :foreground "#ff6188" :weight bold)   ;; doom-color 'red

  ;; Function names
  '(font-lock-function-name-face :foreground "#a9dc76")          ;; doom-color 'green

  ;; Strings
  '(font-lock-string-face :foreground "#ffd866")                 ;; doom-color 'yellow

  ;; Builtins like print(), len()
  '(font-lock-builtin-face :foreground "#ab9df2")                 ;; doom-color 'magenta

  ;; Comments
  '(font-lock-comment-face :foreground "#727072" :slant italic))

;; -------------------------
;; Tree-sitter for Rich Syntax Highlighting
;; -------------------------
(use-package! tree-sitter
  :config
  (global-tree-sitter-mode))

(use-package! tree-sitter-langs
  :after tree-sitter)
;; Color Coding:1 ends here

;; Load and configure MCP package if available locally
(let* ((mcp-dir (expand-file-name "~/.doom.d/"))
       (mcp-file (expand-file-name "mcp.el" mcp-dir)))
  (when (file-exists-p mcp-file)
    (add-to-list 'load-path mcp-dir)
    (use-package! mcp
      :after gptel
      :custom
      (mcp-hub-servers
       '(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/Users/Kageura/Documents/")))
         ("fetch" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-fetch")))
         ("memory" . (:command "npx" :args ("-y" "@pulsemcp/basic-memory")))
         ("sequencethink" . (:command "npx" :args ("-y" "@arben-adm/mcp-sequential-thinking")))
         ("git" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-github")))
         ("python-sdk" . (:command "python3" :args ("-m" "mcp.server.fastmcp" "--spec" "python-sdk")))
         ("puppeteer" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-puppeteer")))
         ("emacs" . (:command "bash" :args ("-c" "~/.config/doom/bin/doomscript ~/.config/doom/bin/emacs-mcp")))))
      :config
      ;; Load the MCP hub component only if present, then start servers
      (when (require 'mcp-hub nil 'noerror)
        (add-hook 'after-init-hook #'mcp-hub-start-all-server)))))

(setq mac-command-modifier      'super
      ns-command-modifier       'super
      mac-option-modifier       'meta
      ns-option-modifier        'meta
      mac-left-option-modifier 'meta)

;;(setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 15)
;;      doom-variable-pitch-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 15)
 ;;     doom-big-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 24))
;;(after! doom-themes
  ;(setq doom-themes-enable-bold t
  ;      doom-themes-enable-italic t))
;(custom-set-faces!
 ;'(font-lock-comment-face :slant italic)
;; '(font-lock-keyword-face :slant italic))
 
;Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; プラットフォーム依存のフォント設定
(cond
 ((eq system-type 'darwin)  ; macOS
  (let ((device-name (shell-command-to-string "sysctl -n hw.model")))
    (cond
     ((string-match-p "Mac15,12" device-name)
      (setq doom-font (font-spec :family "Monaspace Argon" :size 12)))
     (t
      (setq doom-font (font-spec :family "Monaspace Argon" :size 14))))))
 ((eq system-type 'gnu/linux)  ; Linux
  (setq doom-font (font-spec :family "Source Code Pro" :size 14)))
 (t  ; その他のシステム
  (setq doom-font (font-spec :family "monospace" :size 14))))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; Japanese and symbol font support (プラットフォーム対応)
(setq doom-symbol-font 
  (cond
   ((eq system-type 'darwin) (font-spec :family "Noto Sans JP"))
   ((eq system-type 'gnu/linux) (font-spec :family "Noto Sans CJK JP"))
   (t (font-spec :family "sans-serif"))))

(setq display-line-numbers-type t)  ;; Absolute line numbers
(map! :leader
       :desc "Toggle truncate lines"
        "t t" #'toggle-truncate-lines)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Source block fontification
(setq org-src-fontify-natively t)

(after! org
  (setq org-startup-folded 'show2levels)

  (defun my/unfold-toc-section ()
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\*+ Table of Contents" nil t)
          (org-show-subtree)))))

  (add-hook 'org-mode-hook #'my/unfold-toc-section))

;; tex settings
(setq texprogram 'dvipng)

(after! org
  (setq org-html-head-include-scripts t
        ;; xxelatex1
        org-latex-pdf-process
        '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "biber %b"
          "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f")
        ;; org-latex-pdf-process (list "latexmk -shell-escape -f -lualatex %f")
        org-preview-latex-default-process 'imagexetex
        org-export-with-toc t
        org-export-headline-levels 4
        org-pandoc-options '((standalone . t) (self-contained . t))
        org-latex-create-formula-image-program texprogram
        org-export-with-author t
        org-export-headline-levels 1
        org-export-with-drawers nil
        org-export-with-email t
        org-export-with-footnotes t
        org-export-with-sub-superscripts nil
        org-export-with-latex t
        org-export-with-properties nil
        org-export-with-smart-quotes t))
(after! org (add-to-list 'org-latex-packages-alist '("" "mathrsfs" t)))

;; プラットフォーム依存のbibliographyパス
(setq! bibtex-completion-bibliography
  (list (expand-file-name "bibliography.bib" "~")))
(setq! citar-bibliography
  (list (expand-file-name "bibliography.bib" "~")))

(use-package! org
  :config
  ;; A. 基本設定 (ファイルパス、TODOキーワード、タグ、アーカイブ)
  ;; -----------------------------------------------------------------
  (setq org-agenda-files '("~/org/agenda" "~/org-roam"))

  ;; Agendaビューをカレントウィンドウで開く
  (setq org-agenda-window-setup 'current-window)

  ;; SOMEDAY: いつかやる / WAIT: 誰かの返事待ちなど
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "|" "DONE(d)")
          (sequence "SOMEDAY(s)" "|" "CANCELLED(c)")))

  (setq org-tag-persistent-alist
        '(("@work" . ?w) ("@home" . ?h) ("@errand" . ?e)
          ("@must" . ?m) ("@should" . ?s) ("@want" . ?t)))

  (setq org-archive-location "~/org/agenda/archive/%s_archive::")

  (after! org-capture
    (setq org-capture-templates
          '(("t" "Task to Inbox" entry
             (file+headline "~/org/agenda/inbox.org" "Tasks")
             "* TODO %?")

            ("p" "Project Task" entry
             (file+headline "~/org/agenda/gtd.org" "Projects")
             "* TODO %? :@work:\nSCHEDULED: %(org-insert-time-stamp (current-time) t)\n")

            ("r" "Routine Task" entry
             (file+headline "~/org/agenda/routines.org" "Routines")
             "* TODO %? \nSCHEDULED: <> \n:PROPERTIES:\n:STYLE: habit\n:END:")

            ("s" "Someday/Maybe" entry
             (file+headline "~/org/agenda/someday.org" "Ideas")
             "* SOMEDAY %?\n")
            )))

  ;; 日付フォーマットと現在日時の設定
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-start-day nil)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)

  (setq org-agenda-custom-commands
        '(("d" "⚡ Daily Dashboard"
           ((tags-todo "+DEADLINE<=\"<today>\"|+SCHEDULED<=\"<today>\""
                       ((org-agenda-overriding-header "🎯 Today's Focus Tasks")))))

          ("w" "🔍 Weekly Review"
           ((agenda "" ((org-agenda-span 'week)))
            (tags-todo "/DONE"
                       ((org-agenda-overriding-header "Inbox (to be processed)")
                        (org-agenda-files '("~/org/agenda/inbox.org"))))
            (tags-todo "+DEADLINE>=\"<today>\"+DEADLINE<=\"<+1w>\""
                       ((org-agenda-overriding-header "🔥 Deadlines This Week")))
            (tags "project"
                  ((org-agenda-overriding-header "Project Status")))))

          ("s" "💡 Someday / Maybe"
           ((todo "SOMEDAY" ; <- SOMEDAYキーワードのタスクを全ファイルから探す
                       ((org-agenda-overriding-header "On Hold Tasks (by Keyword)")))
            (tags-todo "/DONE" ; <- someday.orgの中のタスクを表示する
                       ((org-agenda-overriding-header "Idea List (in someday.org)")
                            (org-agenda-files '("~/org/agenda/someday.org"))))))
          ("A" "All Tasks"
           ((todo "TODO"
                  ((org-agenda-overriding-header "All TODO Tasks")))
            (todo "PROG"
                  ((org-agenda-overriding-header "In Progress")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting for...")))))
          ))

  ;; キーバインド
    (after! general
    (general-define-key
    :states '(normal motion)
    :keymaps 'doom-leader-map
    "o a" (general-key-dispatch #'org-agenda
                "a" #'org-agenda
                "c" #'org-capture)))
)

;; Org Roam - Second Brain Note Taking System
(use-package! org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-db-location (expand-file-name "org-roam.db" doom-cache-dir))
  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "daily/")
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:10}" 'face 'org-tag)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n d n" . org-roam-dailies-capture-today)
         ("C-c n d d" . org-roam-dailies-goto-today)
         ("C-c n d Y" . org-roam-dailies-capture-yesterday)
         ("C-c n d T" . org-roam-dailies-capture-tomorrow)
         ("C-c n d v" . org-roam-dailies-capture-date)
         ("C-c n d c" . org-roam-dailies-goto-date))
  :config
  (org-roam-setup)

  ;; Capture templates
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :memo:\n\n")
           :unnarrowed t)
          ("l" "programming language" plain
           "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :Programming:\n\n")
           :unnarrowed t)
          ("b" "book notes" plain
           "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Memos\n\n%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :Book:\n\n")
           :unnarrowed t)
          ("m" "mathematical concept" plain
           "* References\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :math:memo:\n\n")
           :unnarrowed t)
          ("r" "research paper" plain
           "* Bibliographic Information\n- Author: %?\n- Title: ${title}\n- Year: \n- Journal/Conference: \n- DOI/URL: \n\n* Abstract\n\n* Key Contributions\n\n* Methodology\n\n* Results\n\n* Personal Notes\n\n* Related Work\n\n* Applications\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :paper:research:\n\n")
           :unnarrowed t)
          ("M" "MOC (Map of Content)" plain
           "* Overview\n%?\n\n* Core Concepts\n\n* Advanced Topics\n\n* Applications\n\n* Learning Path\n\n* Resources\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :MOC:\n\n")
           :unnarrowed t)
          ("p" "project" plain
           "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :Project:\n\n")
           :unnarrowed t)
          ("s" "study project" plain
           "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :Study:\n\n")
           :unnarrowed t)
          ("n" "meeting minutes" plain
           "* Meeting Information\n- Topic: %^{Meeting Topic}\n- Start Time: %^{Start Time}\n- Attendees: %^{Attendees}\n\n* Agenda\n%?\n\n* Discussion\n\n* Checks\n** TODO\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :minutes:\n\n")
           :unnarrowed t)))

  ;; Daily notes capture templates
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n"))))

  ;; Quick note insertion function
  (defun org-roam-node-insert-immediate (arg &rest args)
    "Insert a new org-roam node without opening its buffer."
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates
           (list (append (car org-roam-capture-templates)
                         '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))

  ;; Filter and listing functions
  (defun org-roam-filter-by-tag (tag-name)
    "Filter org-roam nodes by TAG-NAME."
    (mapcar #'org-roam-node-file
            (seq-filter
             (lambda (node)
               (member tag-name (org-roam-node-tags node)))
             (org-roam-node-list))))

  (defun org-roam-list-notes-by-tag (tag-name)
    "List all notes with specified TAG-NAME."
    (mapcar #'org-roam-node-title
            (seq-filter
             (lambda (node)
               (member tag-name (org-roam-node-tags node)))
             (org-roam-node-list))))

   (map! :leader
        :prefix "n"
        "f" nil
        "d" nil
        "l" nil))

  ;; Doom-specific keybindings
  (after! org-roam
    (map! :leader
          :prefix ("n" . "notes")
          :desc "Org roam buffer toggle" "l" #'org-roam-buffer-toggle
          :desc "Find node" "f" #'org-roam-node-find
          :desc "Insert node" "i" #'org-roam-node-insert
          :desc "Insert node immediate" "I" #'org-roam-node-insert-immediate
          (:prefix ("d" . "daily")
           :desc "Capture today" "n" #'org-roam-dailies-capture-today
           :desc "Goto today" "d" #'org-roam-dailies-goto-today
           :desc "Capture yesterday" "Y" #'org-roam-dailies-capture-yesterday
           :desc "Capture tomorrow" "T" #'org-roam-dailies-capture-tomorrow
           :desc "Capture date" "v" #'org-roam-dailies-capture-date
           :desc "Goto date" "c" #'org-roam-dailies-goto-date)))

  ;; Init hooks
  (add-hook 'after-init-hook #'org-roam-db-autosync-mode)

  (setq org-roam-db-update-on-file-change t)

  ;; Auto sync on save
  (add-hook 'after-save-hook
            (lambda ()
              (when (and (buffer-file-name)
                         (string-match-p "org-roam" (buffer-file-name)))
                (org-roam-db-sync))))

  ;; Disable Doom default notes bindings

(after! org
  (defun orgfold-get-fold-info-file-name ()
    (concat (buffer-file-name) ".fold"))

  (defun orgfold-save ()
    (when (and (buffer-file-name)
               (file-exists-p (orgfold-get-fold-info-file-name)))
      (save-excursion
        (goto-char (point-min))
        (let (foldstates)
          (unless (looking-at outline-regexp)
            (outline-next-visible-heading 1))
          (while (not (eobp))
            (push (when (seq-some (lambda (o) (overlay-get o 'invisible))
                                  (overlays-at (line-end-position)))
                    t)
                  foldstates)
            (outline-next-visible-heading 1))
          (with-temp-file (orgfold-get-fold-info-file-name)
            (prin1 (nreverse foldstates) (current-buffer)))))))

  (defun orgfold-restore ()
    (when (buffer-file-name)
      (save-excursion
        (goto-char (point-min))
        (let* ((foldfile (orgfold-get-fold-info-file-name))
               (foldstates
                (when (file-readable-p foldfile)
                  (with-temp-buffer
                    (insert-file-contents foldfile)
                    (when (> (buffer-size) 0)
                      (read (current-buffer)))))))
          ;; Be defensive: only proceed if we read a proper list of states.
          (when (listp foldstates)
            (show-all)
            (goto-char (point-min))
            (unless (looking-at outline-regexp)
              (outline-next-visible-heading 1))
            (while (and (consp foldstates)
                        (not (eobp)))
              (when (pop foldstates)
                (hide-subtree))
              (outline-next-visible-heading 1))))))

  (defun orgfold-init ()
    (interactive)
    (when (buffer-file-name)
      (save-excursion
        (goto-char (point-min))
        (let (foldstates)
          (unless (looking-at outline-regexp)
            (outline-next-visible-heading 1))
          (while (not (eobp))
            (push (when (seq-some (lambda (o) (overlay-get o 'invisible))
                                  (overlays-at (line-end-position)))
                    t)
                  foldstates)
            (outline-next-visible-heading 1))
          (with-temp-file (orgfold-get-fold-info-file-name)
            (prin1 (nreverse foldstates) (current-buffer)))))
      (add-hook 'after-save-hook #'orgfold-save nil t)
      (message "Fold state tracking initialized for %s" (buffer-name))))

  (defun orgfold-activate ()
    (when (and (buffer-file-name)
               (file-exists-p (orgfold-get-fold-info-file-name)))
      (orgfold-restore)
      (add-hook 'after-save-hook #'orgfold-save nil t)))

  ;; キーバインドの設定
  (map! :map org-mode-map
        :leader
        :prefix ("m" . "org")
        :desc "Initialize fold tracking" "f" #'orgfold-init)

  (add-hook 'org-mode-hook #'orgfold-activate)
  ;; (add-hook 'org-mode-hook #'org-modern-mode)  ; Temporarily disabled for testing
  )

(defun find-file-insert (filename &optional wildcards)
  "Insert the selected file name at the current point."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (insert filename))

(defun find-file-insert-relative (filename &optional wildcards)
  "Insert the relative filename of the selected file at the current point."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let* ((current-buffer (buffer-file-name (current-buffer)))
         (directory (file-name-directory current-buffer))
         (relative-filename (file-relative-name filename directory)))
    (insert relative-filename)))

(map! :leader
      :desc "Insert selected file name at point" "if" #'find-file-insert
      :desc "Insert selected file name at point" "ir" #'find-file-insert-relative)

(with-eval-after-load 'peep-dired
  (evil-define-key 'normal peep-dired-mode-map
    (kbd "<SPC>") 'peep-dired-scroll-page-down
    (kbd "C-<SPC>") 'peep-dired-scroll-page-up
    (kbd "<backspace>") 'peep-dired-scroll-page-up
    (kbd "j") 'peep-dired-next-file
    (kbd "k") 'peep-dired-prev-file)

  (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4"))
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-enable-on-directories t))

;; Add the key binding SPC d p to toggle peep-dired-mode while in dired (you can add the key binding you like)
(map! :leader
      (:after dired
              (:map dired-mode-map
               :desc "peep mode" "d p" #'peep-dired)))

(use-package! dired-git-info
  :after dired
  :config
  (add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable)
  )

(map! :leader
      :desc "Run GPTel" "c g" #'gptel
      :desc "GPTel menu" "m g" #'gptel-menu
      :desc "GPTel rewrite" "r g" #'gptel-rewrite
      :desc "GPT Chat" "s g" #'gptel-send)

(defun +open-vterm ()
  "Open a new vterm in a vertical split or switch to it."
  (interactive)
  (if (get-buffer "*vterm*")
      (pop-to-buffer "*vterm*")
    (select-window (split-window-right))
    (vterm)))

(defun +vterm-switch ()
  "Switch to the most recent vterm buffer."
  (interactive)
  (if-let ((buf (car (seq-filter
                      (lambda (b) (string-match-p "\\*vterm" (buffer-name b)))
                      (buffer-list)))))
      (pop-to-buffer buf)
    (message "No vterm buffer found.")))

(after! vterm
  (setq vterm-shell "/bin/zsh")  ;; Replace with your preferred shell
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))

(after! vterm
  (set-popup-rule! "*doom:vterm-popup:*"
    :size 0.30
    :vslot -4
    :select t
    :quit nil
    :ttl 0
    :side 'right)
  (setq vterm-shell "/bin/zsh")
  (setq vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t))
(map! :leader
      :desc "Toggle vterm popup" "o t" #'+vterm/toggle
      :desc "Open vterm here"    "o T" #'+vterm/here)

;; macOS専用設定
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; システム依存のパス設定は環境に合わせて変更してください
  ;; (setenv "PATH" (concat "XXX:" (getenv "PATH")))
  ;; (add-to-list 'exec-path "XXX")
  )

;; Linux専用設定
(when (eq system-type 'gnu/linux)
  ;; 必要に応じてLinux専用の設定をここに追加
  ;; 例: PostgreSQLのパス設定など
  ;; (setenv "PATH" (concat "/usr/bin:" (getenv "PATH")))
  )

(setq gptel-api-key (getenv "OPENAI_API_KEY"))

(use-package! gptel
  :config
  (setq gptel-model 'o4-mini)
  (setq gptel-backend
        (gptel-make-openai
         "OpenAI"
         :key  #'gptel-api-key
         :stream t
         :models '(o4-mini))))

(menu-bar-mode -1)

;; Work around occasional sqlite finalizer errors on GC
;; (wrong-type-argument sqlitep nil)
(with-eval-after-load 'sqlite
  (defun my/sqlite-close-safely (orig connection &rest args)
    (condition-case _
        (when (and connection (fboundp 'sqlitep) (sqlitep connection))
          (apply orig connection args))
      (error nil)))
  (advice-add 'sqlite-close :around #'my/sqlite-close-safely))

(defun my/vterm-here-safe (&optional arg)
  "Safely call +vterm/here with optional ARG to avoid wrong-args error."
  (interactive "P")
  (+vterm/here arg))

(map! :leader
      :desc "Toggle vterm popup"    "o t" #'+vterm/toggle
      :desc "Open inline vterm"     "o T" #'my/vterm-here-safe
      :desc "Open vterm & launch OpenCode" "o o"
      (lambda ()
        (interactive)
        ;; open inline vterm safely
        (my/vterm-here-safe)
        ;; launch OpenCode CLI in that terminal buffer
        (vterm-send-string "opencode")
        (vterm-send-return)))

(use-package! ivy-posframe
  :after ivy
  :init
  (ivy-posframe-mode 1)
  :config
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-center))) ;; Pop up at the center
  (setq ivy-posframe-parameters
        '((internal-border-width . 10)
          (left-fringe . 8)
          (right-fringe . 8))))

(after! ivy
  (ivy-mode 1)  ;; Ensures ivy-mode is on
  ;; Optional: recommended for performance and UX
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t))

(defun config-org-auto-tangle ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.doom.d/config.org"))
    (org-babel-tangle)))

(add-hook 'after-save-hook #'config-org-auto-tangle)
