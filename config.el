(setq doom-theme 'doom-moonlight)

;; Ensure `mcp.el` is correctly located in the `.doom.d` folder.
(add-to-list 'load-path (expand-file-name "mcp.el" "~/.doom.d/"))

;; Load and configure MCP package.
(use-package! mcp
  :after gptel
  :custom
  (setq mcp-hub-servers
      '(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/Users/Kageura/")))
        ("fetch" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-fetch")))
        ("memory" . (:command "npx" :args ("-y" "@pulsemcp/basic-memory")))
        ("sequencethink" . (:command "npx" :args ("-y" "@arben-adm/mcp-sequential-thinking")))
        ("git" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-github")))
        ("python-sdk" . (:command "python3" :args ("-m" "mcp.server.fastmcp" "--spec" "python-sdk")))
        ("puppeteer" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-puppeteer")))
        ("emacs" . (:command "bash" :args ("-c" "~/.config/doom/bin/doomscript ~/.config/doom/bin/emacs-mcp")))))

  :config
  ;; Load the MCP hub component.
  (require 'mcp-hub)
  ;; Start all servers after Emacs initialization.
  (add-hook 'after-init-hook #'mcp-hub-start-all-server))

(setq mac-command-modifier      'super
      ns-command-modifier       'super
      mac-option-modifier       'meta
      ns-option-modifier        'meta
      mac-left-option-modifier 'meta)

(setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 15)
      doom-variable-pitch-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 15)
      doom-big-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
 '(font-lock-comment-face :slant italic)
 '(font-lock-keyword-face :slant italic))

(setq display-line-numbers-type t)  ;; Absolute line numbers
(map! :leader
       :desc "Toggle truncate lines"
        "t t" #'toggle-truncate-lines)

(setq org-directory "~/org-roam/"
      org-hide-emphasis-markers t)

(use-package! org-roam
  :init
  (setq org-roam-v2-ack t)  ;; acknowledge v2
  :custom
  (org-roam-directory (file-truename "~/org-roam")) ;; your notes directory
  (org-roam-completion-everywhere t)
  :config
  (org-roam-db-autosync-enable) ;; keeps database updated
  (map! :leader
        :prefix "n"
        :desc "Find node" "r f" #'org-roam-node-find
        :desc "Insert node" "r i" #'org-roam-node-insert
        :desc "Show graph" "r g" #'org-roam-graph
        :desc "Capture node" "r c" #'org-roam-capture))

(map! :leader
      :desc "Edit config.org" "f i" #'(lambda () (interactive) (find-file "~/.doom.d/config.org")))

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

(map! :leader
      :desc "Opencode in terminal" "o o"
      (lambda ()
        (interactive)
        (+vterm/here)
        (vterm-send-string "opencode")
        (vterm-send-return)))

(use-package! gptel
  :after request
  :config
  ;; ── API Key ────────────────────────────────────────────────────────────────
  (setq! gptel-api-key (getenv "OPENAI_API_KEY"))

  ;; ── Default Backend ─────────────────────────────────────────────────────────
  (setq! gptel-backend
         (gptel-make-openai
          :name    "OpenAI"
          :api-key gptel-api-key
          :model   "gpt-4"
          :host    "https://api.openai.com"
          :path    "/v1/chat/completions"
          :params  '((:temperature . 0.7)
                     (:max_tokens  . 1024))))

  ;; ── Org Integration ─────────────────────────────────────────────────────────
  (after! org
    (setq! gptel-org-auto-set-properties t)))

(menu-bar-mode nil)

(defun my/vterm-here-safe (&optional arg)
  "Safely call +vterm/here with optional ARG to avoid wrong-args error."
  (interactive "P")
  (+vterm/here arg))

(map! :leader
      :desc "Toggle vterm popup"    "o t" #'+vterm/toggle
      :desc "Open inline vterm"     "o T" #'my/vterm-here-safe
      (lambda ()
        (interactive)
        (my/vterm-here-safe))

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
          (right-fringe . 8)))
)

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
