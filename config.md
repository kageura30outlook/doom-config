
# Table of Contents

1.  [Introduction](#orgbc7a955)
2.  [Basic UI](#orgd37a8e4)
3.  [Fonts](#orgf1d99f0)
4.  [Lines](#org0bd5b76)
5.  [Org Mode](#org196e4d2)
6.  [Org Roam V2](#org1562589)
7.  [Keybindings](#orgacd1f49)
8.  [GPTel Keybinds](#orgee1af98)
9.  [vterm](#org6df4cb6)
    1.  [vterm integration](#orgb1d6b09)
    2.  [opencode with vterm](#org5c4e8b8)
10. [Key loading](#org097aa39)
11. [GPTel Setup](#org9cc8964)
12. [Emacs window tilling fix](#org652b774)
13. [Keybinding fixes & OpenCode integration](#orge7f0b16)
14. [ivy-posframe config](#orgb3e305b)
15. [Ivy Mode](#org1aed008)
16. [Modus theme settings](#org320ef79)
17. [Load modus theme](#org1368805)
18. [Auto tangle](#org4fc29d6)



<a id="orgbc7a955"></a>

# Introduction

This is my literate Doom Emacs configuration.
All code blocks here are tangled into \`config.el\`.


<a id="orgd37a8e4"></a>

# Basic UI

Customize theme, line numbers, and fonts.

    (setq doom-theme 'doom-palenight)   ;; Theme


<a id="orgf1d99f0"></a>

# Fonts

Fonts config

    (setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 15)
          doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
          doom-big-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 24))
    (after! doom-themes
      (setq doom-themes-enable-bold t
            doom-themes-enable-italic t))
    (custom-set-faces!
     '(font-lock-comment-face :slant italic)
     '(font-lock-keyword-face :slant italic))


<a id="org0bd5b76"></a>

# Lines

    (setq display-line-numbers-type t)  ;; Absolute line numbers
    (map! :leader
           :desc "Toggle truncate lines"
            "t t" #'toggle-truncate-lines)


<a id="org196e4d2"></a>

# Org Mode

Set up my org directory for notes and tasks.

    (setq org-directory "~/org-roam/"
          org-hide-emphasis-markers t)


<a id="org1562589"></a>

# Org Roam V2

My knowledge management system using Org-Roam v2.

    (use-package! org-roam
      :init
      (setq org-roam-v2-ack t)
      :custom
      (org-roam-directory (file-truename "~/org-roam"))
      (org-roam-completion-everywhere t)
      :config
      (org-roam-db-autosync-enable)
      (map! :leader
            :prefix "n"
            :desc "Find Node" "r" #'org-roam-node-find
            :desc "Insert Node" "i" #'org-roam-node-insert))


<a id="orgacd1f49"></a>

# Keybindings

Custom keybindings for faster access.

    (map! :leader
          :desc "Edit config.org" "f i" #'(lambda () (interactive) (find-file "~/.doom.d/config.org")))
    (map! :leader
          :desc "GPT Chat" "s g" #'gptel-send)


<a id="orgee1af98"></a>

# GPTel Keybinds

    (map! :leader
          :desc "Run GPTel" "c g" #'gptel
          :desc "GPTel menu" "m g" #'gptel-menu
          :desc "GPTel rewrite" "r g" #'gptel-rewrite)

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


<a id="org6df4cb6"></a>

# vterm

\#+begin<sub>src</sub> emacs-lisp
(after! vterm
  (setq vterm-shell &ldquo;/bin/zsh&rdquo;)  ;; Replace with your preferred shell
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))
\#+end<sub>src</sub>\*


<a id="orgb1d6b09"></a>

## vterm integration

    
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


<a id="org5c4e8b8"></a>

## opencode with vterm

    (map! :leader
          :desc "Opencode in terminal" "o o"
          (lambda ()
            (interactive)
            (+vterm/here)
            (vterm-send-string "opencode")
            (vterm-send-return)))


<a id="org097aa39"></a>

# Key loading

    (setq gptel-api-key (getenv "OPENAI_API_KEY"))


<a id="org9cc8964"></a>

# GPTel Setup

\#+begin<sub>src</sub> emacs-lisp
(use-package! gptel
  :config
  ;; Default model
  (setq gptel-model &ldquo;gpt-4o&rdquo;) ;; or &ldquo;gpt-4o&rdquo; if you have access
  (setq gptel-backend (gptel-make-openai
                       &ldquo;OpenAI&rdquo;
                       :host &ldquo;api.openai.com&rdquo;
                       :endpoint &ldquo;/v1/chat/completions&rdquo;
                       :key gptel-api-key))) ;; key will be loaded securely


<a id="org652b774"></a>

# Emacs window tilling fix

    (menu-bar-mode t)


<a id="orge7f0b16"></a>

# Keybinding fixes & OpenCode integration

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


<a id="orgb3e305b"></a>

# ivy-posframe config

    (use-package! ivy-posframe
      :after ivy
      :config
      (setq ivy-posframe-display-functions-alist
            '((t . ivy-posframe-display-at-frame-center))) ;; Pop up at the center
      (setq ivy-posframe-parameters
            '((internal-border-width . 10)
              (left-fringe . 8)
              (right-fringe . 8)))
      (ivy-posframe-mode 1))


<a id="org1aed008"></a>

# Ivy Mode

Force ivy-mode to start early

    (after! ivy
      (ivy-mode 1)  ;; Ensures ivy-mode is on
      ;; Optional: recommended for performance and UX
      (setq ivy-use-virtual-buffers t
            ivy-count-format "(%d/%d) "
            enable-recursive-minibuffers t))


<a id="org320ef79"></a>

# Modus theme settings

    (setq modus-themes-mode-line '(borderless))
    (setq modus-themes-region '(bg-only no-extend))
    (setq modus-themes-completions '(minimal))


<a id="org1368805"></a>

# Load modus theme

    (load-theme 'modus-vivendi)


<a id="org4fc29d6"></a>

# Auto tangle

    (defun config-org-auto-tangle ()
      (when (string-equal (buffer-file-name)
                          (expand-file-name "~/.doom.d/config.org"))
        (org-babel-tangle)))
    
    (add-hook 'after-save-hook #'config-org-auto-tangle)

