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

  (setq org-archive-location "~/Documents/org/agenda/archive/%s_archive::")

  (after! org-capture
    (setq org-capture-templates
          '(("t" "Task to Inbox" entry
             (file+headline "~/Documents/org/agenda/inbox.org" "Tasks")
             "* TODO %?")

            ("p" "Project Task" entry
             (file+headline "~/Documents/org/agenda/gtd.org" "Projects")
             "* TODO %? :@work:\nSCHEDULED: %(org-insert-time-stamp (current-time) t)\n")

            ("r" "Routine Task" entry
             (file+headline "~/Documents/org/agenda/routines.org" "Routines")
             "* TODO %? \nSCHEDULED: <> \n:PROPERTIES:\n:STYLE: habit\n:END:")

            ("s" "Someday/Maybe" entry
             (file+headline "~/Documents/org/agenda/someday.org" "Ideas")
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
                        (org-agenda-files '("~/Documents/org/agenda/inbox.org"))))
            (tags-todo "+DEADLINE>=\"<today>\"+DEADLINE<=\"<+1w>\""
                       ((org-agenda-overriding-header "🔥 Deadlines This Week")))
            (tags "project"
                  ((org-agenda-overriding-header "Project Status")))))

          ("s" "💡 Someday / Maybe"
           ((todo "SOMEDAY" ; <- SOMEDAYキーワードのタスクを全ファイルから探す
                       ((org-agenda-overriding-header "On Hold Tasks (by Keyword)")))
            (tags-todo "/DONE" ; <- someday.orgの中のタスクを表示する
                       ((org-agenda-overriding-header "Idea List (in someday.org)")
                            (org-agenda-files '("~/Documents/org/agenda/someday.org"))))))
          ("A" "All Tasks"
           ((todo "TODO"
                  ((org-agenda-overriding-header "All TODO Tasks")))
            (todo "PROG"
                  ((org-agenda-overriding-header "In Progress")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting for...")))))
          ))

  ;; キーバインド
  (map! :leader
        "o a" 'org-agenda
        "o c" 'org-capture))
