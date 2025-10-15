;;; shell_history.el --- Emacs interface for shell history script -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs interface for the shell_history.clj script that tracks command history
;; for projects and provides various ways to access and manage that history.

;;; Code:

(require 'ivy)
(require 'cl-lib)
(require 's)

(defun shell-history|execute (&rest args)
  "Execute shell history script with ARGS.
If first arg is :working-directory, use the next arg as working directory."
  (let ((cmd (->> (concat "shell_history"
                          (-some->> args
                                    (-filter #'some?)
                                    (s-join " ")
                                    (s-prepend " "))))))
    (shell-command-to-string cmd)))

(defun shell-history/execute-command (command)
  "Execute COMMAND in the shell using the current buffer's default directory."
  (async-shell-command command))

(defun shell-history/insert-command (command)
  "Insert COMMAND into the current buffer at point."
  (insert command))

(defun shell-history/copy-command (command)
  "Copy COMMAND to the kill ring."
  (kill-new command)
  (message "Command copied: %s" command))

(defun shell-history/log-command (command &optional execution-dir)
  "Log COMMAND to the shell history database.
EXECUTION-DIR defaults to current directory if not provided."
  (interactive "sCommand to log: ")
  (let ((dir (or execution-dir default-directory)))
    (shell-history|execute "log"
                           "--command" (shell-quote-argument command)
                           "--execution-dir" (shell-quote-argument dir))
    (message "Command logged: %s" command)))

(defun shell-history/parse-command-line (line)
  "Parse a command history LINE into count and command."
  (when (string-match "^\\s-*\\([0-9]+\\)\\s-+\\(.+\\)$" line)
    (list (string-to-number (match-string 1 line))
          (match-string 2 line))))

(defun shell-history/format-command-with-count (command count &optional width)
  "Format COMMAND with COUNT right-aligned. WIDTH defaults to 80."
  (let* ((width (or width 80))
         (count-str (format "(%d)" count))
         (available-width (- width (length count-str) 1))
         (truncated-command (if (> (length command) available-width)
                                (concat (substring command 0 (- available-width 3)) "...")
                              command))
         (padding (- width (length truncated-command) (length count-str))))
    (format "%s%s%s"
            truncated-command
            (make-string (max 1 padding) ?\s)
            (propertize count-str 'face 'font-lock-comment-face))))

(defun shell-history/extract-command-from-formatted (formatted-line)
  "Extract the actual command from a formatted line with right-aligned count."
  (when (string-match "^\\(.+?\\)\\s-+(.+)$" formatted-line)
    (string-trim (match-string 1 formatted-line))))

(defun shell-history|list-all ()
  "List all commands by frequency across all projects."
  (interactive)
  (let* ((output (shell-history|execute "list"))
         (lines (split-string output "\n" t))
         (ivy-width (or (and (boundp 'ivy-width) ivy-width) 80))
         (commands (mapcar (lambda (line)
                             (let ((parsed (shell-history/parse-command-line line)))
                               (if parsed
                                   (shell-history/format-command-with-count
                                    (cadr parsed) (car parsed) ivy-width)
                                 line)))
                           lines)))
    (ivy-read "Shell History (All): " commands
              :action '(1)
              ("RET" (lambda (x)
                       (let ((cmd (shell-history/extract-command-from-formatted x)))
                         (when cmd (shell-history/execute-command cmd))))
               "Execute command")
              ("i" (lambda (x)
                     (let ((cmd (shell-history/extract-command-from-formatted x)))
                       (when cmd (shell-history/insert-command cmd))))
               "Insert command")
              ("c" (lambda (x)
                     (let ((cmd (shell-history/extract-command-from-formatted x)))
                       (when cmd (shell-history/copy-command cmd))))
               "Copy command")
              :caller 'shell-history|list-all)))

(defun shell-history|list-project ()
  "List commands for the current project."
  (interactive)
  (let* ((output (shell-history|execute "project"))
         (lines (split-string output "\n" t))
         (ivy-width (or (and (boundp 'ivy-width) ivy-width) 80))
         (commands (if (string-match-p "No commands found" output)
                       '("No commands found for current project")
                     (cl-loop for line in lines
                              when (shell-history/parse-command-line line)
                              collect (let ((parsed (shell-history/parse-command-line line)))
                                        (shell-history/format-command-with-count
                                         (cadr parsed) (car parsed) ivy-width))))))
    (if (equal commands '("No commands found for current project"))
        (message "No commands found for current project (not in a git repository?)")
      (ivy-read "Shell History (Project): " commands
                :action '(1)
                ("RET" (lambda (x)
                         (let ((cmd (shell-history/extract-command-from-formatted x)))
                           (when cmd (shell-history/execute-command cmd))))
                 "Execute command")
                ("i" (lambda (x)
                       (let ((cmd (shell-history/extract-command-from-formatted x)))
                         (when cmd (shell-history/insert-command cmd))))
                 "Insert command")
                ("c" (lambda (x)
                       (let ((cmd (shell-history/extract-command-from-formatted x)))
                         (when cmd (shell-history/copy-command cmd))))
                 "Copy command")
                :caller 'shell-history|list-project))))

(defun shell-history|fzf ()
  "Get commands for fzf integration."
  (interactive)
  (shell-history|execute "fzf"))

(defun shell-history|interactive ()
  "Run the interactive shell history browser."
  (interactive)
  (let ((default-directory "/home/floscr/.config/dotfiles/new/modules/scripts"))
    (start-process "shell-history-interactive"
                   "*shell-history-interactive*"
                   "bb" "./src/shell_history.clj" "interactive")))

(defun shell-history/current-command ()
  "Get the current command from shell buffer if available."
  (cond
   ;; In term-mode or shell-mode
   ((or (eq major-mode 'term-mode)
        (eq major-mode 'shell-mode)
        (eq major-mode 'eshell-mode))
    (let ((line (thing-at-point 'line t)))
      (when line
        (string-trim line))))
   ;; In other modes, use region or current line
   (t
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (thing-at-point 'line t)))))

(defun shell-history/log-current-command ()
  "Log the current command (from region, current line, or shell prompt)."
  (interactive)
  (let ((command (shell-history/current-command)))
    (if command
        (shell-history/log-command (string-trim command))
      (call-interactively #'shell-history/log-command))))

;; Integration with existing shell modes
(defun shell-history/setup-hooks ()
  "Set up hooks for automatic command logging."
  (add-hook 'comint-input-filter-functions #'shell-history/maybe-log-command))

(defun shell-history/maybe-log-command (command)
  "Maybe log COMMAND if it's significant."
  ;; Only log commands that are not too short and not common navigation
  (when (and command
             (> (length command) 2)
             (not (string-match-p "^\\s-*\\(cd\\|ls\\|pwd\\)\\s-*$" command)))
    (shell-history/log-command command))
  command)

;; Key bindings suggestion (uncomment to use)
;; (global-set-key (kbd "C-c h l") #'shell-history|list-project)
;; (global-set-key (kbd "C-c h L") #'shell-history|list-all)
;; (global-set-key (kbd "C-c h s") #'shell-history/log-current-command)
;; (global-set-key (kbd "C-c h i") #'shell-history|interactive)

(provide 'shell_history)
;;; shell_history.el ends here
