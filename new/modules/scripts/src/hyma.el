(defvar bb:scripts-dir (f-expand "~/.config/dotfiles/new/modules/scripts"))

(defun bb-hyma|org-task ()
  (interactive)
  (require 'parseedn)
  (let* ((default-directory "~/.config/dotfiles/new/modules/scripts")
         (items (->> (shell-command-to-string "bb ./src/hyma.clj project tasks")
                     (json-read-from-string)
                     (mapcar
                      (lambda (item)
                        (let* ((col-count 80)
                               (title (->> (a-get item 'title)
                                           (s-truncate col-count)
                                           (s-pad-right col-count " ")))
                               (id (-> (format "(%s)" (a-get item 'id))
                                       (propertize 'face 'font-lock-doc-face)))
                               (display-string (concat title " " id)))
                          (cons display-string item)))))))
    (ivy-read "title" items
              :action
              (-lambda ((_ . item))
                (let* ((title (a-get item 'title))
                       (url (a-get-in item '(content url)))
                       (org-body-temp-file (when-let ((body (a-get-in item '(content body))))
                                             (make-temp-file nil nil nil body)))
                       (org-body (when org-body-temp-file
                                   (shell-command-to-string (t! "pandoc -f markdown -t org <<org-body-temp-file>>")))))
                  (insert (t! "
** TODO <<title>> :@PENPOT:
:PROPERTIES:
:GITHUB_URL: <<url>>
:END:

<<org-body>>

")))))))

(defun bb-hyma-standup|copy-link ())
