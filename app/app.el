(prefer-coding-system 'utf-8)
(require 'package)
(require 'cl-lib)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize t)


(setq dependencies
       '(
         elnode
         mustache
         ht
         dash
         s
         f
         ))

(unless (cl-loop for it in dependencies always (package-installed-p it))
  (package-refresh-contents))

(dolist (dependency dependencies)
  (unless (package-installed-p dependency)
    (package-install dependency))
  (require dependency))

(require 'org)
(require 'ox)

(setq data-dir "/root/data")
(setq posts-dir (concat data-dir "/posts/"))
(setq theme-dir (concat data-dir "/themes/default/"))
(setq layout-file-path (concat theme-dir "layout.html"))
(setq blog-title "だるい")


(setq app-routes
      '(("^.*//$" . index-handler)
        ("^.*//posts/\\(.*?\\)/?$" . posts-handler)
        ("^.*//assets/\\(.*\\)$" . assets-handler)
        ("^.*//public/\\(.*\\)$" . public-handler)
        ("^.*//\\(.*\\)" . app-handler)
        ))

(defun elnode-env ()
  (intern (or (getenv "ELNODE_ENV") "development")))

(defun developmentp ()
  (equal (elnode-env) 'development))

(defun productionp ()
  (equal (elnode-env) 'production))

(defun view (view-name)
  (with-temp-buffer
    (insert-file-contents (concat theme-dir view-name ".html"))
    (buffer-string)))

(defun to-str (obj)
  (format "%s" obj))

(defun hr ()
  (message "--------------------------------------------------"))

(defun render-html (httpcon context &optional code)
  (let ((code (or code 200))
        (layout (f-read-text layout-file-path)))
    (elnode-http-start httpcon code '("Content-type" . "text/html"))
    (elnode-http-return httpcon (mustache-render layout context))))

(defun render-org (httpcon file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (let* ((article (get-article file-path))
           (title (gethash "title" article))
           (content (mustache-render (view "article") article))
           (context (ht ("title" title) ("yield" content))))
      (render-html httpcon context))))

(defun assets-handler (httpcon)
  (let ((file-path (concat data-dir (elnode-http-pathinfo httpcon))))
    (if (file-exists-p file-path)
        (elnode-send-file httpcon file-path)
      (elnode-send-404 httpcon))))

(defun app-handler (httpcon)
  (let ((hello (elnode-http-pathinfo httpcon)))
    (render-html httpcon (ht ("title" "hi") ("yield" (format "Hello, %s" hello))))))

(defun index-handler (httpcon)
  (let* ((articles (get-articles))
         (content (mustache-render
                   (view "index")
                   (ht ("articles" articles))))
         (context (ht
                   ("title" blog-title)
                   ("yield" content))))
    (render-html httpcon context)))

(defun posts-handler (httpcon)
  (let ((file-path (concat posts-dir (elnode-http-mapping httpcon 1) ".org")))
    (if (file-exists-p file-path)
        (render-org httpcon file-path)
      (render-html
       httpcon
       (ht ("title" "404 - File not found") ("yield" (format "File `%s` was not found." file-path)))
       404))))

(defun org-date-to-str (date)
  (apply 'format "%s-%s-%s %s:%s"
         (list (to-str (plist-get date :year-start))
               (to-str (plist-get date :month-start))
               (to-str (plist-get date :day-start))
               (to-str (plist-get date :hour-start))
               (to-str (plist-get date :minute-start)))))

(defvar default-article-published-at "1970-01-01 09:00")

(defun get-article (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (let* ((route-path (concat "/" (f-no-ext (f-relative file-path data-dir))))
           (properties (ht<-plist (org-export-get-environment)))
           (title (format "%s" (or (car (gethash :title properties)) "No title")))
           (tags (mapcar (lambda (it) (ht ("tag" it)))
                         (s-split ", *"
                                  (or (gethash :keywords properties) "") t)))
           (published_at (or
                          (let* ((date (cadar (gethash :date properties))))
                            (if date (org-date-to-str date)))
                          default-article-published-at))
           (position (time-to-seconds (date-to-time published_at)))
           (content (org-to-html file-path)))
      (ht ("href" route-path)
          ("title" title)
          ("tags" tags)
          ("published_at" published_at)
          ("position" position)
          ("content" content)))))

(defun org-to-html (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (org-export-as 'html nil nil t (list 'string))))

(defun get-articles ()
  (let* ((article-paths (f-files posts-dir (lambda (file) (s-matches? "\\.org$" file)) nil))
         (articles (mapcar (lambda (article-path) (get-article article-path)) article-paths)))
    (cl-sort articles '> :key (lambda (it) (gethash "position" it)))))

(defun root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon app-routes))


(elnode-start 'root-handler :port (or (getenv "ELNODE_PORT") 80) :host (or (getenv "ELNODE_HOST") "0.0.0.0"))
(message "--> Server started.")


;; (elnode-stop (or (getenv "ELNODE_PORT") 80))
