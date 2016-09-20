;;; darui.io/app.el --- darui website

;; Copyright (c) 2015 supermomonga

;; Author: supermomonga (@supermomonga)
;; URL: https://github.com/supermomonga/darui.io
;; Version 0.0.1
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; My personal website だるい's source repository. http://darui.io/
;; If you notice any wrong mention, typo, or any mistakes, you can send me a Pull Request.

;;; Code:
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
         htmlize
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
(setq assets-dir (concat theme-dir "assets/"))
(setq layout-file-path (concat theme-dir "layout.html"))
(setq rss-file-path (concat theme-dir "rss.xml"))
(setq blog-title "だるい")
(setq blog-description "だるいブログ")
(setq blog-url "https://darui.io/")

;; Logging
(setq elnode--do-error-logging nil)
;; (setq elnode-msg-levels (list :info :status :debug :warning))
(setq elnode-msg-levels nil)
(setq elnode-log-files-directory (concat data-dir "/logs"))
(setq elnode-error-log-to-messages nil)

;; org
;; http://orgmode.org/manual/Publishing-options.html
(setq org-export-default-language "ja")
(setq org-export-with-section-numbers nil)
(setq org-export-with-sub-superscripts nil)
;; If we use html5, ATTR_HTML will be ignored
;; (setq org-html-doctype "html5")
;; (setq org-html-html5-fancy t)
(setq org-html-toplevel-hlevel 3)
(setq org-src-preserve-indentation nil)
(setq org-html-htmlize-output-type 'css)
(setq org-html-table-default-attributes '(:class "table table-bordered table-condensed"))

;; htmlize workaround. see http://wenshanren.org/?p=781
(defun org-font-lock-ensure ()
  (font-lock-fontify-buffer))

(setq app-routes
      '(("^.*//$" . index-handler)
        ("^.*//rss.rdf$" . rss-handler)
        ("^.*//posts/\\(.*?\\)/?$" . posts-handler)
        ("^.*//assets/\\(.*\\)$" . assets-handler)
        ("^.*//public/\\(.*\\)$" . public-handler)
        ("^.*//.+$" . 404-handler)
        ))

(defun elnode-env ()
  (intern (or (getenv "ELNODE_ENV") "development")))

(defun developmentp ()
  (equal (elnode-env) 'development))

(defun productionp ()
  (equal (elnode-env) 'production))

(defmacro tapa (obj &rest body)
  `(let ((it ,obj))
     ,@body)
  obj)

(defmacro tapp (obj &optional prefix)
  `(progn
     (message (concat (or ,prefix "") "%s") ,obj)
     ,obj))

(defun view (view-name)
  (with-temp-buffer
    (insert-file-contents (concat theme-dir view-name ".html"))
    (buffer-string)))

(defun to-str (obj)
  (format "%s" obj))

(defun hr ()
  (message "--------------------------------------------------"))

(defun page-title (title &optional site-title)
  (if site-title
      (format "%s - %s"
              title
              blog-title)
    title))

(defun global-context (httpcon)
  (let* ((request-host (elnode-http-host httpcon))
         (request-path (elnode-http-pathinfo httpcon))
         (request-url (concat "http://" request-host request-path))) ;; elnode not supports ssl for now
  (ht ("request/host" request-host)
      ("request/path" request-path)
      ("request/url" request-url)
      ("hidden" nil))))

(defun render (view context)
  (let ((context (ht-merge (global-context httpcon) context)))
    (mustache-render view context)))

(defun render-html (httpcon context &optional code)
  (let ((code (or code 200))
        (layout (f-read-text layout-file-path)))
    (elnode-http-start httpcon code '("Content-type" . "text/html; charset=utf-8"))
    (elnode-http-return httpcon (render layout context))))

(defun render-org (httpcon file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (let* ((article (get-article file-path))
           (title (page-title (gethash "title" article) blog-title))
           (content (render (view "article") article))
           (context (ht ("title" title) ("yield" content))))
      (render-html httpcon context))))

(defun render-rss (httpcon context &optional code)
  (let ((code (or code 200))
        (layout (f-read-text rss-file-path)))
    (elnode-http-start httpcon code '("Content-type" . "application/xml .rdf; charset=utf-8"))
    (elnode-http-return httpcon (render layout context))))

(defun assets-handler (httpcon)
  (let ((file-path (concat theme-dir (elnode-http-pathinfo httpcon))))
    (if (file-exists-p file-path)
        (elnode-send-file httpcon file-path)
      (elnode-send-404 httpcon))))

(defun app-handler (httpcon)
  (let ((hello (elnode-http-pathinfo httpcon)))
    (render-html httpcon (ht ("title" "hi") ("yield" (format "Hello, %s" hello))))))

(defun rss-handler (httpcon)
  (let* ((articles (get-articles))
         (context (ht
                   ("title" blog-title)
                   ("description" blog-description)
                   ("url" blog-url)
                   ("articles" articles))))
    (render-rss httpcon context)))

(defun index-handler (httpcon)
  (let* ((articles (get-articles))
         (content (mustache-render
                   (view "index")
                   (ht ("articles" articles))))
         (context (ht
                   ("title" blog-title)
                   ("yield" content))))
    (render-html httpcon context)))

(defun 404-handler (httpcon)
  (render-html
   httpcon
   (ht ("title"
        "404 File not found")
       ("yield"
        (mst--escape-html "Could not find the page which was you requested.")))
   404))

(defun posts-handler (httpcon)
  (let ((file-path (concat posts-dir (elnode-http-mapping httpcon 1) ".org")))
    (if (file-exists-p file-path)
        (render-org httpcon file-path)
      (404-handler httpcon))))

(defun org-date-to-str (date)
  (apply 'format "%s-%s-%s %s:%s"
         (list (to-str (plist-get date :year-start))
               (to-str (plist-get date :month-start))
               (to-str (plist-get date :day-start))
               (to-str (plist-get date :hour-start))
               (to-str (plist-get date :minute-start)))))

(setq default-article-published-at "1970-01-01 09:00")

(defun get-article (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (let* ((route (f-no-ext (f-relative file-path data-dir)))
           (route-path (concat "/" route))
           (route-url (concat blog-url route))
           (properties (ht<-plist (org-export-get-environment)))
           (title (format "%s" (or (car (gethash :title properties)) "No title")))
           (tags (mapcar (lambda (it) (ht ("tag" it)))
                         (s-split ", *"
                                  (or (gethash :keywords properties) "") t)))
           (date (cadar (gethash :date properties)))
           (published_at (if date
                             (org-date-to-str date)
                           default-article-published-at))
           (position (time-to-seconds (date-to-time published_at)))
           (content (org-to-html file-path)))
      (ht ("href" route-path)
          ("url" route-url)
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

