(defpackage aprblog
  (:use :cl :gtk4 :split-sequence)
  (:export #:main))
(in-package :aprblog)

(setq 3bmd-code-blocks:*code-blocks* t)

(defparameter *global-config-path* nil)
(defparameter *global-config* nil)

(defparameter *blog-root* nil)

(defvar *home.html* nil)
(defvar *page.html* nil)
(defvar *post.html* nil)
(defvar *tags.html* nil)
(defvar *post.md* nil)
(defvar *page.md* nil)

(defparameter *local-acceptor-port* 4242)
(defvar *local-acceptor* nil)

(defun set-blog-root-variable (path)
  (setf (getf *global-config* :blog-root) path)
  (with-open-file (out *global-config-path*
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists :supersede)
    (prin1 *global-config* out))
  (setf *blog-root* path
	*home.html* (ignore-errors
		     (djula:compile-template*
		      (merge-pathnames #P"templates/home.html"
				       *blog-root*)))
	*page.html* (ignore-errors
		     (djula:compile-template*
		      (merge-pathnames #P"templates/page.html"
				       *blog-root*)))
	*post.html* (ignore-errors
		     (djula:compile-template*
		      (merge-pathnames #P"templates/post.html"
				       *blog-root*)))
	*tags.html* (ignore-errors (djula:compile-template*
				    (merge-pathnames #P"templates/tags.html"
						     *blog-root*)))
	*post.md* (ignore-errors (djula:compile-template*
				  (merge-pathnames #P"templates/post.md"
						   *blog-root*)))
	*page.md* (ignore-errors (djula:compile-template*
				  (merge-pathnames #P"templates/page.md"
						   *blog-root*)))
	*local-acceptor* (make-instance 'hunchentoot:easy-acceptor
					:document-root (merge-pathnames "publish/" *blog-root*)
					:port *local-acceptor-port*)))

(defun copy-directory-contents (from dest)
  (mapcar #'(lambda (in)
              (let ((out (make-pathname
                          :name (pathname-name in) :type (pathname-type in)
                          :directory (append (pathname-directory dest)
                                             (set-exclusive-or (pathname-directory from)
                                                               (pathname-directory in)
                                                               :test #'equal))
                          :defaults dest)))
                (ensure-directories-exist out)
                (uiop:copy-file in out)))
          (delete-if #'uiop:directory-pathname-p
                     (directory (merge-pathnames "**/*.*" from)))))

(defun parse-document (path)
  (with-open-file (in path)
    (let ((line1 (read-line in nil))
          front-matter)
      (when (string= (string-trim '(#\Space #\Return #\Newline) line1) "---")
        (setf front-matter (read in))
        (loop for l = (read-line in nil)
              until (or (null l)
                        (string= (string-trim '(#\Space #\Return #\Newline) l) "---")))
        (setf (getf front-matter :date)
              (local-time:parse-timestring (getf front-matter :date)))
        (setf (getf front-matter :update)
              (local-time:universal-to-timestamp
               (file-write-date path)))
        (setf (getf front-matter :filename) (pathname-name path)))
      (values front-matter
              (string-trim '(#\Space #\Return #\Newline)
                           (apply #'concatenate 'string
                                  (loop with buf = (make-array 4096 :element-type 'character)
                                        for len = (read-sequence buf in)
                                        collect (subseq buf 0 len)
                                        until (null (listen in)))))))))

(defun generate-feed ()
  (let ((conf (with-open-file (in (merge-pathnames "config.sexp" *blog-root*))
                (read in))))
    (with-open-file (out (merge-pathnames "publish/atom.xml" *blog-root*)
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (cxml:with-xml-output (cxml:make-character-stream-sink out :indentation 2 :canonical nil)
        (cxml:with-namespace (nil "http://www.w3.org/2005/Atom")
          (cxml:with-element "feed"
            (cxml:with-element "title" (cxml:text (getf conf :blog-title)))
            (cxml:with-element "subtitle" (cxml:text (getf conf :blog-subtitle)))
            (cxml:with-element "icon" (cxml:text (concatenate
                                                  'string (getf conf :domain) (getf conf :logo))))
            (cxml:with-element "link"
              (cxml:attribute "href" (concatenate 'string (getf conf :domain) "atom.xml"))
              (cxml:attribute "rel" "self"))
            (cxml:with-element "link"
              (cxml:attribute "href" (getf conf :domain)))
            (cxml:with-element "updated"
              (cxml:text (local-time:format-timestring nil (local-time:now))))
            (cxml:with-element "id" (cxml:text (getf conf :domain)))
            (cxml:with-element "author"
              (cxml:with-element "name" (cxml:text (getf conf :author))))

            (loop for post in (directory (merge-pathnames "posts/*.md" *blog-root*))
                  do (multiple-value-bind (front-matter content)
                         (parse-document post)
                       (let ((url (concatenate 'string
                                               (getf conf :domain) (pathname-name post) ".html")))
                         (cxml:with-element "entry"
                           (cxml:with-element "title" (cxml:text (getf front-matter :title)))
                           (cxml:with-element "link" (cxml:attribute "href" url))
                           (cxml:with-element "id" (cxml:text url))
                           (cxml:with-element "published"
                             (cxml:text (local-time:format-timestring
                                         nil (getf front-matter :date))))
                           (cxml:with-element "updated"
                             (cxml:text (local-time:format-timestring
                                         nil (getf front-matter :update))))
                           (cxml:with-element "summary"
                             (cxml:text
                              (if (> (length content) 300)
                                  (concatenate 'string (subseq content 0 297) "...")
                                  content)))))))))))))

(defun generate ()
  (let ((posts (directory
                (merge-pathnames #P"posts/*.md"
                                 *blog-root*)))
        (pages (directory
                (merge-pathnames #P"pages/*.md"
                                 *blog-root*)))
        (publish (merge-pathnames #P"publish/" *blog-root*))
        (res (merge-pathnames #P"resources/" *blog-root*))
        (conf (with-open-file (in (merge-pathnames #P"config.sexp" *blog-root*))
                (read in)))
        all-tags)
    (when (probe-file publish)
      (uiop:delete-directory-tree publish :validate t)
      (ensure-directories-exist publish))
    (copy-directory-contents res publish)

    (loop for page in pages
          collect
          (multiple-value-bind (front-matter content)
              (parse-document page)
            (setf (getf front-matter :content)
                  (with-output-to-string (out)
                    (3bmd:parse-string-and-print-to-stream
                     content out)))
            (list (lambda ()
                    (with-open-file
                        (out (make-pathname :name (pathname-name page) :type "html"
                                            :defaults publish)
                             :direction :output
                             :if-does-not-exist :create)
                      (apply #'djula:render-template* *page.html* out
                             (append conf front-matter))))
                  (list :title (getf front-matter :title)
                        :path (format nil "/~A.html" (pathname-name page)))))
            into r
          finally (progn (setf (getf conf :pages) (mapcar #'second r))
                         (mapcar (lambda (i) (funcall (car i))) r)))
    (loop for post in posts
          collect
          (multiple-value-bind (front-matter content)
              (parse-document post)
            (loop for tag in (getf front-matter :tags) do
              (uiop:if-let ((pos (position-if (lambda (lst) (equal (getf lst :name) tag)) all-tags)))
                (setf (getf (nth pos all-tags) :posts)
                      (cons front-matter (getf (nth pos all-tags) :posts)))
                (setf all-tags
                      (cons (list :name tag :posts (list front-matter))
                            all-tags))))

            (setf (getf front-matter :content)
                  (with-output-to-string (out)
                    (3bmd:parse-string-and-print-to-stream
                     content out)))
            (with-open-file
                (out (make-pathname :name (pathname-name post) :type "html"
                                    :defaults publish)
                     :direction :output
                     :if-does-not-exist :create)
              (apply #'djula:render-template* *post.html* out
                     (append conf front-matter)))
            (when (> (length content) 300)
              (setf (getf front-matter :content)
                    (with-output-to-string (out)
                      (3bmd:parse-string-and-print-to-stream
                       (concatenate 'string (subseq content 0 300) "...")
                       out))))
            front-matter)
            into r
          finally (with-open-file (out (merge-pathnames "index.html" publish)
                                       :direction :output
                                       :if-does-not-exist :create)
                    (setf (getf conf :posts) r)
                    (apply #'djula:render-template* *home.html* out
                           conf)))

    (with-open-file (out (merge-pathnames "tags.html" publish)
                         :direction :output
                         :if-does-not-exist :create)
      (apply #'djula:render-template* *tags.html* out
             (append conf (list :tags all-tags))))

    (generate-feed)))



(defparameter *initial-configs*
  '(
    :blog-title "Title"
    :blog-subtitle "Subtitle"
    :domain "https://examples.com"
    :author "Me"
    :author-description "My descriptions"
    :author-email "no@thankyou.com"
    :author-twitter "https://twitter.com/"
    :author-telegram "https://t.me/"
    :author-github "https://github.com/"
    :logo "images/favicon.png"
    :bg "#FFFFFF"
    :bg-second "#EEEEEE"
    :fg "#000000"
    :fg-second "#5A5A5A"
    :deploy-git-repo "https://github.com/apr3vau/apr3vau.github.io"
    :deploy-git-branch "master"
    :deploy-git-commit-message "Update blog content."
    :deploy-git-token ""
    :ui-editor-scheme "Adwaita-dark"
    :ui-editor-font "Serif"
    :ui-editor-font-size "12pt"
    ))

(defmacro with-define-widgets (binding-and-exps &body body)
  (loop for (var def . exps) in binding-and-exps
        collect (list var def) into bindings
        nconc exps into all-exps
        finally (return `(let* (,@bindings)
                           ,@all-exps
                           ,@body))))

(defun post-title-factory-setup (factory item)
  (declare (ignore factory))
  (setf (list-item-child item) (make-label :str "")))

(defun post-title-factory-bind (factory item)
  (declare (ignore factory))
  (setf (label-text (gobj:coerce (list-item-child item) 'label))
        (getf (parse-document
               (string-object-string
                (gobj:coerce (list-item-item item) 'string-object)))
              :title)))

(defun post-date-factory-setup (factory item)
  (declare (ignore factory))
  (setf (list-item-child item) (make-label :str "")))

(defun post-date-factory-bind (factory item)
  (declare (ignore factory))
  (setf (label-text (gobj:coerce (list-item-child item) 'label))
        (local-time:format-timestring
         nil (getf (parse-document
                    (string-object-string
                     (gobj:coerce (list-item-item item) 'string-object)))
                   :date)
         :format '(:year #\- (:month 2) #\- (:day 2)))))

(defun set-serve-button (serve-button serve-button-content)
  (if (and *local-acceptor* (hunchentoot:started-p *local-acceptor*))
      (setf (widget-css-classes serve-button)
            '("destructive-action" "pill")
            (adw:button-content-icon-name serve-button-content)
            "window-close-symbolic"
            (adw:button-content-label serve-button-content)
            "Stop Server")
      (setf (widget-css-classes serve-button)
            '("suggested-action" "pill")
            (adw:button-content-icon-name serve-button-content)
            "network-server-symbolic"
            (adw:button-content-label serve-button-content)
            "Serve Local")))

(defparameter *settings-groups*
  `(("Blog Informations"
     ,(lambda (n) (or (and (>= (length (symbol-name n)) 4)
			   (string-equal "blog" n :end2 4))
		      (member n '("domain") :test 'string-equal))))
    ("Author Informations"
     ,(lambda (n) (and (>= (length (symbol-name n)) 6)
		       (string-equal "author" n :end2 6))))
    ("Webpage Look and Feel"
     ,(lambda (n) (or (and (>= (length (symbol-name n)) 2)
			   (or (string-equal "bg" n :end2 2)
			       (string-equal "fg" n :end2 2)))
		      (member n '("logo") :test 'string-equal))))
    ("GUI"
     ,(lambda (n) (or (and (>= (length (symbol-name n)) 2)
			   (string-equal "ui" n :end2 2)))))
    ("Git Deploy"
     ,(lambda (n) (or (and (>= (length (symbol-name n)) 10)
			   (string-equal "deploy-git" n :end2 10)))))
    ("Others"
     ,(lambda (n) (declare (ignore n)) t))))

(define-application (:name app
                     :id "io.github.apr3vau")
  (define-main-window (window (adw:make-application-window :app *application*))
      (let ((conf (alexandria:when-let
		      ((path (ignore-errors
			      (truename (merge-pathnames "config.sexp" *blog-root*)))))
		    (with-open-file (in path) (read in))))
          editor-file editor-modified settings-fields)
      (setf (widget-size-request window) '(400 600))
      (widget-add-css-class window "devel")
      (with-define-widgets
          (
           (toolbar-view
            (adw:make-toolbar-view)
            (setf (adw:window-content window) toolbar-view))
           (toast-overlay
            (adw:make-toast-overlay)
            (setf (adw:toolbar-view-content toolbar-view) toast-overlay))
           (view-stack
            (adw:make-view-stack)
            (setf (adw:toast-overlay-child toast-overlay) view-stack))
           (header
            (adw:make-header-bar)
            (adw:toolbar-view-add-top-bar toolbar-view header))
           (view-switcher
            (adw:make-view-switcher)
            (setf (adw:view-switcher-policy view-switcher) adw:+view-switcher-policy-wide+
                  (adw:view-switcher-stack view-switcher) view-stack)
            (setf (adw:header-bar-title-widget header) view-switcher))
           (overview-box
            (make-box :orientation +orientation-horizontal+
                      :spacing 10)
            (adw:view-stack-add-titled-with-icon
             view-stack overview-box "Overview" "Overview" "view-list-symbolic"))
           (status
            (adw:make-status-page)
            (setf (widget-hexpand-p status) nil
                  (adw:status-page-icon-name status) "system-file-manager-symbolic")
            (box-append overview-box status))

           ;; Blog actions
           (actions-grid
            (make-grid)
            (setf (widget-margin-start actions-grid) 10
                  (widget-margin-end actions-grid) 10
                  (grid-row-spacing actions-grid) 10
                  (grid-column-spacing actions-grid) 10
                  (grid-column-homogeneous-p actions-grid) t
                  (adw:status-page-child status) actions-grid))
           (generate-button-content
            (adw:make-button-content)
            (setf (adw:button-content-icon-name generate-button-content)
                  "application-x-executable-symbolic"
                  (adw:button-content-label generate-button-content)
                  "Generate"))
           (generate-toast
            (adw:make-toast :title "Static Files Generated.")
            (setf (adw:toast-timeout generate-toast) 3))
           (generate-button
            (make-button :label "Generate")
            (setf (widget-hexpand-p generate-button) t
                  (widget-css-classes generate-button) '("suggested-action" "pill")
                  (button-child generate-button) generate-button-content)
            (connect generate-button "clicked"
                     (lambda (button) (declare (ignore button))
                       (when (hunchentoot:started-p *local-acceptor*)
                         (hunchentoot:stop *local-acceptor*)
                         (set-serve-button serve-button serve-button-content))
                       (generate)
                       (adw:toast-overlay-add-toast toast-overlay generate-toast)))
            (grid-attach actions-grid generate-button 1 1 1 1))

           (serve-button-content
            (adw:make-button-content))
           (serve-button
            (make-button :label "Serve Local")
            (setf (widget-hexpand-p serve-button) t
                  (button-child serve-button) serve-button-content)
            (set-serve-button serve-button serve-button-content)
            (connect serve-button "clicked"
                     (lambda (button) (declare (ignore button))
                       (if (hunchentoot:started-p *local-acceptor*)
                           (hunchentoot:stop *local-acceptor*)
                           (progn (hunchentoot:start *local-acceptor*)
                                  (ignore-errors
                                   (trivial-open-browser:open-browser
                                    (format nil "http://localhost:~A/" *local-acceptor-port*)))))
                       (set-serve-button serve-button serve-button-content)))
            (grid-attach actions-grid serve-button 2 1 1 1))
           (exit-button-content
            (adw:make-button-content)
            (setf (adw:button-content-icon-name exit-button-content)
                  "application-exit-symbolic"
                  (adw:button-content-label exit-button-content)
                  "Exit"))
           (exit-button
            (make-button :label "Exit")
            (setf (widget-css-classes exit-button) '("pill")
                  (widget-hexpand-p exit-button) t
                  (button-child exit-button) exit-button-content)
            (connect exit-button "clicked"
                     (lambda (button) (declare (ignore button))
                       (window-destroy window)))
            (grid-attach actions-grid exit-button 1 2 1 1))

           ;; Deploy
           (deploy-start-toast
            (adw:make-toast :title "Start Deployment...")
            (setf (adw:toast-timeout deploy-finish-toast) 60
                  (adw:toast-button-label deploy-start-toast) "Abort")
            (let ((abort-action (gio:make-simple-action :name "abort-action"
                                                        :parameter-type nil)))
              (connect abort-action "activate"
                       (lambda ()
                         (bt2:destroy-thread
                          (find (lambda (thread)
                                  (equal (bt2:thread-name thread) "aprblog_deploy"))
                                (bt2:all-threads)))
                         (adw:toast-dismiss deploy-start-toast)))))
           (deploy-finish-toast
            (adw:make-toast :title "Deployment finished.")
            (setf (adw:toast-timeout deploy-finish-toast) 3
                  (adw:toast-priority deploy-finish-toast) adw:+toast-priority-high+))
           (deploy-button-content
            (adw:make-button-content)
            (setf (adw:button-content-icon-name deploy-button-content)
                  "document-send-symbolic"
                  (adw:button-content-label deploy-button-content)
                  "Deploy Git"))
           (deploy-button
            (make-button :label "Deploy Git")
            (setf (widget-css-classes deploy-button) '("suggested-action" "pill")
                  (widget-hexpand-p deploy-button) t
                  (button-child deploy-button) deploy-button-content)
            (connect
             deploy-button "clicked"
             (lambda (button) (declare (ignore button))
               (text-buffer-insert
                output-buffer
                (text-buffer-get-iter-at-mark output-buffer output-mark)
                (format nil "--- Deployment started at ~A ---~%~%"
                        (local-time:format-timestring nil (local-time:now))))
               (bt2:make-thread
                (lambda ()
                  (flet ((run-with-output (command)
                           (uiop:run-program
                            command
                            :output
                            (lambda (in)
                              (loop with buf = (make-array 4096 :element-type 'character)
                                    for len = (read-sequence buf in)
                                    do (run-in-main-event-loop ()
                                         (text-buffer-insert
                                          output-buffer
                                          (text-buffer-get-iter-at-mark output-buffer output-mark)
                                          (subseq buf 0 len)))
                                    until (< len 4096))))))
                    (setf (adw:view-stack-visible-child view-stack) output-box)
                    (let ((publish (merge-pathnames "publish/" *blog-root*))
                          (deploy-dir (merge-pathnames "deploy-git/" *blog-root*)))
                      (ensure-directories-exist deploy-dir)
                      (uiop:chdir deploy-dir)
                      (unless (probe-file (merge-pathnames ".git/" deploy-dir))
                        (run-with-output '("git" "init")))
                      (loop for p in (delete-if
                                      (lambda (p)
                                        (let ((filename (file-namestring p)))
                                          (or (and (> (length filename) 0)
                                                   (char= (aref filename 0) #\.))
                                              (char= (aref (car (last (pathname-directory p))) 0) #\.))))
                                      (directory (merge-pathnames "*.*" deploy-dir)))
                            if (uiop:directory-pathname-p p)
                              do (uiop:delete-directory-tree p :validate t)
                            else do (delete-file p))
                      (copy-directory-contents publish deploy-dir)
                      (run-with-output `("git" "config" "user.name" ,(getf conf :author)))
                      (run-with-output `("git" "config" "user.email" ,(getf conf :author-email)))
                      (run-with-output '("git" "add" "-A"))
                      (handler-case
                          (run-with-output
                           (list "git" "commit" "-m" (getf conf :deploy-git-commit-message)))
                        (error (e) (declare (ignore e))
                          (run-in-main-event-loop ()
                            (text-buffer-insert
                             output-buffer
                             (text-buffer-get-iter-at-mark output-buffer output-mark)
                             (format nil "~%Warning: Commit failed.~%~%")))))
                      (run-with-output
                       (delete
                        nil (list
                             "git" "push" "-u"
                             (let ((uri (quri:uri (getf conf :deploy-git-repo))))
                               (setf (quri:uri-userinfo uri) (getf conf :deploy-git-token))
                               (quri:render-uri uri))
                             (when (getf conf :deploy-git-branch)
                               (getf conf :deploy-git-branch))
                             "--force")))
                      (run-in-main-event-loop ()
                        (text-buffer-insert
                         output-buffer
                         (text-buffer-get-iter-at-mark output-buffer output-mark)
                         (format nil "~% --- done --- ~%~%")))
                      (adw:toast-overlay-add-toast toast-overlay deploy-finish-toast)
                      (adw:toast-dismiss deploy-start-toast))))
                :name "aprblog_deploy")
               (adw:toast-overlay-add-toast toast-overlay deploy-start-toast)))
            (grid-attach actions-grid deploy-button 2 2 1 1))

	   (change-root-button
	    (make-button :label "Change Root")
	    (setf (widget-css-classes change-root-button) '("pill")
                  (widget-hexpand-p change-root-button) t)
	    (let ((content (adw:make-button-content)))
	      (setf (adw:button-content-label content) "Change Root"
		    (adw:button-content-icon-name content) "folder-open-symbolic"
		    (button-child change-root-button) content))
	    (connect change-root-button "clicked"
		     (lambda (button) (declare (ignore button))
		       (funcall set-blog-root)))
	    (grid-attach actions-grid change-root-button 1 3 1 1))
	   (new-blog-button
	    (make-button :label "New Blog")
	    (setf (widget-css-classes new-blog-button) '("pill")
                  (widget-hexpand-p new-blog-button) t)
	    (let ((content (adw:make-button-content)))
	      (setf (adw:button-content-label content) "New Blog"
		    (adw:button-content-icon-name content) "folder-new-symbolic"
		    (button-child new-blog-button) content))
	    (connect new-blog-button "clicked"
		     (lambda (button) (declare (ignore button))
		       (funcall new-blog)))
	    (grid-attach actions-grid new-blog-button 2 3 1 1))

           ;; Posts & Pages
           (posts-toolbar-view
            (adw:make-toolbar-view)
            (box-append overview-box posts-toolbar-view))
           (posts-stack
            (adw:make-view-stack)
            (setf (adw:toolbar-view-content posts-toolbar-view) posts-stack))
           (posts-switcher
            (adw:make-view-switcher)
            (setf (adw:view-switcher-policy posts-switcher) adw:+view-switcher-policy-wide+
                  (adw:view-switcher-stack posts-switcher) posts-stack)
            (adw:toolbar-view-add-top-bar posts-toolbar-view posts-switcher))

           (posts-actions
            (make-box :orientation +orientation-horizontal+
                      :spacing 10)
            (setf (widget-css-classes posts-actions) '("toolbar")
                  (box-homogeneous-p posts-actions) t)
            (adw:toolbar-view-add-bottom-bar posts-toolbar-view posts-actions))
           (posts-delete
            (make-button :label "Delete")
            (setf (widget-css-classes posts-delete) '("destructive-action"))
            (let ((content (adw:make-button-content)))
              (setf (adw:button-content-label content) "Delete"
                    (adw:button-content-icon-name content) "user-trash-symbolic")
              (setf (button-child posts-delete) content))
            (connect posts-delete "clicked" delete-post)
            (box-append posts-actions posts-delete))
           (posts-edit
            (make-button :label "Edit")
            (setf (widget-css-classes posts-edit) '("raised"))
            (let ((content (adw:make-button-content)))
              (setf (adw:button-content-label content) "Edit"
                    (adw:button-content-icon-name content) "document-edit-symbolic")
              (setf (button-child posts-edit) content))
            (connect posts-edit "clicked" edit-post)
            (box-append posts-actions posts-edit))
           (posts-new
            (make-button :label "New")
            (setf (widget-css-classes posts-new) '("suggested-action"))
            (let ((content (adw:make-button-content)))
              (setf (adw:button-content-label content) "New"
                    (adw:button-content-icon-name content) "document-new-symbolic")
              (setf (button-child posts-new) content))
            (connect posts-new "clicked" new-post)
            (box-append posts-actions posts-new))

           ;; Posts
           (posts-model
            (make-string-list
             :strings (when *blog-root*
			(mapcar #'namestring
				(directory
				 (merge-pathnames "posts/*.md" *blog-root*))))))
           (posts-selection-model
            (make-single-selection :model posts-model))
           (title-factory
            (make-signal-list-item-factory)
            (connect title-factory "setup" #'post-title-factory-setup)
            (connect title-factory "bind" #'post-title-factory-bind))
           (date-factory
            (make-signal-list-item-factory)
            (connect date-factory "setup" #'post-date-factory-setup)
            (connect date-factory "bind" #'post-date-factory-bind))
           (posts-list-view
            (make-column-view :model posts-selection-model)
            (setf (widget-hexpand-p posts-list-view) t
                  (widget-vexpand-p posts-list-view) t
                  (column-view-show-column-separators-p posts-list-view) t)
            (connect posts-list-view "activate" edit-post)
            (adw:view-stack-add-titled-with-icon
             posts-stack posts-list-view "Posts" "Posts" "x-office-document-symbolic"))
           (posts-date-col
            (make-column-view-column
             :title "Date" :factory date-factory)
            (setf (column-view-column-resizable-p posts-date-col) t)
            (column-view-append-column posts-list-view posts-date-col))
           (posts-title-col
            (make-column-view-column
             :title "Title" :factory title-factory)
            (setf (column-view-column-expand-p posts-title-col) t
                  (column-view-column-resizable-p posts-title-col) t)
            (column-view-append-column posts-list-view posts-title-col))

           ;; Pages
           (pages-model
            (make-string-list
             :strings (when *blog-root*
			(mapcar #'namestring
				(directory
				 (merge-pathnames "pages/*.md" *blog-root*))))))
           (pages-selection-model
            (make-single-selection :model pages-model))
           (pages-factory
            (make-signal-list-item-factory)
            (connect pages-factory "setup" #'post-title-factory-setup)
            (connect pages-factory "bind" #'post-title-factory-bind))
           (pages-list-view
            (make-list-view :model pages-selection-model
                            :factory pages-factory)
            (setf (widget-hexpand-p pages-list-view) t
                  (widget-vexpand-p pages-list-view) t
                  (widget-css-classes pages-list-view) '("title-3"))
            (connect pages-list-view "activate" edit-post)
            (adw:view-stack-add-titled-with-icon
             posts-stack pages-list-view "Pages" "Pages" "view-paged-symbolic"))
	   
           ;; Write
           (write-box
            (make-box :orientation +orientation-vertical+
                      :spacing 0)
            (adw:view-stack-add-titled-with-icon
             view-stack write-box "Write" "Write" "text-editor-symbolic"))
           (editor-window
            (make-scrolled-window)
            (box-append write-box editor-window))
           (editor-buffer
            (sourceview:make-buffer
             :language (sourceview:language-manager-get-language
                        (sourceview:make-language-manager) "markdown"))
            (connect editor-buffer "changed"
                     (lambda (buffer) (declare (ignore buffer))
                       (setf editor-modified t))))
           (editor
            (sourceview:make-view :buffer editor-buffer)
            (setf (widget-vexpand-p editor) t
                  (text-view-left-margin editor) 10
                  (text-view-wrap-mode editor) +wrap-mode-word+
                  (sourceview:view-highlight-current-line-p editor) t)
	    (connect editor "paste-clipboard"
		     (lambda (self) (declare (ignore self))
		       (let* ((clipboard (widget-clipboard editor))
			      (mimes (gdk:content-formats-mime-types
				      (gdk:clipboard-formats clipboard))))
			 (alexandria:when-let ((type (find "image/" mimes :test 'uiop:string-prefix-p)))
			   (let ((dialog (make-file-dialog)))
			     (setf (file-dialog-title dialog) "Import resource to"
				   (file-dialog-initial-folder dialog)
				   (gio:file-new-for-path
				    (namestring (or (probe-file (merge-pathnames "resources/" *blog-root*))
						    (probe-file *blog-root*)))))
			     (file-dialog-save
			      dialog window (gio:make-cancellable)
			      (cffi:get-callback
			       (cffi:defcallback paste-clipboard-callback
				   :void ((obj :pointer) (res :pointer) (data :pointer))
				 (declare (ignore obj data))
				 (alexandria:when-let
				     ((path (ignore-errors
					     (gio:file-path (file-dialog-save-finish dialog res)))))
				   (gdk:clipboard-read-texture-async
				    clipboard (gio:make-cancellable)
				    (cffi:get-callback
				     (cffi:defcallback read-clipboard-callback
					 :void ((obj :pointer) (res :pointer) (data :pointer))
				       (declare (ignore obj data))
				       (gdk:texture-save-to-png
					(gdk:clipboard-read-texture-finish clipboard res)
					path)
				       (when (search (append (pathname-directory *blog-root*) '("resources"))
						     (pathname-directory path)
						     :test 'equal)
					(text-buffer-insert-at-cursor
					 editor-buffer
					 (format nil "![image](/~A)"
						 (enough-namestring path (merge-pathnames "resources/" *blog-root*)))))))
				    (cffi:null-pointer)))))
			      (cffi:null-pointer)))))))
            (setf (window-child editor-window) editor))
           (editor-css-provider
            (make-css-provider)
            (style-context-add-provider (widget-style-context editor)
                                        editor-css-provider
                                        +style-provider-priority-application+))

           (modeline
            (make-box :orientation +orientation-horizontal+
                      :spacing 0)
            (setf (widget-hexpand-p modeline) t
                  (widget-css-classes modeline) '("toolbar"))
            (box-append write-box modeline))
           (modeline-pathname
            (make-label :str "NIL")
            (setf (widget-hexpand-p modeline-pathname) t
                  (widget-css-classes modeline-pathname) '("title-4"))
            (box-append modeline modeline-pathname))
           (file-saved-toast
            (adw:make-toast :title "Changes saved.")
            (setf (adw:toast-timeout file-saved-toast) 2))
	   (modeline-resource
	    (make-button :label "Resource")
	    (setf (widget-css-classes modeline-resource) '("raised")
                  (button-child modeline-resource)
                  (let ((content (adw:make-button-content)))
                    (setf (adw:button-content-label content) "Resource"
                          (adw:button-content-icon-name content) "insert-link-symbolic")
                    content))
	    (connect modeline-resource "clicked"
		     (lambda (button) (declare (ignore button))
		       (let ((dialog (make-file-dialog)))
			 (setf (file-dialog-title dialog) "Select a file"
			       (file-dialog-initial-folder dialog)
			       (gio:file-new-for-path
				(namestring (or (probe-file (merge-pathnames "resources/" *blog-root*))
						(probe-file *blog-root*)))))
			 (file-dialog-open
			  dialog window (gio:make-cancellable)
			  (cffi:get-callback
			   (cffi:defcallback paste-clipboard-callback
			       :void ((obj :pointer) (res :pointer) (data :pointer))
			     (declare (ignore obj data))
			     (alexandria:when-let
				 ((path (ignore-errors
					 (gio:file-path (file-dialog-open-finish dialog res)))))
			       (if (search (append (pathname-directory *blog-root*) '("resources"))
					   (pathname-directory path)
					   :test 'equal)
				 (text-buffer-insert-at-cursor
				  editor-buffer
				  (format nil "![image](/~A)"
					  (enough-namestring path (merge-pathnames "resources/" *blog-root*))))
				 (let ((dialog (adw:make-message-dialog
						:parent window
						:heading "Selected file isn't under resource directory"
						:body "It is an invalid resource and will not be published, please copy it to the resource folder first")))
				   (adw:message-dialog-add-response dialog "OK" "OK")
				   (window-present dialog))))))
			  (cffi:null-pointer)))))
	    (box-append modeline modeline-resource))
           (modeline-save
            (make-button :label "Save")
            (setf (widget-css-classes modeline-save) '("suggested-action")
                  (button-child modeline-save)
                  (let ((content (adw:make-button-content)))
                    (setf (adw:button-content-label content) "Save"
                          (adw:button-content-icon-name content) "media-floppy-symbolic")
                    content))
            (connect modeline-save "clicked"
                     (lambda (button) (declare (ignore button))
                       (funcall save-editor-file-then
                                (lambda ()
                                  (setf (adw:toast-title file-saved-toast)
                                        (format nil "~A saved."
                                                (file-namestring editor-file)))
                                  (adw:toast-overlay-add-toast toast-overlay file-saved-toast)))))
            (box-append modeline modeline-save))

           ;; Output
           (output-box
            (make-box :orientation +orientation-vertical+ :spacing 0)
            (adw:view-stack-add-titled-with-icon
             view-stack output-box "Output" "Output" "system-log-out-symbolic"))
           (output-buffer
            (make-text-buffer :table nil))
           (output-mark
            (text-buffer-create-mark
             output-buffer nil (text-buffer-get-iter-at-offset output-buffer 0) nil)
            (text-buffer-insert
             output-buffer
             (text-buffer-get-iter-at-mark output-buffer output-mark)
             (format nil "# Running outputs will show here.~%~%")))
           (output-view
            (make-text-view :buffer output-buffer)
            (setf (widget-vexpand-p output-view) t
                  (widget-margin-all output-view) 10
                  (text-view-editable-p output-view) nil)
            (let ((provider (make-css-provider)))
              (css-provider-load-from-data
               provider "textview { font-family: Monospace; font-size: 10pt; }")
              (style-context-add-provider
               (widget-style-context output-view)
               provider
               +style-provider-priority-application+))
            (box-append output-box output-view))

           (settings-box
            (make-box :orientation +orientation-vertical+
                      :spacing 0)
            (adw:view-stack-add-titled-with-icon
             view-stack settings-box "Settings" "Settings" "emblem-system-symbolic"))
           (settings-window
            (make-scrolled-window)
            (box-append settings-box settings-window))
	   
	   (make-settings-page
	    (lambda (&rest args) (declare (ignore args))
	      (let ((page (adw:make-preferences-page)))
		(setf (widget-vexpand-p page) t)
		(setf settings-fields nil)
		(let ((groups (loop for (title pred) in *settings-groups*
				    for group = (adw:make-preferences-group)
				    do (setf (adw:preferences-group-title group) title)
				       (adw:preferences-page-add page group)
				    collect (list pred group))))
		  (loop for (name value) on conf by 'cddr
			when (stringp value) do
			  (let ((group (second (assoc-if (lambda (pred) (funcall pred name)) groups))))
			    (let ((name-field (make-entry))
				  (value-field (make-entry))
				  (delete-button (make-button :icon-name "user-trash-symbolic"))
				  (box (make-box :orientation +orientation-horizontal+ :spacing 0)))
			      (setf settings-fields
				    (nconc settings-fields (list (list name-field value-field))))
			      (setf (widget-css-classes box) '("linked")
				    (widget-hexpand-p name-field) t
				    (widget-hexpand-p value-field) t
				    (entry-buffer-text (entry-buffer name-field)) (string-capitalize name)
				    (entry-buffer-text (entry-buffer value-field)) value)
			      (connect delete-button "clicked"
				       (lambda (button) (declare (ignore button))
					 (adw:preferences-group-remove group box)
					 (setf settings-fields
					       (delete (list name-field value-field) settings-fields
						       :test 'equal))))
			      (box-append box name-field)
			      (box-append box value-field)
			      (box-append box delete-button)
			      (adw:preferences-group-add group box)))))
		(setf (window-child settings-window) page)
		page)))
           (settings-page
            (funcall make-settings-page))
           (settings-actions
            (make-box :orientation +orientation-horizontal+
                      :spacing 10)
            (box-append settings-box settings-actions))
	   (settings-revert
	    (make-button :label "Revert")
	    (setf (widget-hexpand-p settings-revert) t
                  (widget-css-classes settings-revert) '("destructive-action" "pill"))
	    (connect settings-revert "clicked"
		     (lambda (button) (declare (ignore button))
		       (setf settings-page (funcall make-settings-page))))
	    (box-append settings-actions settings-revert))
           (settings-add
            (make-button :label "Add Field")
            (setf (widget-hexpand-p settings-add) t
                  (widget-css-classes settings-add) '("pill"))
            (connect settings-add "clicked"
                     (lambda (&rest args) (declare (ignore args))
                       (let ((group (adw:make-preferences-group))
                             (name-field (make-entry))
                             (value-field (make-entry))
                             (delete-button (make-button :icon-name "user-trash-symbolic"))
                             (box (make-box :orientation +orientation-horizontal+ :spacing 0)))
                         (setf settings-fields
                               (nconc settings-fields (list (list name-field value-field))))
                         (setf (widget-css-classes box) '("linked")
                               (widget-hexpand-p name-field) t
                               (widget-hexpand-p value-field) t
                               (entry-buffer-text (entry-buffer name-field)) "New"
                               (entry-buffer-text (entry-buffer value-field)) "new")
                         (connect delete-button "clicked"
                                  (lambda (button) (declare (ignore button))
                                    (adw:preferences-page-remove settings-page group)
                                    (setf settings-fields
                                          (delete (list name-field value-field) settings-fields
                                                  :test 'equal))))
                         (box-append box name-field)
                         (box-append box value-field)
                         (box-append box delete-button)
                         (adw:preferences-group-add group box)
                         (adw:preferences-page-add settings-page group))))
            (box-append settings-actions settings-add))
           (settings-saved-toast
            (adw:make-toast :title "Settings Saved")
            (setf (adw:toast-timeout settings-saved-toast) 3))
           (settings-save
            (make-button :label "Save")
            (setf (widget-hexpand-p settings-save) t
                  (widget-css-classes settings-save) '("suggested-action" "pill"))
            (connect settings-save "clicked"
                     (lambda (&rest args) (declare (ignore args))
                       (setf conf
                             (nconc
                              (loop for (name-field value-field) in settings-fields
                                    when (> (length (entry-buffer-text (entry-buffer name-field))) 0)
                                      collect (intern (string-upcase
                                                       (entry-buffer-text (entry-buffer name-field)))
                                                      "KEYWORD")
                                      and collect (entry-buffer-text (entry-buffer value-field)))
                              (loop for (name value) on conf by 'cddr
                                    unless (stringp value)
                                      collect name and collect value)))
                       (let ((*print-case* :downcase))
                         (with-open-file (out (merge-pathnames "config.sexp" *blog-root*)
                                              :direction :output
                                              :if-exists :supersede
					      :if-does-not-exist :create)
                           (format out "(~{~&~I~:W ~:W~}~&)" conf)))
                       (funcall set-config-fields)
                       (adw:toast-overlay-add-toast toast-overlay settings-saved-toast)))
            (box-append settings-actions settings-save))

           ;; Functions
	   (new-blog
	    nil
	    (setf new-blog
		  (lambda (&rest args) (declare (ignore args))
		    (let ((dialog (make-file-dialog)))
		      (setf (file-dialog-title dialog) "Set root directory of the new blog"
			    (file-dialog-initial-folder dialog)
			    (gio:file-new-for-path (namestring (user-homedir-pathname))))
		      (file-dialog-select-folder
		       dialog window (gio:make-cancellable)
		       (cffi:get-callback
			(cffi:defcallback new-blog-callback
			    :void ((obj :pointer) (res :pointer) (data :pointer))
			  (declare (ignore obj data))
			  (alexandria:when-let
			      ((root (ignore-errors
				      (truename
				       (gio:file-path (file-dialog-select-folder-finish dialog res))))))
			    (set-blog-root-variable root)
			    (let ((holding (adw:make-message-dialog
					    :parent window
					    :heading "Fetching default contents..."
					    :body nil)))
			      (adw:message-dialog-add-response holding "continue" "Continue")
			      (funcall #'(setf adw:message-dialog-response-enabled)
				       (list "continue" nil) holding)
			      (window-present holding)
			      (bt:make-thread
			       (lambda ()
				 (loop for p in '("posts/" "pages/" "resources/" "templates/") do
				   (ensure-directories-exist (merge-pathnames p *blog-root*)))
				 (loop for f in '("base.html" "home.html" "tags.html"
						  "page.html" "post.html" "postblock.html"
						  "page.md" "post.md")
				       unless (probe-file (merge-pathnames (format nil "templates/~A" f)
									   *blog-root*))
					 do (dex:fetch
					     (format nil "https://apr3vau.github.io/resources/templates/~A" f)
					     (merge-pathnames (format nil "templates/~A" f) *blog-root*)))
				 (unless (probe-file (merge-pathnames "config.sexp" *blog-root*))
				   (with-open-file (out (merge-pathnames "config.sexp" *blog-root*)
							:direction :output
							:if-does-not-exist :create)
				     (let ((*print-case* :downcase))
				       (format out "(~{~&~I~:W ~:W~}~&)" *initial-configs*))))
				 
				 (run-in-main-event-loop ()
				   (setf (adw:message-dialog-heading holding) "Blog created.")
				   (funcall #'(setf adw:message-dialog-response-enabled)
					    (list "continue" t) holding))))
			      (connect holding "response"
				       (lambda (obj res) (declare (ignore obj))
					 (when (string= res "continue")
					   (setf conf
						 (ignore-errors
						  (with-open-file (in (merge-pathnames "config.sexp" *blog-root*))
						    (read in))))
					   (funcall set-config-fields)
					   (setf (adw:view-stack-visible-child view-stack) settings-box))))))))
		       (cffi:null-pointer))))))
	   (set-blog-root
	    nil
	    (setf set-blog-root
		  (lambda (&rest args) (declare (ignore args))
		    (let ((dialog (make-file-dialog)))
		      (setf (file-dialog-initial-folder dialog)
			    (gio:file-new-for-path (namestring (user-homedir-pathname))))
		      (file-dialog-select-folder
		       dialog window (gio:make-cancellable)
		       (cffi:get-callback
			(cffi:defcallback set-blog-root-callback
			    :void ((obj :pointer) (res :pointer) (data :pointer))
			  (declare (ignore obj data))
			  (alexandria:when-let
			      ((gfile (ignore-errors
				       (file-dialog-select-folder-finish dialog res))))
			    (set-blog-root-variable (truename (gio:file-path gfile)))
			    (setf conf
				  (ignore-errors
				   (with-open-file (in (merge-pathnames "config.sexp" *blog-root*))
				     (read in))))
			    (funcall set-config-fields))))
		       (cffi:null-pointer))))))
           (set-config-fields
            (lambda (&rest args) (declare (ignore args))
              (if *blog-root*
		  (progn
		    (setf (adw:status-page-title status) (getf conf :blog-title "")
			  (adw:status-page-description status) (getf conf :blog-subtitle ""))
		    (alexandria:when-let ((logo (getf conf :logo)))
		      (let ((path (merge-pathnames
				   logo (merge-pathnames "resources/" *blog-root*))))
			(setf (adw:status-page-paintable status)
			      (when (probe-file path)
				(gdk:make-texture :path (namestring path))))))
		    (when (probe-file (merge-pathnames "posts/" *blog-root*))
		      (string-list-splice posts-model 0 (gio:list-model-n-items posts-model)
					  (mapcar 'namestring
						  (directory (merge-pathnames "posts/*.md" *blog-root*)))))
		    (when (probe-file (merge-pathnames "pages/" *blog-root*))
		      (string-list-splice pages-model 0 (gio:list-model-n-items pages-model)
					  (mapcar 'namestring
						  (directory (merge-pathnames "pages/*.md" *blog-root*)))))
		    (let ((manager (sourceview:make-style-scheme-manager)))
		      (setf (sourceview:buffer-style-scheme editor-buffer)
			    (sourceview:style-scheme-manager-get-scheme
			     manager
			     (find (getf conf :ui-editor-scheme "Adwaita")
				   (sourceview:style-scheme-manager-scheme-ids manager)
				   :test 'string-equal))))
		    (css-provider-load-from-data
		     editor-css-provider
		     (format nil "textview { font-family: ~A; font-size: ~A; }"
			     (getf conf :ui-editor-font "Serif")
			     (getf conf :ui-editor-font-size "12pt")))
		    (setf settings-page (funcall make-settings-page)))
		  (let ((dialog (adw:make-message-dialog
				 :parent window
				 :heading "Set a blog directory to manage"
				 :body "You haven't set a blog directory yet. Would you like to select a exist blog , or to create a new one?")))
		    (adw:message-dialog-add-response dialog "select" "Select")
		    (adw:message-dialog-add-response dialog "new" "New...")
		    (funcall #'(setf adw:message-dialog-response-appearance)
                             (list "new" adw:+response-appearance-suggested+) dialog)
		    (connect dialog "response"
			     (lambda (obj res) (declare (ignore obj))
			       (cond ((string= res "new")
				      (funcall new-blog))
				     ((string= res "select")
				      (funcall set-blog-root)))))
		    (window-present dialog))
		  ))
            (funcall set-config-fields))
           (save-editor-file-then
            (lambda (func)
              (flet ((save-and-exec ()
                       (with-open-file (out editor-file
                                            :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                         (write-string (text-buffer-text editor-buffer) out))
                       (setf editor-modified nil)
                       (funcall func)))
                (if editor-file
                    (save-and-exec)
                    (let ((dialog (make-file-dialog)))
		      (setf (file-dialog-initial-name dialog)
			    "NIL.md"
			    (file-dialog-initial-folder dialog)
			    (gio:file-new-for-path
			     (namestring (merge-pathnames #P"posts/" *blog-root*))))
		      (file-dialog-save
		       dialog window (gio:make-cancellable)
		       (cffi:get-callback
			(cffi:defcallback save-file-callback
			    :void ((obj :pointer) (res :pointer) (data :pointer))
			  (declare (ignore obj data))
			  (setf editor-file (ignore-errors (file-dialog-save-finish dialog res)))
			  (when editor-file (save-and-exec))))
		       (cffi:null-pointer)))))))

           (check-modified-then
            (lambda (func)
              (if editor-modified
                  (let ((dialog
                          (adw:make-message-dialog
                           :parent window
                           :heading (format nil "~A has been modified, Save it?" editor-file)
                           :body nil)))
                    (adw:message-dialog-add-response dialog "cancel" "Cancel")
                    (adw:message-dialog-add-response dialog "discard" "Discard")
                    (adw:message-dialog-add-response dialog "save" "Save")
                    (funcall #'(setf adw:message-dialog-response-appearance)
                             (list "discard" adw:+response-appearance-destructive+) dialog)
                    (funcall #'(setf adw:message-dialog-response-appearance)
                             (list "save" adw:+response-appearance-suggested+) dialog)
                    (connect
                     dialog "response"
                     (lambda (obj res) (declare (ignore obj))
                       (and (cond ((string= res "save")
                                   (funcall save-editor-file-then func))
                                  ((string= res "discard") t)
                                  (t nil))
                            (funcall func))))
                    (window-present dialog))
                  (funcall func))))

           (edit-post
            (lambda (&rest args) (declare (ignore args))
              (funcall
               check-modified-then
               (lambda ()
                 (alexandria:when-let*
                     ((name (adw:view-stack-visible-child-name posts-stack))
                      (index (bitset-get-nth
                              (selection-model-selection
                               (if (string= name "Posts") posts-selection-model pages-selection-model))
                              0))
                      (path (string-list-get-string (if (string= name "Posts") posts-model pages-model)
                                                    index)))
                   (setf (text-buffer-text editor-buffer) (uiop:read-file-string path)
                         editor-file path
                         editor-modified nil
                         (label-text modeline-pathname) (file-namestring path))
                   (setf (adw:view-stack-visible-child view-stack) write-box))))))
           (delete-post
            (lambda (&rest args) (declare (ignore args))
              (alexandria:when-let*
                  ((name (adw:view-stack-visible-child-name posts-stack))
                   (index (bitset-get-nth
                           (selection-model-selection
                            (if (string= name "Posts") posts-selection-model pages-selection-model))
                           0))
                   (path (string-list-get-string (if (string= name "Posts") posts-model pages-model)
                                                 index)))
                (let ((dialog (adw:make-message-dialog
                               :parent window
                               :heading "Are you sure to delete this file?"
                               :body "It will be erased from your disk permanently")))
                  (adw:message-dialog-add-response dialog "cancel" "Cancel")
                  (adw:message-dialog-add-response dialog "delete" "Delete")
                  (funcall #'(setf adw:message-dialog-response-appearance)
                           (list "cancel" adw:+response-appearance-destructive+) dialog)
                  (connect
                   dialog "response"
                   (lambda (obj res) (declare (ignore obj))
                     (when (string= res "delete")
                       (delete-file path)
                       (string-list-remove
                        (if (string= name "Posts") posts-model pages-model)
                        index))))
                  (window-present dialog)))))
           (new-post
            (lambda (&rest args) (declare (ignore args))
              (let* ((name (adw:view-stack-visible-child-name posts-stack))
                     (base-dir (merge-pathnames (if (string= name "Posts") "posts/" "pages/")
                                                *blog-root*)))
                (with-define-widgets
                    ((dialog
                      (adw:make-message-dialog
                       :parent window
                       :heading (format nil "Create New ~A"
                                        (if (string= name "Posts") "Post" "Page"))
                       :body nil)
                      (adw:message-dialog-add-response dialog "cancel" "Cancel")
                      (adw:message-dialog-add-response dialog "add" "Add")
                      (funcall #'(setf adw:message-dialog-response-enabled)
                               (list "add" nil) dialog)
                      (funcall #'(setf adw:message-dialog-response-appearance)
                               (list "add" adw:+response-appearance-suggested+) dialog)
                      (connect
                       dialog "response"
                       (lambda (obj res) (declare (ignore obj))
                         (when (string= res "add")
                           (let ((path (editable-text filename)))
                             (with-open-file (out path
                                                  :direction :output
                                                  :if-does-not-exist :create)
                               (djula:render-template*
                                (if (string= name "Posts") *post.md* *page.md*)
                                out
                                :title (editable-text title)
                                :date (multiple-value-bind (s m h) (get-decoded-time)
                                        (declare (ignore s))
                                        (local-time:format-timestring
                                         nil (local-time:encode-timestamp
                                              0 0 m h
                                              (floor (adw:spin-row-value date-day))
                                              (floor (adw:spin-row-value date-mon))
                                              (floor (adw:spin-row-value date-year)))
                                         :format '(:year #\- (:month 2) #\- (:day 2) #\T
                                                   (:hour 2) #\: (:min 2) #\: (:sec 2))))))
                             (string-list-append
                              (if (string= name "Posts") posts-model pages-model)
                              path)
                             (funcall
                              check-modified-then
                              (lambda ()
                                (setf (text-buffer-text editor-buffer) (uiop:read-file-string path)
                                      editor-file path
                                      editor-modified nil
                                      (label-text modeline-pathname) (file-namestring path))
                                (setf (adw:view-stack-visible-child view-stack) write-box))))))))
                     (dialog-box
                      (make-box :orientation +orientation-vertical+
                                :spacing 10)
                      (setf (adw:message-dialog-extra-child dialog) dialog-box))
                     (title
                      (adw:make-entry-row)
                      (setf (adw:preferences-row-title title) "Title")
                      (connect title "changed"
                               (lambda (obj) (declare (ignore obj))
                                 (let ((str (ppcre:regex-replace-all "\\s" (editable-text title) "-")))
                                   (when (string= str "")
                                     (setf str "NIL"))
                                   (setf (editable-text filename)
                                         (namestring
                                          (make-pathname
                                           :name str :type "md" :defaults base-dir))))
                                 (funcall check-valid)))
                      (box-append dialog-box title))
                     (date-box
                      (make-box :orientation +orientation-horizontal+
                                :spacing 10)
                      (box-append dialog-box date-box))
                     (date-day
                      (adw:make-spin-row :min 1d0 :max 31d0 :step 1d0 :digits 2)
                      (setf (adw:spin-row-snap-to-ticks-p date-day) t)
                      (connect date-day "changed" check-valid)
                      (box-append date-box date-day))
                     (date-mon
                      (adw:make-spin-row :min 1d0 :max 12d0 :step 1d0 :digits 2)
                      (setf (adw:spin-row-snap-to-ticks-p date-mon) t)
                      (connect date-mon "changed" check-valid)
                      (box-append date-box date-mon))
                     (date-year
                      (adw:make-spin-row :min 1d0 :max 9999d0 :step 1d0 :digits 4)
                      (setf (adw:spin-row-snap-to-ticks-p date-year) t)
                      (connect date-year "changed" check-valid)
                      (box-append date-box date-year)

                      (multiple-value-bind (s min h d m y)
                          (get-decoded-time) (declare (ignore s min h))
                        (setf (adw:spin-row-value date-day) (float d 0d0)
                              (adw:spin-row-value date-mon) (float m 0d0)
                              (adw:spin-row-value date-year) (float y 0d0))))

                     (filename
                      (adw:make-entry-row)
                      (setf (adw:preferences-row-title filename) "File Name")
                      (connect filename "changed" check-valid)
                      (box-append dialog-box filename))

                     (validp-label
                      (make-label :str "")
                      (setf (widget-css-classes validp-label) '("error"))
                      (box-append dialog-box validp-label))
                     (check-valid
                      (lambda (&rest args) (declare (ignore args))
                        (let ((okp t)
                              (msg "")
                              (namestr (editable-text filename)))
                          (when (probe-file namestr)
                            (setf okp nil
                                  msg "Invalid file, Please choose a different file name"))
                          (handler-case
                              (multiple-value-bind (s m h) (get-decoded-time)
                                (declare (ignore s))
                                (local-time:encode-timestamp
                                 0 0 m h
                                 (floor (adw:spin-row-value date-day))
                                 (floor (adw:spin-row-value date-mon))
                                 (floor (adw:spin-row-value date-year))))
                            (local-time::invalid-time-specification
                              (e) (declare (ignore e))
                              (setf okp nil
                                    msg (format nil "~A~&Invalid Date" msg))))
                          (if okp
                              (progn (setf (label-text validp-label) nil)
                                     (funcall #'(setf adw:message-dialog-response-enabled)
                                              (list "add" t) dialog))
                              (progn (funcall #'(setf adw:message-dialog-response-enabled)
                                              (list "add" nil) dialog)
                                     (setf (label-text validp-label) msg)))))))
                  (window-present dialog)))))
           )
        (unless (widget-visible-p window)
          (window-present window))
        window))))

(defun main ()
  (unless (gir:repository-get-search-path)
    (if (ignore-errors (truename "../lib/girepository-1.0/"))
	(gir:repository-prepend-search-path
	 (namestring (truename "../lib/girepository-1.0/")))
	(progn (log4cl:log-error "Cannot find typelib! Exit...")
	       (return-from main))))
  (setq *global-config-path*
	#+win32 (merge-pathnames "AppData/Local/aprblog.sexp"
				 (user-homedir-pathname))
	#-win32 (merge-pathnames "~/.config/aprblog.sexp"
				 (user-homedir-pathname)))
  (ensure-directories-exist *global-config-path*)
  (setq *global-config*
	(when (probe-file *global-config-path*)
	  (ignore-errors
	   (read-from-string (uiop:read-file-string *global-config-path*)))))
  
  (alexandria:when-let ((path (getf *global-config* :blog-root)))
    (set-blog-root-variable path))
  
  (float-features:with-float-traps-masked t
    (unless (adw:initialized-p)
      (adw:init))
    (funcall 'app)))

;; (loop for d in (gir:repository-get-search-path)
;;       nconc (directory (merge-pathnames "*.*" (truename d))) into r
;;       finally (loop for i in r
;; 		    for name = (merge-pathnames (file-namestring i)
;; 						(truename "lib/girepository-1.0/"))
;; 		    unless (probe-file name)
;; 		      do (uiop:copy-file i name)))
;; (generate)
;; (aprblog:main)
