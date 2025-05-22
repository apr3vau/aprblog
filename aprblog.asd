(asdf:defsystem aprblog
  :author "April Simone"
  :depends-on (:djula :hunchentoot :cl-ppcre :local-time :cxml :quri :trivial-open-browser
                      :3bmd :3bmd-ext-code-blocks :3bmd-ext-tables
		      :cl-gtk4 :cl-gtk4.adw :cl-gdk4 :cl-gtk4.sourceview
		      :log4cl :float-features :dexador :split-sequence)
  :components ((:file "aprblog"))
  :defsystem-depends-on (:deploy)
  :build-operation "program-op"
  :build-pathname "bin/aprblog"
  :entry-point "aprblog:main")
