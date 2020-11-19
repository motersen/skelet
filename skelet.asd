(defsystem "skelet"
    :description "duplicate hollow directory tree husks"
    :depends-on (:asdf)
    :components ((:file "skelet"))
    :class "program-system"
    :build-operation "program-op"
    :build-pathname "bin/skelet"
    :entry-point "skelet:main")
