(require 'skelet)

(ext:install-c-compiler)
(setq c::*delete-files* nil)

(eval-when (:compile-toplevel)
  (declaim (optimize speed space (compilation-speed 0))))

(ensure-directories-exist (pathname "bin/"))

(asdf:make-build :skelet
                 :type :program
                 :move-here (make-pathname :directory '(:relative "bin"))
                 :epilogue-code '(progn (skelet:main)
                                  (si:exit)))
