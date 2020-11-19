(push (uiop:pathname-directory-pathname *load-truename*)
      asdf:*central-registry*)

(asdf:make "skelet")
