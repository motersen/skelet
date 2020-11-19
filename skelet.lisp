(defpackage :skelet
  (:use :cl
        :uiop)
  (:export :leaf-directories
           :remap-subpath
           :remap-empty-directory-trees
           :main))

(in-package :skelet)

(defun leaf-directories (paths)
  (if (null paths)
      nil
      (let ((subdirs (subdirectories (car paths))))
        (if (null subdirs)
            ;; leaf-directory found, collect
            (cons (car paths)
                  (leaf-directories (cdr paths)))
            (leaf-directories (append subdirs
                                      (cdr paths)))))))

(defun remap-subpath (maybe-subpath base-path new-base)
  (let ((subpath (subpathp maybe-subpath base-path)))
    (and subpath
         (merge-pathnames subpath
                          new-base))))

(defun remap-empty-directory-trees (new-base &rest base-paths)
  (let ((trees (leaf-directories (mapcar #'ensure-directory-pathname
                                         base-paths))))
    (mapcar (lambda (dir base-path)
              (remap-subpath dir
                             (truename* (pathname-directory-pathname base-path))
                             (ensure-directory-pathname new-base)))
            trees
            base-paths)))

(defun main ()
  (let ((cmdargs (raw-command-line-arguments)))
    (if (< (length cmdargs) 3)
        (error "Not enough arguments"))
    (let* ((base-paths (mapcar #'pathname (butlast (rest cmdargs))))
           (new-base (pathname (car (last cmdargs))))
           (new-dirs (apply #'remap-empty-directory-trees new-base base-paths)))
      (mapc #'ensure-directories-exist new-dirs))))
