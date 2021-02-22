(defpackage :skelet
  (:use :cl
        :uiop)
  (:export :leaf-directories
           :remap-subpath
           :remap-empty-directory-trees
           :main))

(in-package :skelet)

(defun leaf-directories (paths)
  (cond
    ((null paths) nil)
    ((not (listp paths)) (leaf-directories (list paths)))
    (t (let ((subdirs (subdirectories (car paths))))
         (if (null subdirs)
             ;; leaf-directory found, collect
             (cons (car paths)
                   (leaf-directories (cdr paths)))
             (leaf-directories (append subdirs
                                       (cdr paths))))))))

(defun remap-subpath (maybe-subpath base-path new-base)
  (let ((subpath (subpathp maybe-subpath base-path)))
    (and subpath
         (merge-pathnames subpath
                          new-base))))

(defun remap-empty-directory-trees (new-base &rest base-paths)
  (let ((trees (mapcar #'leaf-directories
                       ;; ensure absolute pathname for subpath comparison
                       ;; and full directory pathname, don't drop last dir
                       ;; TODO HANDLE NONEXISTENT DIRS
                       (mapcar #'truename* base-paths)))
        (new-base (ensure-directory-pathname new-base))
        (base-paths (mapcar (lambda (path)
                              (truename*
                               (pathname-directory-pathname path)))
                            base-paths)))
    (flet ((remap-tree (tree base-path)
             (mapcar (lambda (leafdir)
                       (remap-subpath leafdir
                                      base-path
                                      new-base))
                     tree)))
      (apply #'append
             (mapcar #'remap-tree
                     trees
                     base-paths)))))

(defun main ()
  (let ((cmdargs (raw-command-line-arguments)))
    (if (< (length cmdargs) 3)
        (error "Not enough arguments"))
    (let* ((base-paths (mapcar #'pathname (butlast (rest cmdargs))))
           (new-base (pathname (car (last cmdargs))))
           (new-dirs (apply #'remap-empty-directory-trees new-base base-paths)))
      (mapc #'ensure-directories-exist new-dirs))))
