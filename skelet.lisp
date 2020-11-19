(defpackage :skelet
  (:use :cl
        :uiop)
  (:export :leaf-directories
           :remap-subpath
           :remap-empty-directory-tree
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

(defun remap-empty-directory-tree (base-path new-base)
  (let ((tree (leaf-directories (list (ensure-directory-pathname base-path)))))
    (mapcar (lambda (dir)
              (remap-subpath dir
                             (truename* (pathname-directory-pathname base-path))
                             (ensure-directory-pathname new-base)))
            tree)))

(defun main ()
  (let ((cmdargs (raw-command-line-arguments)))
    (if (< (length cmdargs) 3)
        (error "Not enough arguments"))
    (let* ((base-path (pathname (second cmdargs)))
           (new-base (pathname (third cmdargs)))
           (new-dirs (remap-empty-directory-tree base-path new-base)))
      (mapc #'ensure-directories-exist new-dirs))))
