;;; use-module.el --- provide use-module macro -*- lexical-binding: t; -*-

;;; Commentary:
;; Create `use-module' macro used by `require`.
;; the use of `use-module` is like simplified use-modules of schema.

;;; Code:

(defmacro defconst-if-not (var val &optional docstring)
  "If VAR isn't defined, VAR will be defined by VAL and DOCSTRING."
  (if (not (boundp var))
      `(defconst ,var ,val ,docstring)))

(defconst-if-not lisp-dir (expand-file-name "lisp" user-emacs-directory))
(defconst-if-not use-module-load-dir lisp-dir
  "The directory used by `use-module'.
It is the parent directory of module when the DIR of `use-module' is nil.
Its default value is the value of `lisp-dir'")
(defvar use-module-support-suffix '("el" "elc")
  "Define the suffixes that `use-module' supports.")

(defmacro use-module (target &optional dir not-parent)
  "Change the style of require like use-modules of schema.
The format of TARGET is (a b c), and to find the relative file \"a/b/c.el\"
or \"a/b/c.elc\" . The supported suffix in the `use-module-support-suffix' list.
DIR is the directory where file exists, which of default value is
the value of `use-module-load-dir'.
When target is (a b c), if NOT-PARENT is nil, require `a/b/c` feature,
otherwise require `c` feature."
  (or dir (setq dir use-module-load-dir))
  (pcase target
    (`(,_ . ,_)                         ;not empty list
     (letrec ((relative-file-name-without-suffix
               (mapconcat #'symbol-name target "/"))
              (feature
               (if not-parent
                   (car (last target))
                 (intern relative-file-name-without-suffix)))
              (possible-relative-file-names
               (mapcar
                (lambda (suffix)
                  (concat relative-file-name-without-suffix "." suffix))
                use-module-support-suffix))
              (possible-absolute-paths
               (mapcar
                (lambda (relative-file-name)
                  (expand-file-name relative-file-name dir))
                possible-relative-file-names))
              (existed-absolute-paths
               (seq-filter
                #'file-exists-p possible-absolute-paths)))
       (if existed-absolute-paths
           `(require ',feature ,(car existed-absolute-paths))
         `(display-warning
           'use-module
           (format "Fail to load `%S` feature, because no any files: '%S'\n"
                   ',feature
                   ',possible-absolute-paths)))))
    (_
     `(display-warning
       'use-module
       (format "`%S` isn't list" ',target)))))

(defmacro use-module/require (&rest args)
  "Declare needful other modules.
ARGS can be the four formats —— (:any a b c),(:one a b c), (:all a b c)
and (a b c).
A (:any a b c) represents that at least any one of a, b or c is required.
A (:one a b c) represents that the only one of a, b or c feature is required.
A (:all a b c) represents that a, b and c feature are all required.
A (a b c) is alias of (:all a b c)."
  (let ((get-existed-features
         (lambda (need-check-features)
           (seq-filter #'featurep need-check-features))))
    (pcase args
      (`(:any . ,rest)
       (if (funcall get-existed-features rest)
           `t
         `(display-warning
           'use-module/require
           (format "The %s needs one of '%S' at least.\n"
                   (file-current-filename)
                   ',rest))))
      (`(:one . ,rest)
       (if (length= (funcall get-existed-features rest) 1)
           `t
         `(display-warning
           'use-module/require
           (format "The %s needs and only needs one of %S\n"
                   (file-current-filename)
                   ',rest))))
      ((or `(:all . ,rest) rest)
       (let ((existed-featres (funcall get-existed-features rest)))
         (if (equal existed-featres rest)
             `t
           `(display-warning
             'use-module/require
             (format "The %s needs all '%S'"
                     (file-current-filename)
                     ',rest))))))))

(defmacro use-module/conflict (&rest args)
  "Declare conflicting module.
When conflicting module was loaded, report warning.
ARGS is a list of feature symbols."
  (if (listp args)
      (let ((existed-features
             (seq-filter #'featurep args)))
        (if existed-features
            `(display-warning
              'use-module/conflict
              (format "The %s conflicts with %S, and there can only be one."
                      (file-current-filename)
                      ',args))
          `t))
    `(display-warning
      'use-module/conflict
      "args isn't a list"
      :error)))

(defun file-current-filename ()
  "Return path to current file."
  (cond
   (load-in-progress load-file-name)
   ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
    byte-compile-current-file)
   (:else (buffer-file-name))))

(provide 'use-module)
;;; use-module.el ends here
