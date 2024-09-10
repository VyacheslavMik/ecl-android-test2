(in-package :cl-user)
(format t "ECL (Embeddable Common-Lisp) ~A (git:~D)~%"
	(lisp-implementation-version)
	(ext:lisp-implementation-vcs-id))

(defvar *ecl-home* *default-pathname-defaults*)
(format t "LISP DIRECTORY: ~A" *ecl-home*)
(format t "LISP DIRECTORY2 : ~A" *default-pathname-defaults*)

(format t "ZDEC0")
(format t "Loading the modules~%")
(require '#:sockets)

(format t "ZDEC2")
(require '#:asdf)
(format t "ZDEC3")
(require '#:serve-event)
(format t "ZDEC4")

(setf asdf:*user-cache* (merge-pathnames #P"../cache/" *default-pathname-defaults*))

(pushnew (namestring *default-pathname-defaults*)
	 asdf:*central-registry*)

(when (probe-file #P"etc/user.lisp")
  (load "etc/user"))
