(use-package (find-package 'cffi))

(defun print-pointer ()
  (let* ((env (mem-ref *env* :pointer))
	 (get-version (mem-ref (inc-pointer env 32) :pointer)))
    (list *env* env get-version)))

(defun prepare ()
  (let* ((jvm (mem-ref *jvm* :pointer))
	 (attach-thread (mem-ref (inc-pointer jvm 32) :pointer)))
    (list *jvm* attach-thread)))

(defun get-env-fn ()
  (let* ((jvm (mem-ref *jvm* :pointer))
	 (get-env (mem-ref (inc-pointer jvm 48) :pointer)))
    (list *jvm* get-env)))

(defparameter *env* (cffi:foreign-alloc :pointer))

(defun pointer-mem-ref (pointer)
  (cffi:mem-ref pointer :pointer))

(defun get-env ()
  (let ((env (cffi:foreign-alloc :pointer)))
    (setf (cffi:mem-ref env :pointer) (cffi:foreign-alloc :pointer))
    (destructuring-bind (jvm get-env-fn) (get-env-fn)
      (values
       env
       (cffi:foreign-funcall-pointer get-env-fn () :pointer jvm :pointer env :int #x00010006 :int)))))

(defun get-version2 ()
  (let ((env (cffi:foreign-alloc :pointer)))
    (setf (cffi:mem-ref env :pointer) (cffi:foreign-alloc :pointer))
    (cffi:with-foreign-object (args '(:struct jvm-attach-args))
      (cffi:with-foreign-slots ((version) args (:struct jvm-attach-args))
	(setf version #x00010006))
      (cffi:foreign-funcall-pointer
       (cffi:foreign-slot-value (cffi:mem-ref *jvm* :pointer)
				'(:struct jni-invoke-interface)
				'attach-current-thread)
       () 
       :pointer *jvm*
       :pointer env
       :pointer args
       :int)

      (cffi:foreign-funcall-pointer
       (cffi:foreign-slot-value (cffi:mem-ref
				 (cffi:mem-ref env :pointer)
				 :pointer)
				'(:struct jni-native-interface)
				'get-version)
       () 
       :pointer (cffi:mem-ref env :pointer)
       :int))))

(defun get-version3 ()
  (cffi:with-foreign-object (env :pointer)
    (cffi:with-foreign-object (p :pointer)
      (cffi:with-foreign-object (args '(:struct jvm-attach-args))
	(cffi:with-foreign-slots ((version) args (:struct jvm-attach-args))
	  (setf version #x00010006))

	(cffi:foreign-funcall-pointer
	 (cffi:foreign-slot-value (cffi:mem-ref *jvm* :pointer)
				  '(:struct jni-invoke-interface)
				  'attach-current-thread)
	 () 
	 :pointer *jvm*
	 :pointer env
	 :pointer args
	 :int)

	(cffi:foreign-funcall-pointer
	 (cffi:foreign-slot-value (cffi:mem-ref
				   (cffi:mem-ref env :pointer)
				   :pointer)
				  '(:struct jni-native-interface)
				  'get-version)
	 () 
	 :pointer (cffi:mem-ref env :pointer)
	 :int)))))

;; Need to detach thread after work
(defun attach-current-thread ()
  (let ((env (cffi:foreign-alloc :pointer)))
    (setf (cffi:mem-ref env :pointer) (cffi:foreign-alloc :pointer))
    (print env)
    (print (pointer-mem-ref env))
    (destructuring-bind (jvm attach-thread) (prepare)
      (cffi:with-foreign-object (args '(:struct jvm-attach-args))
	(cffi:with-foreign-slots ((version) args (:struct jvm-attach-args))
	  (setf version #x00010006))
	(values
	 env
	 (foreign-funcall-pointer attach-thread () :pointer jvm :pointer env :pointer args :int))))))

(defun get-version (env)
  (let* ((env         (pointer-mem-ref env))
	 (env%        (pointer-mem-ref env))
	 (get-version (mem-ref (inc-pointer env% 32) :pointer)))
    (cffi:foreign-funcall-pointer get-version () :pointer env :int)))

(defun foo ()
  (let ((env (attach-current-thread)))
    (get-version env)))


(defmacro with-attached-thread ((env-var) &body body)
  (let ((env  (gensym "env"))
	(p    (gensym "p"))
	(args (gensym "args"))
	(res  (gensym "res"))
	(ex   (gensym "ex")))
    `(cffi:with-foreign-object (,env :pointer)
       (cffi:with-foreign-object (,p :pointer)
	 (setf (cffi:mem-ref ,env :pointer) ,p)
	 (cffi:with-foreign-object (,args '(:struct jvm-attach-args))
	   (cffi:with-foreign-slots ((version) ,args (:struct jvm-attach-args))
	     (setf version #x00010006))

	   (cffi:foreign-funcall-pointer
	    (cffi:foreign-slot-value (cffi:mem-ref *jvm* :pointer)
				     '(:struct jni-invoke-interface)
				     'AttachCurrentThread)
	    () 
	    :pointer *jvm*
	    :pointer ,env
	    :pointer ,args
	    :int)

	   (let* ((,env-var ,env)
		  (,res     (progn ,@body))
		  (,ex      (jni/exception-clear ,env)))

	     (cffi:foreign-funcall-pointer
	      (cffi:foreign-slot-value (cffi:mem-ref *jvm* :pointer)
				       '(:struct jni-invoke-interface)
				       'DetachCurrentThread)
	      () 
	      :pointer *jvm*
	      :int)

	     (values ,res ,ex)))))))


(defun jni/find-class (full-class-name)
  (with-attached-thread (env)
    (cffi:with-foreign-string (fstr full-class-name)
      (cffi:foreign-funcall-pointer
       (cffi:foreign-slot-value (cffi:mem-ref
				 (cffi:mem-ref env :pointer)
				 :pointer)
				'(:struct jni-native-interface)
				'FindClass)
       () 
       :pointer (cffi:mem-ref env :pointer)
       :pointer fstr
       :pointer))))

(defun jni/find-class2 (full-class-name)
  (with-attached-thread (env)
    (cffi:foreign-funcall-pointer
     (cffi:foreign-slot-value (cffi:mem-ref
			       (cffi:mem-ref env :pointer)
			       :pointer)
			      '(:struct jni-native-interface)
			      'FindClass)
     () 
     :pointer (cffi:mem-ref env :pointer)
     :string full-class-name
     :pointer)))

(cffi:defcfun "findClass" :pointer
  (env :pointer)
  (name :string))

(cffi:defcfun "findClass" :pointer
  (name :pointer))

(with-attached-thread (env)
  (let* ((c (jni/find-class env "java/lang/ClassLoader"))
	 (m (jni/get-method-id env c "findClass" "(Ljava/lang/String;)Ljava/lang/Class;")))
    (cffi:with-foreign-string (s "com/example/ecl_android_test/HelloEclActivity")
      (print (jni/call-object-method env *class-loader* m s)))))

(with-attached-thread (env)
  (print (cffi:foreign-funcall-pointer *find-class-ptr* () 
				       :pointer (cffi:mem-ref env :pointer)
				       :string "com/example/ecl_android_test/HelloEclActivity"
				       :pointer)))

(with-attached-thread (env)
  (let* ((class (get-hello-ecl-activity-class env))
	 (activity (get-hello-ecl-activity env class))
	 (text-view (find-view-by-id env class activity "sample_text"))
	 (run-on-ui-thread (jni/get-method-id env class "runOnUiThread" "(Ljava/lang/Runnable;)V"))
	 (callback (make-callback env 7)))
    (jni/call-void-method env activity run-on-ui-thread callback)))
