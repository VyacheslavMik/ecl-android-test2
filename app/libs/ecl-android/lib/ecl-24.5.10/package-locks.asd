
(defsystem "package-locks" :class asdf::prebuilt-system
        :lib #P"SYS:LIBPACKAGE-LOCKS.A"
        :depends-on NIL
        :components ((:compiled-file "package-locks" :pathname #P"SYS:PACKAGE-LOCKS.FAS")))