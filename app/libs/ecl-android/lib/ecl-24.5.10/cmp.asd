
(defsystem "cmp" :class asdf::prebuilt-system
        :lib #P"SYS:LIBCMP.A"
        :depends-on NIL
        :components ((:compiled-file "cmp" :pathname #P"SYS:CMP.FAS")))