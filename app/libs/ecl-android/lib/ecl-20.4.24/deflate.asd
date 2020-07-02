
(defsystem "deflate" :class asdf::prebuilt-system
        :lib #P"SYS:LIBDEFLATE.A"
        :depends-on NIL
        :components ((:compiled-file "deflate" :pathname #P"SYS:DEFLATE.FAS")))