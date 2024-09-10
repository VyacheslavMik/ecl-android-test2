
(defsystem "prebuilt-asdf" :class asdf::prebuilt-system
        :lib #P"SYS:LIBASDF.A"
        :depends-on ("cmp")
        :components ((:compiled-file "prebuilt-asdf" :pathname #P"SYS:ASDF.FAS")))