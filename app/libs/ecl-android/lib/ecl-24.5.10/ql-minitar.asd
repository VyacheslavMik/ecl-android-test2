
(defsystem "ql-minitar" :class asdf::prebuilt-system
        :lib #P"SYS:LIBQL-MINITAR.A"
        :depends-on NIL
        :components ((:compiled-file "ql-minitar" :pathname #P"SYS:QL-MINITAR.FAS")))