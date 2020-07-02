
(defsystem "ecl-help" :class asdf::prebuilt-system
        :lib #P"SYS:LIBECL-HELP.A"
        :depends-on NIL
        :components ((:compiled-file "ecl-help" :pathname #P"SYS:ECL-HELP.FAS")))