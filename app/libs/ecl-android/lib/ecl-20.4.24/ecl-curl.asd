
(defsystem "ecl-curl" :class asdf::prebuilt-system
        :lib #P"SYS:LIBECL-CURL.A"
        :depends-on NIL
        :components ((:compiled-file "ecl-curl" :pathname #P"SYS:ECL-CURL.FAS")))