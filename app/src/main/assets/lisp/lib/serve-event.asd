
(defsystem "serve-event" :class asdf::prebuilt-system
        :lib #P"SYS:LIBSERVE-EVENT.A"
        :depends-on NIL
        :components ((:compiled-file "serve-event" :pathname #P"SYS:SERVE-EVENT.FAS")))