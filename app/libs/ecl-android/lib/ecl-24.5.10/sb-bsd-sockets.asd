
(defsystem "sb-bsd-sockets" :class asdf::prebuilt-system
        :lib #P"SYS:LIBSB-BSD-SOCKETS.A"
        :depends-on ("sockets")
        :components ((:compiled-file "sb-bsd-sockets" :pathname #P"SYS:SB-BSD-SOCKETS.FAS")))