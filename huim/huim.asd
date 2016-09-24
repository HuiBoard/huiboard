;; Copyright (c) 2016 Joshua Miller

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(asdf:defsystem #:huim
                :description "Huiboard - Hardware/Human/Humanoid/Hominid User/Universal Interface Manager"
                :author "Joshua Miller <huiboard@gmail.com>"
                :license "MPL 2.0"
                :depends-on (;;#:cl-cairo2
                             ;;#:lparallel
                             ;;#:bordeaux-threads
                             #:split-sequence
                             #:inferior-shell
                             #:cl-serial
                             )
                :serial t
                :components ((:file "package")
			     (:file "rosetta")
			     (:file "settings")
			     (:file "freqs")
			     (:file "huim")
			     (:file "menu")
			     ))
