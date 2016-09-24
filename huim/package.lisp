;; Copyright (c) 2016 Joshua Miller

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:huim
  (:use #:cl)
  (:export :main-loop
           :shell-command
           :menu
           :key
	   :*dictionary-file-path*
           :*serial-port-path*
           )
  )
