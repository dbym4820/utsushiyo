#|
  This file is a part of utsushiyo project.
  Copyright (c) 2018 Tomoki ABURATANI (aburatanitomoki@gmail.com)
|#

(defsystem "utsushiyo-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Tomoki ABURATANI"
  :license "MIT"
  :depends-on ("utsushiyo"
               "prove")
  :components ((:module "t"
                :components
                ((:test-file "utsushiyo"))))
  :description "Test system for utsushiyo"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
