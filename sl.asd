(defsystem :sl
  :version      "0.0.1"
  :description  "Wrapper for the lowest common denominator of Sly and Slime"
  :author       "Spenser Truex <web@spensertruex.com>"
  :serial       t
  :license      "GNU GPL v3"
  :components ((:file "package")
               (:file "utils")
               (:file "macrolayer")
               (:file "sl"))
  :weakly-depends-on (:slynk :swank)
  :depends-on (:alexandria))
