(defpackage :swisseph)

(in-package :swisseph)

(asdf:defsystem artistic
  :name "cl-swisspeh"
  :version "0.1"
  :maintainer "JRH"
  :author "JRH"
  :license "MIT"
  :description "Common Lisp bindings for the Swiss Ephemeris astro library"
  :serial t
  :depends-on (:alexandria :cffi)
  :components ((:file "swisseph")))
