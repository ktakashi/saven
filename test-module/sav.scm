(saven
 (name "test-module")
 (version "1.0")
 (dependencies
  (github
   (name "ktakashi/r6rs-pffi")
   (release "master"))
  (github
   (name "ktakashi/r7rs-postgresql")
   (release "master")
   (scope "test")))
 (modules "baz" "foo" "bar"))
