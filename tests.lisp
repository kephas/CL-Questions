(define-test read-conversion
  (assert-equal -1 (funcall (make-read-converter 'integer "") "-1"))
  (assert-equal 12.8 (funcall (make-read-converter 'float "") "12.8")))

(define-test conversion-chain-basics
  (assert-equal "42" (funcall (make-conversion-chain nil) "42"))
  (assert-equal 42 (funcall (make-conversion-chain (list (make-read-converter 'integer ""))) "42")))
