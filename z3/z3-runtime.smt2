(declare-datatypes
 ()
 ((Bytes
   (bytes-literal (digest Int))
   (uint256->bytes (bytes-integer Int))
   (msg-cat (msg-left Bytes) (msg-right Bytes)))))

(declare-fun digest (Bytes) Int)

(define-fun-rec bytes-length ((bs Bytes)) Int
  (match bs
    (((uint256->bytes _) 32)
     ((bytes-literal _) 32)
     ((msg-cat l r) (+ 32 (bytes-length l) (bytes-length r))))))
