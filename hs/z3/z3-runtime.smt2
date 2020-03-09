(declare-sort Address 0)

(declare-datatypes
 ()
 ((Bytes
   (bytes0)
   (bytes-literal (digest Int))
   (toBytes_Int (toBytes_Int_x Int))
   (toBytes_Bool (toBytes_Bool_x Bool))
   (toBytes_Bytes (toBytes_Bytes_x Bytes))
   (msg-cat (msg-left Bytes) (msg-right Bytes)))))

(declare-fun digest (Bytes) Int)

(define-fun-rec bytes-length ((bs Bytes)) Int
  (match bs
    (((uint256->bytes _) 32)
     ((bytes-literal _) 32)
     ((msg-cat l r) (+ 32 (bytes-length l) (bytes-length r))))))
