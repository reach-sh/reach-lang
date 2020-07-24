;; These options just make it easier to use the file independently
;; in a way that is consistent with how it is used via Haskell
(set-option :print-success true)
(set-option :produce-models true)
(set-option :produce-unsat-cores true)

(set-logic ALL)
(declare-sort Address 0)

(declare-datatypes ((Bytes 0))
 (((bytes0)
   (bytes-literal (digest Int))
   (toBytes_Int (toBytes_Int_x Int))
   (toBytes_Bool (toBytes_Bool_x Bool))
   (toBytes_Bytes (toBytes_Bytes_x Bytes))
   (msg-cat (msg-left Bytes) (msg-right Bytes)))))

;; (declare-fun digest (Bytes) Int)
