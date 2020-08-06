;; These options just make it easier to use the file independently
;; in a way that is consistent with how it is used via Haskell
(set-option :print-success true)
(set-option :produce-models true)
(set-option :produce-unsat-cores true)

(set-logic QF_AUFLIA)

(declare-sort Bytes 0)
(declare-fun bytes0 () Bytes)
(declare-fun bytes-literal (Int) Bytes)
(define-fun toBytes_Bytes ((b Bytes)) Bytes
  b)
(declare-fun msg-cat (Bytes Bytes) Bytes)
(declare-fun digest (Bytes) Int)

(declare-sort Null 0)
(declare-fun null () Null)
(declare-fun toBytes_Null (Null) Bytes)

(declare-fun toBytes_Bool (Bool) Bytes)

(declare-fun toBytes_Int (Int) Bytes)

(declare-sort Address 0)
(declare-fun toBytes_Address (Address) Bytes)
