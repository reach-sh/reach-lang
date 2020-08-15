;; These options just make it easier to use the file independently
;; in a way that is consistent with how it is used via Haskell
(set-option :print-success true)
(set-option :produce-models true)
(set-option :produce-unsat-cores true)

(set-logic ALL)

(define-sort UInt256 () Int)

(declare-sort Bytes 0)
(declare-fun bytes0 () Bytes)
(declare-fun bytes (Int) Bytes)
(define-fun Bytes_toBytes ((b Bytes)) Bytes
  b)
(declare-fun bytesAppend (Bytes Bytes) Bytes)
(declare-fun digest (Bytes) UInt256)

(declare-sort Null 0)
(declare-fun null () Null)
(declare-fun Null_toBytes (Null) Bytes)

(declare-fun Bool_toBytes (Bool) Bytes)

(declare-fun UInt256_toBytes (UInt256) Bytes)

(declare-sort Address 0)
(declare-fun Address_toBytes (Address) Bytes)
