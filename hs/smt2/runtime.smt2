;; These options just make it easier to use the file independently
;; in a way that is consistent with how it is used via Haskell

;(set-option :print-success true)
;(set-option :produce-models true)
;(set-option :produce-unsat-cores true)

(set-logic ALL)

(define-sort UInt () Int)
;; These basically say "anything can happen" which is better than an error
(declare-fun UInt_lsh (UInt UInt) UInt)
(declare-fun UInt_rsh (UInt UInt) UInt)
(declare-fun UInt_band (UInt UInt) UInt)
(declare-fun UInt_bior (UInt UInt) UInt)
(declare-fun UInt_bxor (UInt UInt) UInt)
(declare-fun UInt_sqrt (UInt) UInt)

(declare-sort Bytes 0)
(declare-fun bytes0 () Bytes)

(declare-fun bytes (Int) Bytes)
;; (assert
;;  (forall ((x Int) (y Int))
;;          (=> (not (= x y))
;;              (not (= (bytes x) (bytes y))))))

(define-fun Bytes_toBytes ((b Bytes)) Bytes
  b)
(declare-fun Bytes_xor (Bytes Bytes) Bytes)
(declare-fun btoiLast8 (Bytes) UInt)

(declare-fun bytesAppend (Bytes Bytes) Bytes)
;; (assert
;;  (forall ((b Bytes))
;;          (= b (bytesAppend bytes0 b))))
;; (assert
;;  (forall ((b Bytes))
;;          (= b (bytesAppend b bytes0))))
;; (assert
;;  (forall ((x Bytes) (y Bytes) (z Bytes))
;;          (= (bytesAppend x (bytesAppend y z))
;;             (bytesAppend (bytesAppend x y) z))))

(declare-sort BytesDyn 0)
(declare-fun BytesDyn_toBytes (BytesDyn) Bytes)

(declare-sort Digest 0)
(declare-fun digest (Bytes) Digest)
(declare-fun Digest_toBytes (Digest) Bytes)
(declare-fun Digest_xor (Digest Digest) Digest)
(declare-fun dtoiLast8 (Digest) UInt)

(declare-sort Null 0)
(declare-fun null () Null)
(define-fun Null_toBytes ((n Null)) Bytes
  bytes0)

(declare-fun Bool_toBytes (Bool) Bytes)
;; (assert
;;  (forall ((x Bool) (y Bool))
;;          (=> (not (= x y))
;;              (not (= (Bool_toBytes x) (Bool_toBytes y))))))

(declare-fun UInt_toBytes (UInt) Bytes)
;; (assert
;;  (forall ((x UInt) (y UInt))
;;          (=> (not (= x y))
;;              (not (= (UInt_toBytes x) (UInt_toBytes y))))))

(declare-sort Address 0)

(declare-fun Address_toBytes (Address) Bytes)
;; (assert
;;  (forall ((x Address) (y Address))
;;          (=> (not (= x y))
;;              (not (= (Address_toBytes x) (Address_toBytes y))))))

(declare-sort Token 0)

(declare-fun Token_toBytes (Token) Bytes)
;; (assert
;;  (forall ((x Token) (y Token))
;;          (=> (not (= x y))
;;              (not (= (Token_toBytes x) (Token_toBytes y))))))

(declare-sort Contract 0)
(declare-fun Contract_toBytes (Contract) Bytes)
(declare-fun Contract_addressEq (Contract Address) Bool)
