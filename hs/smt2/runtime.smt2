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
;; (assert
;;  (forall ((x Int) (y Int))
;;          (=> (not (= x y))
;;              (not (= (bytes x) (bytes y))))))

(define-fun Bytes_toBytes ((b Bytes)) Bytes
  b)

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

(declare-sort Digest 0)
(declare-fun digest (Bytes) Digest)
(declare-fun Digest_toBytes (Digest) Bytes)

(declare-sort Null 0)
(declare-fun null () Null)
(define-fun Null_toBytes ((n Null)) Bytes
  bytes0)

(declare-fun Bool_toBytes (Bool) Bytes)
;; (assert
;;  (forall ((x Bool) (y Bool))
;;          (=> (not (= x y))
;;              (not (= (Bool_toBytes x) (Bool_toBytes y))))))

(declare-fun UInt256_toBytes (UInt256) Bytes)
;; (assert
;;  (forall ((x UInt256) (y UInt256))
;;          (=> (not (= x y))
;;              (not (= (UInt256_toBytes x) (UInt256_toBytes y))))))

(declare-sort Address 0)

(declare-fun Address_toBytes (Address) Bytes)
;; (assert
;;  (forall ((x Address) (y Address))
;;          (=> (not (= x y))
;;              (not (= (Address_toBytes x) (Address_toBytes y))))))
