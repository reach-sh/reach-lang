[send->] (set-option :print-success true )
[<-recv] success
[send->] (set-option :produce-models true )
[<-recv] success
[send->] (set-option :produce-unsat-cores true )
[<-recv] success
[send->] (declare-datatypes () ((Bytes (bytes-literal (digest Int ) ) (uint256->bytes (bytes-integer Int ) ) (msg-cat (msg-left Bytes ) (msg-right Bytes ) ) ) ) )
[<-recv] success
[send->] (declare-fun digest (Bytes ) Int )
[<-recv] success
[send->] (define-fun-rec bytes-length ((bs Bytes ) ) Int (match bs (((uint256->bytes _ ) 32 ) ((bytes-literal _ ) 32 ) ((msg-cat l r ) (+ 32 (bytes-length l ) (bytes-length r ) ) ) ) ) )
[<-recv] success
[send->] (declare-fun v0 () Int )
[<-recv] success
[send->] (declare-fun v1 () Int )
[<-recv] success
[send->] (declare-fun v2 () Int )
[<-recv] success
[send->] (declare-fun v3 () Int )
[<-recv] success
[send->] (declare-fun v4 () Bytes )
[<-recv] success
[send->] (declare-fun v5 () Int )
[<-recv] success
[send->] (declare-fun v6 () Int )
[<-recv] success
[send->] (declare-fun v7 () Int )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (declare-fun v9 () Bytes )
[<-recv] success
[send->] (declare-fun v10 () Int )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (declare-fun v12 () Bytes )
[<-recv] success
[send->] (declare-fun v13 () Bool )
[<-recv] success
[send->] (declare-fun v14 () Bool )
[<-recv] success
[send->] (declare-fun v15 () Bool )
[<-recv] success
[send->] (declare-fun v16 () Bool )
[<-recv] success
[send->] (declare-fun v17 () Bool )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (declare-fun v20 () Bool )
[<-recv] success
[send->] (declare-fun v21 () Bool )
[<-recv] success
[send->] (declare-fun v22 () Bool )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (declare-fun v24 () Bytes )
[<-recv] success
[send->] (declare-fun v25 () Bytes )
[<-recv] success
[send->] (declare-fun v26 () Bytes )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (declare-fun v28 () Int )
[<-recv] success
[send->] (declare-fun v29 () Bytes )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (declare-fun v33 () Bool )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (declare-fun v35 () Bool )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (declare-fun v38 () Int )
[<-recv] success
[send->] (declare-fun v39 () Int )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (declare-fun v43 () Int )
[<-recv] success
[send->] (declare-fun v44 () Bytes )
[<-recv] success
[send->] (declare-fun v45 () Int )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (declare-fun v52 () Bytes )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (declare-fun v55 () Bytes )
[<-recv] success
[send->] (declare-fun v56 () Bytes )
[<-recv] success
[send->] (declare-fun v57 () Bytes )
[<-recv] success
[send->] (declare-fun v58 () Int )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (declare-fun v70 () Int )
[<-recv] success
[send->] (declare-fun v71 () Int )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (declare-fun v91 () Bool )
[<-recv] success
[send->] (declare-fun v92 () Bool )
[<-recv] success
[send->] (declare-fun v93 () Bool )
[<-recv] success
[send->] (declare-fun v94 () Bool )
[<-recv] success
[send->] (declare-fun v95 () Bool )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (declare-fun v97 () Bool )
[<-recv] success
[send->] (declare-fun v98 () Bool )
[<-recv] success
[send->] (declare-fun v99 () Bool )
[<-recv] success
[send->] (declare-fun v100 () Bool )
[<-recv] success
[send->] (declare-fun v101 () Bool )
[<-recv] success
[send->] (declare-fun v102 () Bool )
[<-recv] success
[send->] (declare-fun v103 () Bool )
[<-recv] success
[send->] (declare-fun v104 () Bool )
[<-recv] success
[send->] (declare-fun v105 () Bool )
[<-recv] success
[send->] (declare-fun v106 () Int )
[<-recv] success
[send->] (declare-fun v107 () Bool )
[<-recv] success
[send->] (declare-fun v108 () Int )
[<-recv] success
[send->] (declare-fun v109 () Int )
[<-recv] success
[send->] (declare-fun v110 () Int )
[<-recv] success
[send->] (declare-fun v111 () Int )
[<-recv] success
[send->] (declare-fun v112 () Int )
[<-recv] success
[send->] (declare-fun v113 () Int )
[<-recv] success
[send->] (declare-fun v114 () Bytes )
[<-recv] success
[send->] (declare-fun v115 () Bool )
[<-recv] success
[send->] (declare-fun v116 () Bool )
[<-recv] success
[send->] (declare-fun v117 () Bool )
[<-recv] success
[send->] (declare-fun v118 () Bool )
[<-recv] success
[send->] (declare-fun v119 () Bool )
[<-recv] success
[send->] (declare-fun v120 () Bytes )
[<-recv] success
[send->] (declare-fun v121 () Bytes )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v2 v0 ) )
[<-recv] success
[send->] (assert (= v3 v1 ) )
[<-recv] success
[send->] (assert (= v5 (+ v2 v3 ) ) )
[<-recv] success
[send->] (declare-fun txn_value1 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 txn_value1 ) )
[<-recv] success
[send->] (assert (= txn_value1 v5 ) )
[<-recv] success
[send->] (assert (= v6 txn_value1 ) )
[<-recv] success
[send->] (assert (= v7 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v8 (= v6 v7 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v8 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun txn_value2 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 txn_value2 ) )
[<-recv] success
[send->] (assert (= txn_value2 v2 ) )
[<-recv] success
[send->] (assert (= v10 txn_value2 ) )
[<-recv] success
[send->] (assert (= v11 (= v10 v2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v11 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v11 )
[<-recv] success
[send->] (assert (= v13 (= v12 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v14 (= v12 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v15 (= v12 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v16 (ite v13 true v14 ) ) )
[<-recv] success
[send->] (assert (= v17 (ite v16 true v15 ) ) )
[<-recv] success
[send->] (assert v17 )
[<-recv] success
[send->] (assert (= v18 (ite v14 1 2 ) ) )
[<-recv] success
[send->] (assert (= v19 (ite v13 0 v18 ) ) )
[<-recv] success
[send->] (assert (= v20 (<= 0 v19 ) ) )
[<-recv] success
[send->] (assert (= v21 (< v19 3 ) ) )
[<-recv] success
[send->] (assert (= v22 (ite v20 v21 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v22 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v22 )
[<-recv] success
[send->] (assert (= v24 (uint256->bytes v23 ) ) )
[<-recv] success
[send->] (assert (= v25 (uint256->bytes v19 ) ) )
[<-recv] success
[send->] (assert (= v26 (msg-cat v24 v25 ) ) )
[<-recv] success
[send->] (assert (= v27 (digest v26 ) ) )
[<-recv] success
[send->] (assert (= v28 v27 ) )
[<-recv] success
[send->] (declare-fun txn_value3 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 txn_value3 ) )
[<-recv] success
[send->] (assert (= txn_value3 0 ) )
[<-recv] success
[send->] (assert (= v30 txn_value3 ) )
[<-recv] success
[send->] (assert (= v31 (= v30 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v31 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (assert (= v33 (= v32 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v34 (= v32 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v35 (= v32 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v36 (ite v33 true v34 ) ) )
[<-recv] success
[send->] (assert (= v37 (ite v36 true v35 ) ) )
[<-recv] success
[send->] (assert v37 )
[<-recv] success
[send->] (assert (= v38 (ite v34 1 2 ) ) )
[<-recv] success
[send->] (assert (= v39 (ite v33 0 v38 ) ) )
[<-recv] success
[send->] (assert (= v40 (<= 0 v39 ) ) )
[<-recv] success
[send->] (assert (= v41 (< v39 3 ) ) )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v42 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (assert (= v43 v39 ) )
[<-recv] success
[send->] (declare-fun txn_value4 () Int )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (+ ctc_balance3 txn_value4 ) )
[<-recv] success
[send->] (assert (= txn_value4 0 ) )
[<-recv] success
[send->] (assert (= v45 txn_value4 ) )
[<-recv] success
[send->] (assert (= v46 (= v45 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v46 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v46 )
[<-recv] success
[send->] (assert (= v47 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v48 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v49 (ite v47 v48 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v49 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v49 )
[<-recv] success
[send->] (assert (= v50 v23 ) )
[<-recv] success
[send->] (assert (= v51 v19 ) )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (= txn_value5 0 ) )
[<-recv] success
[send->] (assert (= v53 txn_value5 ) )
[<-recv] success
[send->] (assert (= v54 (= v53 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v54 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v54 )
[<-recv] success
[send->] (assert (= v55 (uint256->bytes v50 ) ) )
[<-recv] success
[send->] (assert (= v56 (uint256->bytes v51 ) ) )
[<-recv] success
[send->] (assert (= v57 (msg-cat v55 v56 ) ) )
[<-recv] success
[send->] (assert (= v58 (digest v57 ) ) )
[<-recv] success
[send->] (assert (= v59 (= v28 v58 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v59 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v59 )
[<-recv] success
[send->] (assert (= v60 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v61 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v62 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v62 )
[<-recv] success
[send->] (assert (= v63 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v64 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v65 (ite v63 v64 false ) ) )
[<-recv] success
[send->] (assert (= v66 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v67 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v68 (ite v66 v67 false ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v65 v68 false ) ) )
[<-recv] success
[send->] (assert (= v70 (- 4 v43 ) ) )
[<-recv] success
[send->] (assert (= v71 (+ v51 v70 ) ) )
[<-recv] success
[send->] (assert (= v72 (mod v71 3 ) ) )
[<-recv] success
[send->] (assert (= v73 (ite v68 0 1 ) ) )
[<-recv] success
[send->] (assert (= v74 (ite v65 2 v73 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v69 v72 v74 ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v75 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v75 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v78 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (assert (= v79 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v80 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v81 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v83 true v82 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v84 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (assert (= v85 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v85 false true ) ) )
[<-recv] success
[send->] (assert (= v90 (ite v89 true v88 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v90 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v90 )
[<-recv] success
[send->] (assert (= v91 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v92 (= v51 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v92 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v93 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v94 (= v51 1 ) ) )
[<-recv] success
[send->] (assert (= v95 (ite v94 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v95 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v96 (= v51 2 ) ) )
[<-recv] success
[send->] (assert (= v97 (ite v96 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v97 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v98 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v99 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v101 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v102 (ite v101 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v102 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v103 (= v43 2 ) ) )
[<-recv] success
[send->] (assert (= v104 (ite v103 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v104 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v105 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v106 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v107 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v108 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v109 (ite v107 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v107 v108 v2 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v105 v106 v109 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v105 0 v110 ) ) )
[<-recv] success
[send->] (assert (= v113 (+ v3 v111 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v113 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v112 ) )
[<-recv] success
[send->] (assert (= v115 (<= 0 v75 ) ) )
[<-recv] success
[send->] (assert (= v116 (< v75 3 ) ) )
[<-recv] success
[send->] (assert (= v117 (ite v115 v116 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v117 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v117 )
[<-recv] success
[send->] (assert (= v118 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v119 (= v75 1 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v119 (bytes-literal 3502248497 ) (bytes-literal 1195058296 ) ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v118 (bytes-literal 1946528133 ) v120 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance7 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v2 v0 ) )
[<-recv] success
[send->] (assert (= v3 v1 ) )
[<-recv] success
[send->] (assert (= v5 (+ v2 v3 ) ) )
[<-recv] success
[send->] (declare-fun txn_value1 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 txn_value1 ) )
[<-recv] success
[send->] (assert (= txn_value1 v5 ) )
[<-recv] success
[send->] (assert (= v6 txn_value1 ) )
[<-recv] success
[send->] (assert (= v7 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v8 (= v6 v7 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v8 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun txn_value2 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 txn_value2 ) )
[<-recv] success
[send->] (assert (= txn_value2 v2 ) )
[<-recv] success
[send->] (assert (= v10 txn_value2 ) )
[<-recv] success
[send->] (assert (= v11 (= v10 v2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v11 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v11 )
[<-recv] success
[send->] (assert (= v13 (= v12 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v14 (= v12 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v15 (= v12 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v16 (ite v13 true v14 ) ) )
[<-recv] success
[send->] (assert (= v17 (ite v16 true v15 ) ) )
[<-recv] success
[send->] (assert v17 )
[<-recv] success
[send->] (assert (= v18 (ite v14 1 2 ) ) )
[<-recv] success
[send->] (assert (= v19 (ite v13 0 v18 ) ) )
[<-recv] success
[send->] (assert (= v20 (<= 0 v19 ) ) )
[<-recv] success
[send->] (assert (= v21 (< v19 3 ) ) )
[<-recv] success
[send->] (assert (= v22 (ite v20 v21 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v22 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v22 )
[<-recv] success
[send->] (assert (= v24 (uint256->bytes v23 ) ) )
[<-recv] success
[send->] (assert (= v25 (uint256->bytes v19 ) ) )
[<-recv] success
[send->] (assert (= v26 (msg-cat v24 v25 ) ) )
[<-recv] success
[send->] (assert (= v27 (digest v26 ) ) )
[<-recv] success
[send->] (assert (= v28 v27 ) )
[<-recv] success
[send->] (declare-fun txn_value3 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 txn_value3 ) )
[<-recv] success
[send->] (assert (= txn_value3 0 ) )
[<-recv] success
[send->] (assert (= v30 txn_value3 ) )
[<-recv] success
[send->] (assert (= v31 (= v30 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v31 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (assert (= v33 (= v32 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v34 (= v32 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v35 (= v32 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v36 (ite v33 true v34 ) ) )
[<-recv] success
[send->] (assert (= v37 (ite v36 true v35 ) ) )
[<-recv] success
[send->] (assert v37 )
[<-recv] success
[send->] (assert (= v38 (ite v34 1 2 ) ) )
[<-recv] success
[send->] (assert (= v39 (ite v33 0 v38 ) ) )
[<-recv] success
[send->] (assert (= v40 (<= 0 v39 ) ) )
[<-recv] success
[send->] (assert (= v41 (< v39 3 ) ) )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v42 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (assert (= v43 v39 ) )
[<-recv] success
[send->] (declare-fun txn_value4 () Int )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (+ ctc_balance3 txn_value4 ) )
[<-recv] success
[send->] (assert (= txn_value4 0 ) )
[<-recv] success
[send->] (assert (= v45 txn_value4 ) )
[<-recv] success
[send->] (assert (= v46 (= v45 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v46 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v46 )
[<-recv] success
[send->] (assert (= v47 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v48 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v49 (ite v47 v48 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v49 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v49 )
[<-recv] success
[send->] (assert (= v50 v23 ) )
[<-recv] success
[send->] (assert (= v51 v19 ) )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (= txn_value5 0 ) )
[<-recv] success
[send->] (assert (= v53 txn_value5 ) )
[<-recv] success
[send->] (assert (= v54 (= v53 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v54 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v54 )
[<-recv] success
[send->] (assert (= v55 (uint256->bytes v50 ) ) )
[<-recv] success
[send->] (assert (= v56 (uint256->bytes v51 ) ) )
[<-recv] success
[send->] (assert (= v57 (msg-cat v55 v56 ) ) )
[<-recv] success
[send->] (assert (= v58 (digest v57 ) ) )
[<-recv] success
[send->] (assert (= v59 (= v28 v58 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v59 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v59 )
[<-recv] success
[send->] (assert (= v60 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v61 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v62 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v62 )
[<-recv] success
[send->] (assert (= v63 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v64 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v65 (ite v63 v64 false ) ) )
[<-recv] success
[send->] (assert (= v66 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v67 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v68 (ite v66 v67 false ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v65 v68 false ) ) )
[<-recv] success
[send->] (assert (= v70 (- 4 v43 ) ) )
[<-recv] success
[send->] (assert (= v71 (+ v51 v70 ) ) )
[<-recv] success
[send->] (assert (= v72 (mod v71 3 ) ) )
[<-recv] success
[send->] (assert (= v73 (ite v68 0 1 ) ) )
[<-recv] success
[send->] (assert (= v74 (ite v65 2 v73 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v69 v72 v74 ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v75 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v75 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v78 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (assert (= v79 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v80 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v81 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v83 true v82 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v84 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (assert (= v85 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v85 false true ) ) )
[<-recv] success
[send->] (assert (= v90 (ite v89 true v88 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v90 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v90 )
[<-recv] success
[send->] (assert (= v91 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v92 (= v51 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v92 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v93 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v94 (= v51 1 ) ) )
[<-recv] success
[send->] (assert (= v95 (ite v94 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v95 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v96 (= v51 2 ) ) )
[<-recv] success
[send->] (assert (= v97 (ite v96 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v97 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v98 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v99 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v101 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v102 (ite v101 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v102 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v103 (= v43 2 ) ) )
[<-recv] success
[send->] (assert (= v104 (ite v103 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v104 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v105 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v106 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v107 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v108 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v109 (ite v107 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v107 v108 v2 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v105 v106 v109 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v105 0 v110 ) ) )
[<-recv] success
[send->] (assert (= v113 (+ v3 v111 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v113 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v112 ) )
[<-recv] success
[send->] (assert (= v115 (<= 0 v75 ) ) )
[<-recv] success
[send->] (assert (= v116 (< v75 3 ) ) )
[<-recv] success
[send->] (assert (= v117 (ite v115 v116 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v117 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v117 )
[<-recv] success
[send->] (assert (= v118 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v119 (= v75 1 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v119 (bytes-literal 3502248497 ) (bytes-literal 1195058296 ) ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v118 (bytes-literal 1946528133 ) v120 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance7 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v2 v0 ) )
[<-recv] success
[send->] (assert (= v3 v1 ) )
[<-recv] success
[send->] (assert (= v5 (+ v2 v3 ) ) )
[<-recv] success
[send->] (declare-fun txn_value1 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 txn_value1 ) )
[<-recv] success
[send->] (assert (= txn_value1 v5 ) )
[<-recv] success
[send->] (assert (= v6 txn_value1 ) )
[<-recv] success
[send->] (assert (= v7 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v8 (= v6 v7 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v8 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun txn_value2 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 txn_value2 ) )
[<-recv] success
[send->] (assert (= txn_value2 v2 ) )
[<-recv] success
[send->] (assert (= v10 txn_value2 ) )
[<-recv] success
[send->] (assert (= v11 (= v10 v2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v11 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v11 )
[<-recv] success
[send->] (assert (= v13 (= v12 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v14 (= v12 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v15 (= v12 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v16 (ite v13 true v14 ) ) )
[<-recv] success
[send->] (assert (= v17 (ite v16 true v15 ) ) )
[<-recv] success
[send->] (assert v17 )
[<-recv] success
[send->] (assert (= v18 (ite v14 1 2 ) ) )
[<-recv] success
[send->] (assert (= v19 (ite v13 0 v18 ) ) )
[<-recv] success
[send->] (assert (= v20 (<= 0 v19 ) ) )
[<-recv] success
[send->] (assert (= v21 (< v19 3 ) ) )
[<-recv] success
[send->] (assert (= v22 (ite v20 v21 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v22 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v22 )
[<-recv] success
[send->] (assert (= v24 (uint256->bytes v23 ) ) )
[<-recv] success
[send->] (assert (= v25 (uint256->bytes v19 ) ) )
[<-recv] success
[send->] (assert (= v26 (msg-cat v24 v25 ) ) )
[<-recv] success
[send->] (assert (= v27 (digest v26 ) ) )
[<-recv] success
[send->] (assert (= v28 v27 ) )
[<-recv] success
[send->] (declare-fun txn_value3 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 txn_value3 ) )
[<-recv] success
[send->] (assert (= txn_value3 0 ) )
[<-recv] success
[send->] (assert (= v30 txn_value3 ) )
[<-recv] success
[send->] (assert (= v31 (= v30 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v31 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (assert (= v33 (= v32 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v34 (= v32 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v35 (= v32 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v36 (ite v33 true v34 ) ) )
[<-recv] success
[send->] (assert (= v37 (ite v36 true v35 ) ) )
[<-recv] success
[send->] (assert v37 )
[<-recv] success
[send->] (assert (= v38 (ite v34 1 2 ) ) )
[<-recv] success
[send->] (assert (= v39 (ite v33 0 v38 ) ) )
[<-recv] success
[send->] (assert (= v40 (<= 0 v39 ) ) )
[<-recv] success
[send->] (assert (= v41 (< v39 3 ) ) )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v42 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (assert (= v43 v39 ) )
[<-recv] success
[send->] (declare-fun txn_value4 () Int )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (+ ctc_balance3 txn_value4 ) )
[<-recv] success
[send->] (assert (= txn_value4 0 ) )
[<-recv] success
[send->] (assert (= v45 txn_value4 ) )
[<-recv] success
[send->] (assert (= v46 (= v45 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v46 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v46 )
[<-recv] success
[send->] (assert (= v47 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v48 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v49 (ite v47 v48 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v49 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v49 )
[<-recv] success
[send->] (assert (= v50 v23 ) )
[<-recv] success
[send->] (assert (= v51 v19 ) )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (= txn_value5 0 ) )
[<-recv] success
[send->] (assert (= v53 txn_value5 ) )
[<-recv] success
[send->] (assert (= v54 (= v53 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v54 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v54 )
[<-recv] success
[send->] (assert (= v55 (uint256->bytes v50 ) ) )
[<-recv] success
[send->] (assert (= v56 (uint256->bytes v51 ) ) )
[<-recv] success
[send->] (assert (= v57 (msg-cat v55 v56 ) ) )
[<-recv] success
[send->] (assert (= v58 (digest v57 ) ) )
[<-recv] success
[send->] (assert (= v59 (= v28 v58 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v59 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v59 )
[<-recv] success
[send->] (assert (= v60 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v61 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v62 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v62 )
[<-recv] success
[send->] (assert (= v63 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v64 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v65 (ite v63 v64 false ) ) )
[<-recv] success
[send->] (assert (= v66 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v67 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v68 (ite v66 v67 false ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v65 v68 false ) ) )
[<-recv] success
[send->] (assert (= v70 (- 4 v43 ) ) )
[<-recv] success
[send->] (assert (= v71 (+ v51 v70 ) ) )
[<-recv] success
[send->] (assert (= v72 (mod v71 3 ) ) )
[<-recv] success
[send->] (assert (= v73 (ite v68 0 1 ) ) )
[<-recv] success
[send->] (assert (= v74 (ite v65 2 v73 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v69 v72 v74 ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v75 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v75 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v78 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (assert (= v79 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v80 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v81 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v83 true v82 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v84 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (assert (= v85 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v85 false true ) ) )
[<-recv] success
[send->] (assert (= v90 (ite v89 true v88 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v90 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v90 )
[<-recv] success
[send->] (assert (= v91 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v92 (= v51 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v92 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v93 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v94 (= v51 1 ) ) )
[<-recv] success
[send->] (assert (= v95 (ite v94 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v95 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v96 (= v51 2 ) ) )
[<-recv] success
[send->] (assert (= v97 (ite v96 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v97 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v98 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v99 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v101 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v102 (ite v101 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v102 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v103 (= v43 2 ) ) )
[<-recv] success
[send->] (assert (= v104 (ite v103 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v104 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v105 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v106 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v107 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v108 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v109 (ite v107 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v107 v108 v2 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v105 v106 v109 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v105 0 v110 ) ) )
[<-recv] success
[send->] (assert (= v113 (+ v3 v111 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v113 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v112 ) )
[<-recv] success
[send->] (assert (= v115 (<= 0 v75 ) ) )
[<-recv] success
[send->] (assert (= v116 (< v75 3 ) ) )
[<-recv] success
[send->] (assert (= v117 (ite v115 v116 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v117 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v117 )
[<-recv] success
[send->] (assert (= v118 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v119 (= v75 1 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v119 (bytes-literal 3502248497 ) (bytes-literal 1195058296 ) ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v118 (bytes-literal 1946528133 ) v120 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance7 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (declare-fun txn_value1 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 txn_value1 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value1 ) )
[<-recv] success
[send->] (assert (= v6 txn_value1 ) )
[<-recv] success
[send->] (assert (= v7 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v8 (= v6 v7 ) ) )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun txn_value2 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 txn_value2 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value2 ) )
[<-recv] success
[send->] (assert (= v10 txn_value2 ) )
[<-recv] success
[send->] (assert (= v11 (= v10 v2 ) ) )
[<-recv] success
[send->] (assert v11 )
[<-recv] success
[send->] (declare-fun txn_value3 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 txn_value3 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value3 ) )
[<-recv] success
[send->] (assert (= v30 txn_value3 ) )
[<-recv] success
[send->] (assert (= v31 (= v30 0 ) ) )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (declare-fun txn_value4 () Int )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (+ ctc_balance3 txn_value4 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value4 ) )
[<-recv] success
[send->] (assert (= v45 txn_value4 ) )
[<-recv] success
[send->] (assert (= v46 (= v45 0 ) ) )
[<-recv] success
[send->] (assert v46 )
[<-recv] success
[send->] (assert (= v47 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v48 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v49 (ite v47 v48 false ) ) )
[<-recv] success
[send->] (assert v49 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value5 ) )
[<-recv] success
[send->] (assert (= v53 txn_value5 ) )
[<-recv] success
[send->] (assert (= v54 (= v53 0 ) ) )
[<-recv] success
[send->] (assert v54 )
[<-recv] success
[send->] (assert (= v55 (uint256->bytes v50 ) ) )
[<-recv] success
[send->] (assert (= v56 (uint256->bytes v51 ) ) )
[<-recv] success
[send->] (assert (= v57 (msg-cat v55 v56 ) ) )
[<-recv] success
[send->] (assert (= v58 (digest v57 ) ) )
[<-recv] success
[send->] (assert (= v59 (= v28 v58 ) ) )
[<-recv] success
[send->] (assert v59 )
[<-recv] success
[send->] (assert (= v60 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v61 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (assert v62 )
[<-recv] success
[send->] (assert (= v63 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v64 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v65 (ite v63 v64 false ) ) )
[<-recv] success
[send->] (assert (= v66 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v67 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v68 (ite v66 v67 false ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v65 v68 false ) ) )
[<-recv] success
[send->] (assert (= v70 (- 4 v43 ) ) )
[<-recv] success
[send->] (assert (= v71 (+ v51 v70 ) ) )
[<-recv] success
[send->] (assert (= v72 (mod v71 3 ) ) )
[<-recv] success
[send->] (assert (= v73 (ite v68 0 1 ) ) )
[<-recv] success
[send->] (assert (= v74 (ite v65 2 v73 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v69 v72 v74 ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v75 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v75 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v78 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (assert (= v79 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v80 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v81 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v83 true v82 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v84 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (assert (= v85 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v85 false true ) ) )
[<-recv] success
[send->] (assert (= v90 (ite v89 true v88 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v90 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v90 )
[<-recv] success
[send->] (assert (= v91 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v92 (= v51 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v92 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v93 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v94 (= v51 1 ) ) )
[<-recv] success
[send->] (assert (= v95 (ite v94 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v95 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v96 (= v51 2 ) ) )
[<-recv] success
[send->] (assert (= v97 (ite v96 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v97 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v98 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v99 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v101 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v102 (ite v101 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v102 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v103 (= v43 2 ) ) )
[<-recv] success
[send->] (assert (= v104 (ite v103 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v104 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v105 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v106 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v107 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v108 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v109 (ite v107 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v107 v108 v2 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v105 v106 v109 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v105 0 v110 ) ) )
[<-recv] success
[send->] (assert (= v113 (+ v3 v111 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v113 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v112 ) )
[<-recv] success
[send->] (assert (= v115 (<= 0 v75 ) ) )
[<-recv] success
[send->] (assert (= v116 (< v75 3 ) ) )
[<-recv] success
[send->] (assert (= v117 (ite v115 v116 false ) ) )
[<-recv] success
[send->] (assert v117 )
[<-recv] success
[send->] (assert (= v118 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v119 (= v75 1 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v119 (bytes-literal 3502248497 ) (bytes-literal 1195058296 ) ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v118 (bytes-literal 1946528133 ) v120 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance7 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v2 v0 ) )
[<-recv] success
[send->] (assert (= v3 v1 ) )
[<-recv] success
[send->] (assert (= v5 (+ v2 v3 ) ) )
[<-recv] success
[send->] (declare-fun txn_value1 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 txn_value1 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value1 ) )
[<-recv] success
[send->] (assert (= v6 txn_value1 ) )
[<-recv] success
[send->] (assert (= v7 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v8 (= v6 v7 ) ) )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun txn_value2 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 txn_value2 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value2 ) )
[<-recv] success
[send->] (assert (= v10 txn_value2 ) )
[<-recv] success
[send->] (assert (= v11 (= v10 v2 ) ) )
[<-recv] success
[send->] (assert v11 )
[<-recv] success
[send->] (assert (= v13 (= v12 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v14 (= v12 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v15 (= v12 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v16 (ite v13 true v14 ) ) )
[<-recv] success
[send->] (assert (= v17 (ite v16 true v15 ) ) )
[<-recv] success
[send->] (assert v17 )
[<-recv] success
[send->] (assert (= v18 (ite v14 1 2 ) ) )
[<-recv] success
[send->] (assert (= v19 (ite v13 0 v18 ) ) )
[<-recv] success
[send->] (assert (= v20 (<= 0 v19 ) ) )
[<-recv] success
[send->] (assert (= v21 (< v19 3 ) ) )
[<-recv] success
[send->] (assert (= v22 (ite v20 v21 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v22 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v22 )
[<-recv] success
[send->] (assert (= v24 (uint256->bytes v23 ) ) )
[<-recv] success
[send->] (assert (= v25 (uint256->bytes v19 ) ) )
[<-recv] success
[send->] (assert (= v26 (msg-cat v24 v25 ) ) )
[<-recv] success
[send->] (assert (= v27 (digest v26 ) ) )
[<-recv] success
[send->] (assert (= v28 v27 ) )
[<-recv] success
[send->] (declare-fun txn_value3 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 txn_value3 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value3 ) )
[<-recv] success
[send->] (assert (= v30 txn_value3 ) )
[<-recv] success
[send->] (assert (= v31 (= v30 0 ) ) )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (declare-fun txn_value4 () Int )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (+ ctc_balance3 txn_value4 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value4 ) )
[<-recv] success
[send->] (assert (= v45 txn_value4 ) )
[<-recv] success
[send->] (assert (= v46 (= v45 0 ) ) )
[<-recv] success
[send->] (assert v46 )
[<-recv] success
[send->] (assert (= v47 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v48 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v49 (ite v47 v48 false ) ) )
[<-recv] success
[send->] (assert v49 )
[<-recv] success
[send->] (assert (= v50 v23 ) )
[<-recv] success
[send->] (assert (= v51 v19 ) )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value5 ) )
[<-recv] success
[send->] (assert (= v53 txn_value5 ) )
[<-recv] success
[send->] (assert (= v54 (= v53 0 ) ) )
[<-recv] success
[send->] (assert v54 )
[<-recv] success
[send->] (assert (= v55 (uint256->bytes v50 ) ) )
[<-recv] success
[send->] (assert (= v56 (uint256->bytes v51 ) ) )
[<-recv] success
[send->] (assert (= v57 (msg-cat v55 v56 ) ) )
[<-recv] success
[send->] (assert (= v58 (digest v57 ) ) )
[<-recv] success
[send->] (assert (= v59 (= v28 v58 ) ) )
[<-recv] success
[send->] (assert v59 )
[<-recv] success
[send->] (assert (= v60 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v61 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (assert v62 )
[<-recv] success
[send->] (assert (= v63 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v64 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v65 (ite v63 v64 false ) ) )
[<-recv] success
[send->] (assert (= v66 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v67 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v68 (ite v66 v67 false ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v65 v68 false ) ) )
[<-recv] success
[send->] (assert (= v70 (- 4 v43 ) ) )
[<-recv] success
[send->] (assert (= v71 (+ v51 v70 ) ) )
[<-recv] success
[send->] (assert (= v72 (mod v71 3 ) ) )
[<-recv] success
[send->] (assert (= v73 (ite v68 0 1 ) ) )
[<-recv] success
[send->] (assert (= v74 (ite v65 2 v73 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v69 v72 v74 ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v75 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v75 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v78 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (assert (= v79 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v80 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v81 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v83 true v82 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v84 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (assert (= v85 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v85 false true ) ) )
[<-recv] success
[send->] (assert (= v90 (ite v89 true v88 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v90 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v90 )
[<-recv] success
[send->] (assert (= v91 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v92 (= v51 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v92 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v93 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v94 (= v51 1 ) ) )
[<-recv] success
[send->] (assert (= v95 (ite v94 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v95 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v96 (= v51 2 ) ) )
[<-recv] success
[send->] (assert (= v97 (ite v96 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v97 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v98 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v99 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v101 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v102 (ite v101 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v102 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v103 (= v43 2 ) ) )
[<-recv] success
[send->] (assert (= v104 (ite v103 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v104 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v105 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v106 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v107 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v108 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v109 (ite v107 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v107 v108 v2 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v105 v106 v109 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v105 0 v110 ) ) )
[<-recv] success
[send->] (assert (= v113 (+ v3 v111 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v113 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v112 ) )
[<-recv] success
[send->] (assert (= v115 (<= 0 v75 ) ) )
[<-recv] success
[send->] (assert (= v116 (< v75 3 ) ) )
[<-recv] success
[send->] (assert (= v117 (ite v115 v116 false ) ) )
[<-recv] success
[send->] (assert v117 )
[<-recv] success
[send->] (assert (= v118 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v119 (= v75 1 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v119 (bytes-literal 3502248497 ) (bytes-literal 1195058296 ) ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v118 (bytes-literal 1946528133 ) v120 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance7 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (declare-fun txn_value1 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 txn_value1 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value1 ) )
[<-recv] success
[send->] (assert (= v6 txn_value1 ) )
[<-recv] success
[send->] (assert (= v7 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v8 (= v6 v7 ) ) )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun txn_value2 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 txn_value2 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value2 ) )
[<-recv] success
[send->] (assert (= v10 txn_value2 ) )
[<-recv] success
[send->] (assert (= v11 (= v10 v2 ) ) )
[<-recv] success
[send->] (assert v11 )
[<-recv] success
[send->] (declare-fun txn_value3 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 txn_value3 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value3 ) )
[<-recv] success
[send->] (assert (= v30 txn_value3 ) )
[<-recv] success
[send->] (assert (= v31 (= v30 0 ) ) )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (assert (= v33 (= v32 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v34 (= v32 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v35 (= v32 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v36 (ite v33 true v34 ) ) )
[<-recv] success
[send->] (assert (= v37 (ite v36 true v35 ) ) )
[<-recv] success
[send->] (assert v37 )
[<-recv] success
[send->] (assert (= v38 (ite v34 1 2 ) ) )
[<-recv] success
[send->] (assert (= v39 (ite v33 0 v38 ) ) )
[<-recv] success
[send->] (assert (= v40 (<= 0 v39 ) ) )
[<-recv] success
[send->] (assert (= v41 (< v39 3 ) ) )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v42 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (assert (= v43 v39 ) )
[<-recv] success
[send->] (declare-fun txn_value4 () Int )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (+ ctc_balance3 txn_value4 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value4 ) )
[<-recv] success
[send->] (assert (= v45 txn_value4 ) )
[<-recv] success
[send->] (assert (= v46 (= v45 0 ) ) )
[<-recv] success
[send->] (assert v46 )
[<-recv] success
[send->] (assert (= v47 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v48 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v49 (ite v47 v48 false ) ) )
[<-recv] success
[send->] (assert v49 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value5 ) )
[<-recv] success
[send->] (assert (= v53 txn_value5 ) )
[<-recv] success
[send->] (assert (= v54 (= v53 0 ) ) )
[<-recv] success
[send->] (assert v54 )
[<-recv] success
[send->] (assert (= v55 (uint256->bytes v50 ) ) )
[<-recv] success
[send->] (assert (= v56 (uint256->bytes v51 ) ) )
[<-recv] success
[send->] (assert (= v57 (msg-cat v55 v56 ) ) )
[<-recv] success
[send->] (assert (= v58 (digest v57 ) ) )
[<-recv] success
[send->] (assert (= v59 (= v28 v58 ) ) )
[<-recv] success
[send->] (assert v59 )
[<-recv] success
[send->] (assert (= v60 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v61 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (assert v62 )
[<-recv] success
[send->] (assert (= v63 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v64 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v65 (ite v63 v64 false ) ) )
[<-recv] success
[send->] (assert (= v66 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v67 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v68 (ite v66 v67 false ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v65 v68 false ) ) )
[<-recv] success
[send->] (assert (= v70 (- 4 v43 ) ) )
[<-recv] success
[send->] (assert (= v71 (+ v51 v70 ) ) )
[<-recv] success
[send->] (assert (= v72 (mod v71 3 ) ) )
[<-recv] success
[send->] (assert (= v73 (ite v68 0 1 ) ) )
[<-recv] success
[send->] (assert (= v74 (ite v65 2 v73 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v69 v72 v74 ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v75 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v75 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v78 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (assert (= v79 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v80 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v81 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v83 true v82 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v84 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (assert (= v85 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v85 false true ) ) )
[<-recv] success
[send->] (assert (= v90 (ite v89 true v88 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v90 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v90 )
[<-recv] success
[send->] (assert (= v91 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v92 (= v51 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v92 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v93 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v94 (= v51 1 ) ) )
[<-recv] success
[send->] (assert (= v95 (ite v94 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v95 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v96 (= v51 2 ) ) )
[<-recv] success
[send->] (assert (= v97 (ite v96 v91 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v97 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v98 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v99 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v101 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v102 (ite v101 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v102 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v103 (= v43 2 ) ) )
[<-recv] success
[send->] (assert (= v104 (ite v103 v98 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v104 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v105 (= v75 2 ) ) )
[<-recv] success
[send->] (assert (= v106 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v107 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v108 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v109 (ite v107 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v107 v108 v2 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v105 v106 v109 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v105 0 v110 ) ) )
[<-recv] success
[send->] (assert (= v113 (+ v3 v111 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v113 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v112 ) )
[<-recv] success
[send->] (assert (= v115 (<= 0 v75 ) ) )
[<-recv] success
[send->] (assert (= v116 (< v75 3 ) ) )
[<-recv] success
[send->] (assert (= v117 (ite v115 v116 false ) ) )
[<-recv] success
[send->] (assert v117 )
[<-recv] success
[send->] (assert (= v118 (= v75 0 ) ) )
[<-recv] success
[send->] (assert (= v119 (= v75 1 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v119 (bytes-literal 3502248497 ) (bytes-literal 1195058296 ) ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v118 (bytes-literal 1946528133 ) v120 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance7 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (exit )
