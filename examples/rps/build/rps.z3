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
[send->] (declare-fun v4 () Bool )
[<-recv] success
[send->] (declare-fun v5 () Int )
[<-recv] success
[send->] (declare-fun v6 () Int )
[<-recv] success
[send->] (declare-fun v7 () Int )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (declare-fun v9 () Bool )
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
[send->] (declare-fun v29 () Bool )
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
[send->] (declare-fun v44 () Bool )
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
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (declare-fun v57 () Bytes )
[<-recv] success
[send->] (declare-fun v58 () Bytes )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (declare-fun v60 () Int )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (declare-fun v62 () Bytes )
[<-recv] success
[send->] (declare-fun v63 () Bytes )
[<-recv] success
[send->] (declare-fun v64 () Bytes )
[<-recv] success
[send->] (declare-fun v65 () Int )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (declare-fun v77 () Int )
[<-recv] success
[send->] (declare-fun v78 () Int )
[<-recv] success
[send->] (declare-fun v79 () Int )
[<-recv] success
[send->] (declare-fun v80 () Int )
[<-recv] success
[send->] (declare-fun v81 () Int )
[<-recv] success
[send->] (declare-fun v82 () Int )
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
[send->] (declare-fun v106 () Bool )
[<-recv] success
[send->] (declare-fun v107 () Bool )
[<-recv] success
[send->] (declare-fun v108 () Bool )
[<-recv] success
[send->] (declare-fun v109 () Bool )
[<-recv] success
[send->] (declare-fun v110 () Bool )
[<-recv] success
[send->] (declare-fun v111 () Bool )
[<-recv] success
[send->] (declare-fun v112 () Bool )
[<-recv] success
[send->] (declare-fun v113 () Int )
[<-recv] success
[send->] (declare-fun v114 () Bool )
[<-recv] success
[send->] (declare-fun v115 () Int )
[<-recv] success
[send->] (declare-fun v116 () Int )
[<-recv] success
[send->] (declare-fun v117 () Int )
[<-recv] success
[send->] (declare-fun v118 () Int )
[<-recv] success
[send->] (declare-fun v119 () Int )
[<-recv] success
[send->] (declare-fun v120 () Int )
[<-recv] success
[send->] (declare-fun v121 () Bool )
[<-recv] success
[send->] (declare-fun v122 () Bool )
[<-recv] success
[send->] (declare-fun v123 () Bool )
[<-recv] success
[send->] (declare-fun v124 () Bool )
[<-recv] success
[send->] (declare-fun v125 () Bool )
[<-recv] success
[send->] (declare-fun v126 () Bool )
[<-recv] success
[send->] (declare-fun v127 () Bool )
[<-recv] success
[send->] (declare-fun v128 () Bool )
[<-recv] success
[send->] (declare-fun v129 () Bytes )
[<-recv] success
[send->] (declare-fun v130 () Bytes )
[<-recv] success
[send->] (declare-fun v131 () Bytes )
[<-recv] success
[send->] (declare-fun v132 () Bytes )
[<-recv] success
[send->] (declare-fun v133 () Int )
[<-recv] success
[send->] (declare-fun v134 () Int )
[<-recv] success
[send->] (declare-fun v135 () Int )
[<-recv] success
[send->] (declare-fun v136 () Int )
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
[send->] (push 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance0 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v136 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance2 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v135 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v135 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance3 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v134 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v134 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance4 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (assert (= v52 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v53 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v54 (ite v52 v53 false ) ) )
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
[send->] (assert (= v55 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v56 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v57 (ite v56 (bytes-literal 3108938406 ) (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v58 (ite v55 (bytes-literal 31217174 ) v57 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v133 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v133 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance5 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (= txn_value5 0 ) )
[<-recv] success
[send->] (assert (= v60 txn_value5 ) )
[<-recv] success
[send->] (assert (= v61 (= v60 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v61 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v61 )
[<-recv] success
[send->] (assert (= v62 (uint256->bytes v50 ) ) )
[<-recv] success
[send->] (assert (= v63 (uint256->bytes v51 ) ) )
[<-recv] success
[send->] (assert (= v64 (msg-cat v62 v63 ) ) )
[<-recv] success
[send->] (assert (= v65 (digest v64 ) ) )
[<-recv] success
[send->] (assert (= v66 (= v28 v65 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v66 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v66 )
[<-recv] success
[send->] (assert (= v67 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v68 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v67 v68 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v69 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (assert (= v70 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (assert (= v73 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (ite v72 v75 false ) ) )
[<-recv] success
[send->] (assert (= v77 (- 4 v43 ) ) )
[<-recv] success
[send->] (assert (= v78 (+ v51 v77 ) ) )
[<-recv] success
[send->] (assert (= v79 (mod v78 3 ) ) )
[<-recv] success
[send->] (assert (= v80 (ite v75 0 1 ) ) )
[<-recv] success
[send->] (assert (= v81 (ite v72 2 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v76 v79 v81 ) ) )
[<-recv] success
[send->] (assert (= v83 (<= 0 v82 ) ) )
[<-recv] success
[send->] (assert (= v84 (< v82 5 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v83 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v85 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v85 )
[<-recv] success
[send->] (assert (= v86 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v87 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v88 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v87 v88 false ) ) )
[<-recv] success
[send->] (assert (= v90 (ite v86 false true ) ) )
[<-recv] success
[send->] (assert (= v91 (ite v90 true v89 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v91 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (assert (= v92 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v94 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v95 (ite v93 v94 false ) ) )
[<-recv] success
[send->] (assert (= v96 (ite v92 false true ) ) )
[<-recv] success
[send->] (assert (= v97 (ite v96 true v95 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v97 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v97 )
[<-recv] success
[send->] (assert (= v98 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v99 (= v51 0 ) ) )
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
[send->] (assert (= v101 (= v51 1 ) ) )
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
[send->] (assert (= v103 (= v51 2 ) ) )
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
[send->] (assert (= v105 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v106 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v109 (ite v108 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v109 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v110 (= v43 2 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v110 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v111 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v112 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v113 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v114 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v115 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v116 (ite v114 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (ite v114 v115 v2 ) ) )
[<-recv] success
[send->] (assert (= v118 (ite v112 v113 v116 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v112 0 v117 ) ) )
[<-recv] success
[send->] (assert (= v120 (+ v3 v118 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v120 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v119 ) )
[<-recv] success
[send->] (assert (= v122 (<= 0 v82 ) ) )
[<-recv] success
[send->] (assert (= v123 (< v82 5 ) ) )
[<-recv] success
[send->] (assert (= v124 (ite v122 v123 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v124 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v124 )
[<-recv] success
[send->] (assert (= v125 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v126 (= v82 1 ) ) )
[<-recv] success
[send->] (assert (= v127 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v128 (= v82 3 ) ) )
[<-recv] success
[send->] (assert (= v129 (ite v128 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v130 (ite v127 (bytes-literal 1195058296 ) v129 ) ) )
[<-recv] success
[send->] (assert (= v131 (ite v126 (bytes-literal 3502248497 ) v130 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v125 (bytes-literal 1946528133 ) v131 ) ) )
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
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
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
[send->] (push 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance0 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v136 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance2 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v135 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v135 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance3 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v134 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v134 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance4 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (assert (= v52 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v53 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v54 (ite v52 v53 false ) ) )
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
[send->] (assert (= v55 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v56 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v57 (ite v56 (bytes-literal 3108938406 ) (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v58 (ite v55 (bytes-literal 31217174 ) v57 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v133 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v133 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance5 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (= txn_value5 0 ) )
[<-recv] success
[send->] (assert (= v60 txn_value5 ) )
[<-recv] success
[send->] (assert (= v61 (= v60 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v61 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v61 )
[<-recv] success
[send->] (assert (= v62 (uint256->bytes v50 ) ) )
[<-recv] success
[send->] (assert (= v63 (uint256->bytes v51 ) ) )
[<-recv] success
[send->] (assert (= v64 (msg-cat v62 v63 ) ) )
[<-recv] success
[send->] (assert (= v65 (digest v64 ) ) )
[<-recv] success
[send->] (assert (= v66 (= v28 v65 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v66 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v66 )
[<-recv] success
[send->] (assert (= v67 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v68 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v67 v68 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v69 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (assert (= v70 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (assert (= v73 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (ite v72 v75 false ) ) )
[<-recv] success
[send->] (assert (= v77 (- 4 v43 ) ) )
[<-recv] success
[send->] (assert (= v78 (+ v51 v77 ) ) )
[<-recv] success
[send->] (assert (= v79 (mod v78 3 ) ) )
[<-recv] success
[send->] (assert (= v80 (ite v75 0 1 ) ) )
[<-recv] success
[send->] (assert (= v81 (ite v72 2 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v76 v79 v81 ) ) )
[<-recv] success
[send->] (assert (= v83 (<= 0 v82 ) ) )
[<-recv] success
[send->] (assert (= v84 (< v82 5 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v83 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v85 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v85 )
[<-recv] success
[send->] (assert (= v86 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v87 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v88 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v87 v88 false ) ) )
[<-recv] success
[send->] (assert (= v90 (ite v86 false true ) ) )
[<-recv] success
[send->] (assert (= v91 (ite v90 true v89 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v91 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (assert (= v92 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v94 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v95 (ite v93 v94 false ) ) )
[<-recv] success
[send->] (assert (= v96 (ite v92 false true ) ) )
[<-recv] success
[send->] (assert (= v97 (ite v96 true v95 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v97 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v97 )
[<-recv] success
[send->] (assert (= v98 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v99 (= v51 0 ) ) )
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
[send->] (assert (= v101 (= v51 1 ) ) )
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
[send->] (assert (= v103 (= v51 2 ) ) )
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
[send->] (assert (= v105 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v106 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v109 (ite v108 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v109 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v110 (= v43 2 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v110 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v111 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v112 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v113 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v114 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v115 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v116 (ite v114 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (ite v114 v115 v2 ) ) )
[<-recv] success
[send->] (assert (= v118 (ite v112 v113 v116 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v112 0 v117 ) ) )
[<-recv] success
[send->] (assert (= v120 (+ v3 v118 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v120 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v119 ) )
[<-recv] success
[send->] (assert (= v122 (<= 0 v82 ) ) )
[<-recv] success
[send->] (assert (= v123 (< v82 5 ) ) )
[<-recv] success
[send->] (assert (= v124 (ite v122 v123 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v124 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v124 )
[<-recv] success
[send->] (assert (= v125 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v126 (= v82 1 ) ) )
[<-recv] success
[send->] (assert (= v127 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v128 (= v82 3 ) ) )
[<-recv] success
[send->] (assert (= v129 (ite v128 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v130 (ite v127 (bytes-literal 1195058296 ) v129 ) ) )
[<-recv] success
[send->] (assert (= v131 (ite v126 (bytes-literal 3502248497 ) v130 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v125 (bytes-literal 1946528133 ) v131 ) ) )
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
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
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
[send->] (push 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance0 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v136 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance2 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v135 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v135 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance3 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v134 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v134 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance4 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (assert (= v52 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v53 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v54 (ite v52 v53 false ) ) )
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
[send->] (assert (= v55 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v56 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v57 (ite v56 (bytes-literal 3108938406 ) (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v58 (ite v55 (bytes-literal 31217174 ) v57 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v133 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v133 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not true ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance5 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (= txn_value5 0 ) )
[<-recv] success
[send->] (assert (= v60 txn_value5 ) )
[<-recv] success
[send->] (assert (= v61 (= v60 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v61 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v61 )
[<-recv] success
[send->] (assert (= v62 (uint256->bytes v50 ) ) )
[<-recv] success
[send->] (assert (= v63 (uint256->bytes v51 ) ) )
[<-recv] success
[send->] (assert (= v64 (msg-cat v62 v63 ) ) )
[<-recv] success
[send->] (assert (= v65 (digest v64 ) ) )
[<-recv] success
[send->] (assert (= v66 (= v28 v65 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v66 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v66 )
[<-recv] success
[send->] (assert (= v67 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v68 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v67 v68 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v69 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (assert (= v70 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (assert (= v73 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (ite v72 v75 false ) ) )
[<-recv] success
[send->] (assert (= v77 (- 4 v43 ) ) )
[<-recv] success
[send->] (assert (= v78 (+ v51 v77 ) ) )
[<-recv] success
[send->] (assert (= v79 (mod v78 3 ) ) )
[<-recv] success
[send->] (assert (= v80 (ite v75 0 1 ) ) )
[<-recv] success
[send->] (assert (= v81 (ite v72 2 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v76 v79 v81 ) ) )
[<-recv] success
[send->] (assert (= v83 (<= 0 v82 ) ) )
[<-recv] success
[send->] (assert (= v84 (< v82 5 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v83 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v85 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v85 )
[<-recv] success
[send->] (assert (= v86 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v87 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v88 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v87 v88 false ) ) )
[<-recv] success
[send->] (assert (= v90 (ite v86 false true ) ) )
[<-recv] success
[send->] (assert (= v91 (ite v90 true v89 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v91 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (assert (= v92 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v94 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v95 (ite v93 v94 false ) ) )
[<-recv] success
[send->] (assert (= v96 (ite v92 false true ) ) )
[<-recv] success
[send->] (assert (= v97 (ite v96 true v95 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v97 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v97 )
[<-recv] success
[send->] (assert (= v98 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v99 (= v51 0 ) ) )
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
[send->] (assert (= v101 (= v51 1 ) ) )
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
[send->] (assert (= v103 (= v51 2 ) ) )
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
[send->] (assert (= v105 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v106 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v109 (ite v108 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v109 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v110 (= v43 2 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v110 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v111 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v112 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v113 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v114 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v115 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v116 (ite v114 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (ite v114 v115 v2 ) ) )
[<-recv] success
[send->] (assert (= v118 (ite v112 v113 v116 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v112 0 v117 ) ) )
[<-recv] success
[send->] (assert (= v120 (+ v3 v118 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v120 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v119 ) )
[<-recv] success
[send->] (assert (= v122 (<= 0 v82 ) ) )
[<-recv] success
[send->] (assert (= v123 (< v82 5 ) ) )
[<-recv] success
[send->] (assert (= v124 (ite v122 v123 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v124 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v124 )
[<-recv] success
[send->] (assert (= v125 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v126 (= v82 1 ) ) )
[<-recv] success
[send->] (assert (= v127 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v128 (= v82 3 ) ) )
[<-recv] success
[send->] (assert (= v129 (ite v128 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v130 (ite v127 (bytes-literal 1195058296 ) v129 ) ) )
[<-recv] success
[send->] (assert (= v131 (ite v126 (bytes-literal 3502248497 ) v130 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v125 (bytes-literal 1946528133 ) v131 ) ) )
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
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
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
[send->] (push 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance0 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v136 ) )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance2 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v135 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v135 ) )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance3 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v134 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v134 ) )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance4 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v133 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v133 ) )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance5 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value5 ) )
[<-recv] success
[send->] (assert (= v60 txn_value5 ) )
[<-recv] success
[send->] (assert (= v61 (= v60 0 ) ) )
[<-recv] success
[send->] (assert v61 )
[<-recv] success
[send->] (assert (= v62 (uint256->bytes v50 ) ) )
[<-recv] success
[send->] (assert (= v63 (uint256->bytes v51 ) ) )
[<-recv] success
[send->] (assert (= v64 (msg-cat v62 v63 ) ) )
[<-recv] success
[send->] (assert (= v65 (digest v64 ) ) )
[<-recv] success
[send->] (assert (= v66 (= v28 v65 ) ) )
[<-recv] success
[send->] (assert v66 )
[<-recv] success
[send->] (assert (= v67 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v68 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v67 v68 false ) ) )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (assert (= v70 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (assert (= v73 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (ite v72 v75 false ) ) )
[<-recv] success
[send->] (assert (= v77 (- 4 v43 ) ) )
[<-recv] success
[send->] (assert (= v78 (+ v51 v77 ) ) )
[<-recv] success
[send->] (assert (= v79 (mod v78 3 ) ) )
[<-recv] success
[send->] (assert (= v80 (ite v75 0 1 ) ) )
[<-recv] success
[send->] (assert (= v81 (ite v72 2 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v76 v79 v81 ) ) )
[<-recv] success
[send->] (assert (= v83 (<= 0 v82 ) ) )
[<-recv] success
[send->] (assert (= v84 (< v82 5 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v83 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v85 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v85 )
[<-recv] success
[send->] (assert (= v86 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v87 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v88 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v87 v88 false ) ) )
[<-recv] success
[send->] (assert (= v90 (ite v86 false true ) ) )
[<-recv] success
[send->] (assert (= v91 (ite v90 true v89 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v91 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (assert (= v92 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v94 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v95 (ite v93 v94 false ) ) )
[<-recv] success
[send->] (assert (= v96 (ite v92 false true ) ) )
[<-recv] success
[send->] (assert (= v97 (ite v96 true v95 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v97 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v97 )
[<-recv] success
[send->] (assert (= v98 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v99 (= v51 0 ) ) )
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
[send->] (assert (= v101 (= v51 1 ) ) )
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
[send->] (assert (= v103 (= v51 2 ) ) )
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
[send->] (assert (= v105 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v106 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v109 (ite v108 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v109 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v110 (= v43 2 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v110 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v111 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v112 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v113 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v114 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v115 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v116 (ite v114 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (ite v114 v115 v2 ) ) )
[<-recv] success
[send->] (assert (= v118 (ite v112 v113 v116 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v112 0 v117 ) ) )
[<-recv] success
[send->] (assert (= v120 (+ v3 v118 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v120 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v119 ) )
[<-recv] success
[send->] (assert (= v122 (<= 0 v82 ) ) )
[<-recv] success
[send->] (assert (= v123 (< v82 5 ) ) )
[<-recv] success
[send->] (assert (= v124 (ite v122 v123 false ) ) )
[<-recv] success
[send->] (assert v124 )
[<-recv] success
[send->] (assert (= v125 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v126 (= v82 1 ) ) )
[<-recv] success
[send->] (assert (= v127 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v128 (= v82 3 ) ) )
[<-recv] success
[send->] (assert (= v129 (ite v128 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v130 (ite v127 (bytes-literal 1195058296 ) v129 ) ) )
[<-recv] success
[send->] (assert (= v131 (ite v126 (bytes-literal 3502248497 ) v130 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v125 (bytes-literal 1946528133 ) v131 ) ) )
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
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
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
[send->] (push 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance0 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v136 ) )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance2 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v135 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v135 ) )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance3 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v134 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v134 ) )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance4 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (assert (= v52 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v53 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v54 (ite v52 v53 false ) ) )
[<-recv] success
[send->] (assert v54 )
[<-recv] success
[send->] (assert (= v55 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v56 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v57 (ite v56 (bytes-literal 3108938406 ) (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v58 (ite v55 (bytes-literal 31217174 ) v57 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v133 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v133 ) )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance5 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value5 ) )
[<-recv] success
[send->] (assert (= v60 txn_value5 ) )
[<-recv] success
[send->] (assert (= v61 (= v60 0 ) ) )
[<-recv] success
[send->] (assert v61 )
[<-recv] success
[send->] (assert (= v62 (uint256->bytes v50 ) ) )
[<-recv] success
[send->] (assert (= v63 (uint256->bytes v51 ) ) )
[<-recv] success
[send->] (assert (= v64 (msg-cat v62 v63 ) ) )
[<-recv] success
[send->] (assert (= v65 (digest v64 ) ) )
[<-recv] success
[send->] (assert (= v66 (= v28 v65 ) ) )
[<-recv] success
[send->] (assert v66 )
[<-recv] success
[send->] (assert (= v67 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v68 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v67 v68 false ) ) )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (assert (= v70 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (assert (= v73 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (ite v72 v75 false ) ) )
[<-recv] success
[send->] (assert (= v77 (- 4 v43 ) ) )
[<-recv] success
[send->] (assert (= v78 (+ v51 v77 ) ) )
[<-recv] success
[send->] (assert (= v79 (mod v78 3 ) ) )
[<-recv] success
[send->] (assert (= v80 (ite v75 0 1 ) ) )
[<-recv] success
[send->] (assert (= v81 (ite v72 2 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v76 v79 v81 ) ) )
[<-recv] success
[send->] (assert (= v83 (<= 0 v82 ) ) )
[<-recv] success
[send->] (assert (= v84 (< v82 5 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v83 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v85 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v85 )
[<-recv] success
[send->] (assert (= v86 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v87 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v88 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v87 v88 false ) ) )
[<-recv] success
[send->] (assert (= v90 (ite v86 false true ) ) )
[<-recv] success
[send->] (assert (= v91 (ite v90 true v89 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v91 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (assert (= v92 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v94 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v95 (ite v93 v94 false ) ) )
[<-recv] success
[send->] (assert (= v96 (ite v92 false true ) ) )
[<-recv] success
[send->] (assert (= v97 (ite v96 true v95 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v97 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v97 )
[<-recv] success
[send->] (assert (= v98 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v99 (= v51 0 ) ) )
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
[send->] (assert (= v101 (= v51 1 ) ) )
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
[send->] (assert (= v103 (= v51 2 ) ) )
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
[send->] (assert (= v105 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v106 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v109 (ite v108 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v109 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v110 (= v43 2 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v110 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v111 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v112 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v113 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v114 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v115 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v116 (ite v114 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (ite v114 v115 v2 ) ) )
[<-recv] success
[send->] (assert (= v118 (ite v112 v113 v116 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v112 0 v117 ) ) )
[<-recv] success
[send->] (assert (= v120 (+ v3 v118 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v120 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v119 ) )
[<-recv] success
[send->] (assert (= v122 (<= 0 v82 ) ) )
[<-recv] success
[send->] (assert (= v123 (< v82 5 ) ) )
[<-recv] success
[send->] (assert (= v124 (ite v122 v123 false ) ) )
[<-recv] success
[send->] (assert v124 )
[<-recv] success
[send->] (assert (= v125 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v126 (= v82 1 ) ) )
[<-recv] success
[send->] (assert (= v127 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v128 (= v82 3 ) ) )
[<-recv] success
[send->] (assert (= v129 (ite v128 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v130 (ite v127 (bytes-literal 1195058296 ) v129 ) ) )
[<-recv] success
[send->] (assert (= v131 (ite v126 (bytes-literal 3502248497 ) v130 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v125 (bytes-literal 1946528133 ) v131 ) ) )
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
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
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
[send->] (push 1 )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance0 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v136 ) )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance2 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v135 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v135 ) )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance3 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v134 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v134 ) )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance4 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
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
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v133 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v133 ) )
[<-recv] success
[send->] (assert true )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance5 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value5 ) )
[<-recv] success
[send->] (assert (= v60 txn_value5 ) )
[<-recv] success
[send->] (assert (= v61 (= v60 0 ) ) )
[<-recv] success
[send->] (assert v61 )
[<-recv] success
[send->] (assert (= v62 (uint256->bytes v50 ) ) )
[<-recv] success
[send->] (assert (= v63 (uint256->bytes v51 ) ) )
[<-recv] success
[send->] (assert (= v64 (msg-cat v62 v63 ) ) )
[<-recv] success
[send->] (assert (= v65 (digest v64 ) ) )
[<-recv] success
[send->] (assert (= v66 (= v28 v65 ) ) )
[<-recv] success
[send->] (assert v66 )
[<-recv] success
[send->] (assert (= v67 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v68 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v67 v68 false ) ) )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (assert (= v70 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (assert (= v73 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (ite v72 v75 false ) ) )
[<-recv] success
[send->] (assert (= v77 (- 4 v43 ) ) )
[<-recv] success
[send->] (assert (= v78 (+ v51 v77 ) ) )
[<-recv] success
[send->] (assert (= v79 (mod v78 3 ) ) )
[<-recv] success
[send->] (assert (= v80 (ite v75 0 1 ) ) )
[<-recv] success
[send->] (assert (= v81 (ite v72 2 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v76 v79 v81 ) ) )
[<-recv] success
[send->] (assert (= v83 (<= 0 v82 ) ) )
[<-recv] success
[send->] (assert (= v84 (< v82 5 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v83 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v85 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v85 )
[<-recv] success
[send->] (assert (= v86 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v87 (<= 0 v51 ) ) )
[<-recv] success
[send->] (assert (= v88 (< v51 3 ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v87 v88 false ) ) )
[<-recv] success
[send->] (assert (= v90 (ite v86 false true ) ) )
[<-recv] success
[send->] (assert (= v91 (ite v90 true v89 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v91 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (assert (= v92 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (<= 0 v43 ) ) )
[<-recv] success
[send->] (assert (= v94 (< v43 3 ) ) )
[<-recv] success
[send->] (assert (= v95 (ite v93 v94 false ) ) )
[<-recv] success
[send->] (assert (= v96 (ite v92 false true ) ) )
[<-recv] success
[send->] (assert (= v97 (ite v96 true v95 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v97 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v97 )
[<-recv] success
[send->] (assert (= v98 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v99 (= v51 0 ) ) )
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
[send->] (assert (= v101 (= v51 1 ) ) )
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
[send->] (assert (= v103 (= v51 2 ) ) )
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
[send->] (assert (= v105 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v106 (= v43 0 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v43 1 ) ) )
[<-recv] success
[send->] (assert (= v109 (ite v108 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v109 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v110 (= v43 2 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v110 v105 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v111 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v112 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v113 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v114 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v115 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v116 (ite v114 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (ite v114 v115 v2 ) ) )
[<-recv] success
[send->] (assert (= v118 (ite v112 v113 v116 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v112 0 v117 ) ) )
[<-recv] success
[send->] (assert (= v120 (+ v3 v118 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v120 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v119 ) )
[<-recv] success
[send->] (assert (= v122 (<= 0 v82 ) ) )
[<-recv] success
[send->] (assert (= v123 (< v82 5 ) ) )
[<-recv] success
[send->] (assert (= v124 (ite v122 v123 false ) ) )
[<-recv] success
[send->] (assert v124 )
[<-recv] success
[send->] (assert (= v125 (= v82 0 ) ) )
[<-recv] success
[send->] (assert (= v126 (= v82 1 ) ) )
[<-recv] success
[send->] (assert (= v127 (= v82 2 ) ) )
[<-recv] success
[send->] (assert (= v128 (= v82 3 ) ) )
[<-recv] success
[send->] (assert (= v129 (ite v128 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v130 (ite v127 (bytes-literal 1195058296 ) v129 ) ) )
[<-recv] success
[send->] (assert (= v131 (ite v126 (bytes-literal 3502248497 ) v130 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v125 (bytes-literal 1946528133 ) v131 ) ) )
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
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (exit )
