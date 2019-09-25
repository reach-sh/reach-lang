[send->] (set-option :print-success true )
[<-recv] success
[send->] (set-option :produce-models true )
[<-recv] success
[send->] (set-option :produce-unsat-cores true )
[<-recv] success
[send->] (declare-sort Address 0 )
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
[send->] (declare-fun v8 () Int )
[<-recv] success
[send->] (declare-fun v9 () Int )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (declare-fun v13 () Int )
[<-recv] success
[send->] (declare-fun v14 () Bool )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (declare-fun v16 () Bool )
[<-recv] success
[send->] (declare-fun v17 () Bool )
[<-recv] success
[send->] (declare-fun v18 () Bool )
[<-recv] success
[send->] (declare-fun v19 () Bool )
[<-recv] success
[send->] (declare-fun v20 () Bool )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (declare-fun v23 () Bool )
[<-recv] success
[send->] (declare-fun v24 () Bool )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (declare-fun v26 () Int )
[<-recv] success
[send->] (declare-fun v27 () Bytes )
[<-recv] success
[send->] (declare-fun v28 () Bytes )
[<-recv] success
[send->] (declare-fun v29 () Bytes )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (declare-fun v31 () Int )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (declare-fun v33 () Int )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (declare-fun v35 () Bytes )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (declare-fun v41 () Int )
[<-recv] success
[send->] (declare-fun v42 () Int )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (declare-fun v46 () Int )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (declare-fun v50 () Bool )
[<-recv] success
[send->] (declare-fun v51 () Bool )
[<-recv] success
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (declare-fun v54 () Int )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (declare-fun v60 () Bytes )
[<-recv] success
[send->] (declare-fun v61 () Bytes )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (declare-fun v63 () Int )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (declare-fun v65 () Bytes )
[<-recv] success
[send->] (declare-fun v66 () Bytes )
[<-recv] success
[send->] (declare-fun v67 () Bytes )
[<-recv] success
[send->] (declare-fun v68 () Int )
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
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (declare-fun v80 () Int )
[<-recv] success
[send->] (declare-fun v81 () Int )
[<-recv] success
[send->] (declare-fun v82 () Int )
[<-recv] success
[send->] (declare-fun v83 () Int )
[<-recv] success
[send->] (declare-fun v84 () Int )
[<-recv] success
[send->] (declare-fun v85 () Int )
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
[send->] (declare-fun v113 () Bool )
[<-recv] success
[send->] (declare-fun v114 () Bool )
[<-recv] success
[send->] (declare-fun v115 () Bool )
[<-recv] success
[send->] (declare-fun v116 () Int )
[<-recv] success
[send->] (declare-fun v117 () Bool )
[<-recv] success
[send->] (declare-fun v118 () Int )
[<-recv] success
[send->] (declare-fun v119 () Int )
[<-recv] success
[send->] (declare-fun v120 () Int )
[<-recv] success
[send->] (declare-fun v121 () Int )
[<-recv] success
[send->] (declare-fun v122 () Int )
[<-recv] success
[send->] (declare-fun v123 () Int )
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
[send->] (declare-fun v129 () Bool )
[<-recv] success
[send->] (declare-fun v130 () Bool )
[<-recv] success
[send->] (declare-fun v131 () Bool )
[<-recv] success
[send->] (declare-fun v132 () Bytes )
[<-recv] success
[send->] (declare-fun v133 () Bytes )
[<-recv] success
[send->] (declare-fun v134 () Bytes )
[<-recv] success
[send->] (declare-fun v135 () Bytes )
[<-recv] success
[send->] (declare-fun v136 () Int )
[<-recv] success
[send->] (declare-fun v137 () Int )
[<-recv] success
[send->] (declare-fun v138 () Int )
[<-recv] success
[send->] (declare-fun v139 () Int )
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
[send->] (assert (= v8 txn_value1 ) )
[<-recv] success
[send->] (assert (= v9 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v10 (= v8 v9 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v10 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v10 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v139 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v139 ) )
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
[send->] (assert (= v13 txn_value2 ) )
[<-recv] success
[send->] (assert (= v14 (= v13 v2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v14 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v14 )
[<-recv] success
[send->] (assert (= v16 (= v15 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v17 (= v15 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v18 (= v15 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v19 (ite v16 true v17 ) ) )
[<-recv] success
[send->] (assert (= v20 (ite v19 true v18 ) ) )
[<-recv] success
[send->] (assert v20 )
[<-recv] success
[send->] (assert (= v21 (ite v17 1 2 ) ) )
[<-recv] success
[send->] (assert (= v22 (ite v16 0 v21 ) ) )
[<-recv] success
[send->] (assert (= v23 (<= 0 v22 ) ) )
[<-recv] success
[send->] (assert (= v24 (< v22 3 ) ) )
[<-recv] success
[send->] (assert (= v25 (ite v23 v24 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v25 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (assert (= v27 (uint256->bytes v26 ) ) )
[<-recv] success
[send->] (assert (= v28 (uint256->bytes v22 ) ) )
[<-recv] success
[send->] (assert (= v29 (msg-cat v27 v28 ) ) )
[<-recv] success
[send->] (assert (= v30 (digest v29 ) ) )
[<-recv] success
[send->] (assert (= v31 v30 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v138 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v138 ) )
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
[send->] (assert (= v33 txn_value3 ) )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v34 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (assert (= v36 (= v35 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v37 (= v35 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v38 (= v35 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v39 (ite v36 true v37 ) ) )
[<-recv] success
[send->] (assert (= v40 (ite v39 true v38 ) ) )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (assert (= v41 (ite v37 1 2 ) ) )
[<-recv] success
[send->] (assert (= v42 (ite v36 0 v41 ) ) )
[<-recv] success
[send->] (assert (= v43 (<= 0 v42 ) ) )
[<-recv] success
[send->] (assert (= v44 (< v42 3 ) ) )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v45 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v45 )
[<-recv] success
[send->] (assert (= v46 v42 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v137 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v137 ) )
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
[send->] (assert (= v48 txn_value4 ) )
[<-recv] success
[send->] (assert (= v49 (= v48 0 ) ) )
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
[send->] (assert (= v50 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v51 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v52 (ite v50 v51 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v52 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v52 )
[<-recv] success
[send->] (assert (= v53 v26 ) )
[<-recv] success
[send->] (assert (= v54 v22 ) )
[<-recv] success
[send->] (assert (= v55 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v56 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v57 (ite v55 v56 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v57 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v57 )
[<-recv] success
[send->] (assert (= v58 (= v46 0 ) ) )
[<-recv] success
[send->] (assert (= v59 (= v46 1 ) ) )
[<-recv] success
[send->] (assert (= v60 (ite v59 (bytes-literal 3108938406 ) (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v61 (ite v58 (bytes-literal 31217174 ) v60 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v136 ) )
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
[send->] (assert (= v63 txn_value5 ) )
[<-recv] success
[send->] (assert (= v64 (= v63 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v64 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (assert (= v65 (uint256->bytes v53 ) ) )
[<-recv] success
[send->] (assert (= v66 (uint256->bytes v54 ) ) )
[<-recv] success
[send->] (assert (= v67 (msg-cat v65 v66 ) ) )
[<-recv] success
[send->] (assert (= v68 (digest v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (= v31 v68 ) ) )
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
[send->] (assert (= v70 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v72 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (assert (= v73 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (assert (= v79 (ite v75 v78 false ) ) )
[<-recv] success
[send->] (assert (= v80 (- 4 v46 ) ) )
[<-recv] success
[send->] (assert (= v81 (+ v54 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (mod v81 3 ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v78 0 1 ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v75 2 v83 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v79 v82 v84 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v88 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (assert (= v89 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v90 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v91 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v92 (ite v90 v91 false ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v89 false true ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 true v92 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v94 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (assert (= v95 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v96 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v97 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v96 v97 false ) ) )
[<-recv] success
[send->] (assert (= v99 (ite v95 false true ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 true v98 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v100 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (assert (= v101 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v102 (= v54 0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v104 (= v54 1 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v106 (= v54 2 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v109 (= v46 0 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v109 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v110 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v111 (= v46 1 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v111 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v112 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v113 (= v46 2 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v114 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v115 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v116 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v118 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v117 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v117 v118 v2 ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v115 v116 v119 ) ) )
[<-recv] success
[send->] (assert (= v122 (ite v115 0 v120 ) ) )
[<-recv] success
[send->] (assert (= v123 (+ v3 v121 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v123 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v122 ) )
[<-recv] success
[send->] (assert (= v125 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v126 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v127 (ite v125 v126 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v127 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v127 )
[<-recv] success
[send->] (assert (= v128 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v129 (= v85 1 ) ) )
[<-recv] success
[send->] (assert (= v130 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v131 (= v85 3 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v131 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v133 (ite v130 (bytes-literal 1195058296 ) v132 ) ) )
[<-recv] success
[send->] (assert (= v134 (ite v129 (bytes-literal 3502248497 ) v133 ) ) )
[<-recv] success
[send->] (assert (= v135 (ite v128 (bytes-literal 1946528133 ) v134 ) ) )
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
[send->] (assert (= v8 txn_value1 ) )
[<-recv] success
[send->] (assert (= v9 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v10 (= v8 v9 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v10 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v10 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v139 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v139 ) )
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
[send->] (assert (= v13 txn_value2 ) )
[<-recv] success
[send->] (assert (= v14 (= v13 v2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v14 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v14 )
[<-recv] success
[send->] (assert (= v16 (= v15 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v17 (= v15 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v18 (= v15 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v19 (ite v16 true v17 ) ) )
[<-recv] success
[send->] (assert (= v20 (ite v19 true v18 ) ) )
[<-recv] success
[send->] (assert v20 )
[<-recv] success
[send->] (assert (= v21 (ite v17 1 2 ) ) )
[<-recv] success
[send->] (assert (= v22 (ite v16 0 v21 ) ) )
[<-recv] success
[send->] (assert (= v23 (<= 0 v22 ) ) )
[<-recv] success
[send->] (assert (= v24 (< v22 3 ) ) )
[<-recv] success
[send->] (assert (= v25 (ite v23 v24 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v25 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (assert (= v27 (uint256->bytes v26 ) ) )
[<-recv] success
[send->] (assert (= v28 (uint256->bytes v22 ) ) )
[<-recv] success
[send->] (assert (= v29 (msg-cat v27 v28 ) ) )
[<-recv] success
[send->] (assert (= v30 (digest v29 ) ) )
[<-recv] success
[send->] (assert (= v31 v30 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v138 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v138 ) )
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
[send->] (assert (= v33 txn_value3 ) )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v34 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (assert (= v36 (= v35 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v37 (= v35 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v38 (= v35 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v39 (ite v36 true v37 ) ) )
[<-recv] success
[send->] (assert (= v40 (ite v39 true v38 ) ) )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (assert (= v41 (ite v37 1 2 ) ) )
[<-recv] success
[send->] (assert (= v42 (ite v36 0 v41 ) ) )
[<-recv] success
[send->] (assert (= v43 (<= 0 v42 ) ) )
[<-recv] success
[send->] (assert (= v44 (< v42 3 ) ) )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v45 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v45 )
[<-recv] success
[send->] (assert (= v46 v42 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v137 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v137 ) )
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
[send->] (assert (= v48 txn_value4 ) )
[<-recv] success
[send->] (assert (= v49 (= v48 0 ) ) )
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
[send->] (assert (= v50 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v51 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v52 (ite v50 v51 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v52 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v52 )
[<-recv] success
[send->] (assert (= v53 v26 ) )
[<-recv] success
[send->] (assert (= v54 v22 ) )
[<-recv] success
[send->] (assert (= v55 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v56 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v57 (ite v55 v56 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v57 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v57 )
[<-recv] success
[send->] (assert (= v58 (= v46 0 ) ) )
[<-recv] success
[send->] (assert (= v59 (= v46 1 ) ) )
[<-recv] success
[send->] (assert (= v60 (ite v59 (bytes-literal 3108938406 ) (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v61 (ite v58 (bytes-literal 31217174 ) v60 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v136 ) )
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
[send->] (assert (= v63 txn_value5 ) )
[<-recv] success
[send->] (assert (= v64 (= v63 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v64 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (assert (= v65 (uint256->bytes v53 ) ) )
[<-recv] success
[send->] (assert (= v66 (uint256->bytes v54 ) ) )
[<-recv] success
[send->] (assert (= v67 (msg-cat v65 v66 ) ) )
[<-recv] success
[send->] (assert (= v68 (digest v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (= v31 v68 ) ) )
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
[send->] (assert (= v70 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v72 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (assert (= v73 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (assert (= v79 (ite v75 v78 false ) ) )
[<-recv] success
[send->] (assert (= v80 (- 4 v46 ) ) )
[<-recv] success
[send->] (assert (= v81 (+ v54 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (mod v81 3 ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v78 0 1 ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v75 2 v83 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v79 v82 v84 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v88 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (assert (= v89 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v90 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v91 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v92 (ite v90 v91 false ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v89 false true ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 true v92 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v94 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (assert (= v95 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v96 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v97 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v96 v97 false ) ) )
[<-recv] success
[send->] (assert (= v99 (ite v95 false true ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 true v98 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v100 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (assert (= v101 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v102 (= v54 0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v104 (= v54 1 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v106 (= v54 2 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v109 (= v46 0 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v109 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v110 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v111 (= v46 1 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v111 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v112 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v113 (= v46 2 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v114 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v115 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v116 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v118 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v117 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v117 v118 v2 ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v115 v116 v119 ) ) )
[<-recv] success
[send->] (assert (= v122 (ite v115 0 v120 ) ) )
[<-recv] success
[send->] (assert (= v123 (+ v3 v121 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v123 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v122 ) )
[<-recv] success
[send->] (assert (= v125 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v126 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v127 (ite v125 v126 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v127 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v127 )
[<-recv] success
[send->] (assert (= v128 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v129 (= v85 1 ) ) )
[<-recv] success
[send->] (assert (= v130 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v131 (= v85 3 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v131 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v133 (ite v130 (bytes-literal 1195058296 ) v132 ) ) )
[<-recv] success
[send->] (assert (= v134 (ite v129 (bytes-literal 3502248497 ) v133 ) ) )
[<-recv] success
[send->] (assert (= v135 (ite v128 (bytes-literal 1946528133 ) v134 ) ) )
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
[send->] (assert (= v8 txn_value1 ) )
[<-recv] success
[send->] (assert (= v9 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v10 (= v8 v9 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v10 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v10 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v139 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v139 ) )
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
[send->] (assert (= v13 txn_value2 ) )
[<-recv] success
[send->] (assert (= v14 (= v13 v2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v14 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v14 )
[<-recv] success
[send->] (assert (= v16 (= v15 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v17 (= v15 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v18 (= v15 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v19 (ite v16 true v17 ) ) )
[<-recv] success
[send->] (assert (= v20 (ite v19 true v18 ) ) )
[<-recv] success
[send->] (assert v20 )
[<-recv] success
[send->] (assert (= v21 (ite v17 1 2 ) ) )
[<-recv] success
[send->] (assert (= v22 (ite v16 0 v21 ) ) )
[<-recv] success
[send->] (assert (= v23 (<= 0 v22 ) ) )
[<-recv] success
[send->] (assert (= v24 (< v22 3 ) ) )
[<-recv] success
[send->] (assert (= v25 (ite v23 v24 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v25 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (assert (= v27 (uint256->bytes v26 ) ) )
[<-recv] success
[send->] (assert (= v28 (uint256->bytes v22 ) ) )
[<-recv] success
[send->] (assert (= v29 (msg-cat v27 v28 ) ) )
[<-recv] success
[send->] (assert (= v30 (digest v29 ) ) )
[<-recv] success
[send->] (assert (= v31 v30 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v138 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v138 ) )
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
[send->] (assert (= v33 txn_value3 ) )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v34 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (assert (= v36 (= v35 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v37 (= v35 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v38 (= v35 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v39 (ite v36 true v37 ) ) )
[<-recv] success
[send->] (assert (= v40 (ite v39 true v38 ) ) )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (assert (= v41 (ite v37 1 2 ) ) )
[<-recv] success
[send->] (assert (= v42 (ite v36 0 v41 ) ) )
[<-recv] success
[send->] (assert (= v43 (<= 0 v42 ) ) )
[<-recv] success
[send->] (assert (= v44 (< v42 3 ) ) )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v45 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v45 )
[<-recv] success
[send->] (assert (= v46 v42 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v137 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v137 ) )
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
[send->] (assert (= v48 txn_value4 ) )
[<-recv] success
[send->] (assert (= v49 (= v48 0 ) ) )
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
[send->] (assert (= v50 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v51 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v52 (ite v50 v51 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v52 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v52 )
[<-recv] success
[send->] (assert (= v53 v26 ) )
[<-recv] success
[send->] (assert (= v54 v22 ) )
[<-recv] success
[send->] (assert (= v55 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v56 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v57 (ite v55 v56 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v57 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v57 )
[<-recv] success
[send->] (assert (= v58 (= v46 0 ) ) )
[<-recv] success
[send->] (assert (= v59 (= v46 1 ) ) )
[<-recv] success
[send->] (assert (= v60 (ite v59 (bytes-literal 3108938406 ) (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v61 (ite v58 (bytes-literal 31217174 ) v60 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v136 ) )
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
[send->] (assert (= v63 txn_value5 ) )
[<-recv] success
[send->] (assert (= v64 (= v63 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v64 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (assert (= v65 (uint256->bytes v53 ) ) )
[<-recv] success
[send->] (assert (= v66 (uint256->bytes v54 ) ) )
[<-recv] success
[send->] (assert (= v67 (msg-cat v65 v66 ) ) )
[<-recv] success
[send->] (assert (= v68 (digest v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (= v31 v68 ) ) )
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
[send->] (assert (= v70 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v72 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (assert (= v73 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (assert (= v79 (ite v75 v78 false ) ) )
[<-recv] success
[send->] (assert (= v80 (- 4 v46 ) ) )
[<-recv] success
[send->] (assert (= v81 (+ v54 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (mod v81 3 ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v78 0 1 ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v75 2 v83 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v79 v82 v84 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v88 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (assert (= v89 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v90 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v91 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v92 (ite v90 v91 false ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v89 false true ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 true v92 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v94 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (assert (= v95 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v96 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v97 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v96 v97 false ) ) )
[<-recv] success
[send->] (assert (= v99 (ite v95 false true ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 true v98 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v100 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (assert (= v101 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v102 (= v54 0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v104 (= v54 1 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v106 (= v54 2 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v109 (= v46 0 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v109 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v110 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v111 (= v46 1 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v111 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v112 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v113 (= v46 2 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v114 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v115 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v116 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v118 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v117 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v117 v118 v2 ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v115 v116 v119 ) ) )
[<-recv] success
[send->] (assert (= v122 (ite v115 0 v120 ) ) )
[<-recv] success
[send->] (assert (= v123 (+ v3 v121 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v123 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v122 ) )
[<-recv] success
[send->] (assert (= v125 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v126 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v127 (ite v125 v126 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v127 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v127 )
[<-recv] success
[send->] (assert (= v128 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v129 (= v85 1 ) ) )
[<-recv] success
[send->] (assert (= v130 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v131 (= v85 3 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v131 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v133 (ite v130 (bytes-literal 1195058296 ) v132 ) ) )
[<-recv] success
[send->] (assert (= v134 (ite v129 (bytes-literal 3502248497 ) v133 ) ) )
[<-recv] success
[send->] (assert (= v135 (ite v128 (bytes-literal 1946528133 ) v134 ) ) )
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
[send->] (assert (= v8 txn_value1 ) )
[<-recv] success
[send->] (assert (= v9 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v10 (= v8 v9 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v10 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v10 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v139 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v139 ) )
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
[send->] (assert (= v13 txn_value2 ) )
[<-recv] success
[send->] (assert (= v14 (= v13 v2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v14 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v14 )
[<-recv] success
[send->] (assert (= v16 (= v15 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v17 (= v15 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v18 (= v15 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v19 (ite v16 true v17 ) ) )
[<-recv] success
[send->] (assert (= v20 (ite v19 true v18 ) ) )
[<-recv] success
[send->] (assert v20 )
[<-recv] success
[send->] (assert (= v21 (ite v17 1 2 ) ) )
[<-recv] success
[send->] (assert (= v22 (ite v16 0 v21 ) ) )
[<-recv] success
[send->] (assert (= v23 (<= 0 v22 ) ) )
[<-recv] success
[send->] (assert (= v24 (< v22 3 ) ) )
[<-recv] success
[send->] (assert (= v25 (ite v23 v24 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v25 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (assert (= v27 (uint256->bytes v26 ) ) )
[<-recv] success
[send->] (assert (= v28 (uint256->bytes v22 ) ) )
[<-recv] success
[send->] (assert (= v29 (msg-cat v27 v28 ) ) )
[<-recv] success
[send->] (assert (= v30 (digest v29 ) ) )
[<-recv] success
[send->] (assert (= v31 v30 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v138 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v138 ) )
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
[send->] (assert (= v33 txn_value3 ) )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v34 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (assert (= v36 (= v35 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v37 (= v35 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v38 (= v35 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v39 (ite v36 true v37 ) ) )
[<-recv] success
[send->] (assert (= v40 (ite v39 true v38 ) ) )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (assert (= v41 (ite v37 1 2 ) ) )
[<-recv] success
[send->] (assert (= v42 (ite v36 0 v41 ) ) )
[<-recv] success
[send->] (assert (= v43 (<= 0 v42 ) ) )
[<-recv] success
[send->] (assert (= v44 (< v42 3 ) ) )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v45 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v45 )
[<-recv] success
[send->] (assert (= v46 v42 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v137 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v137 ) )
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
[send->] (assert (= v48 txn_value4 ) )
[<-recv] success
[send->] (assert (= v49 (= v48 0 ) ) )
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
[send->] (assert (= v50 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v51 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v52 (ite v50 v51 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v52 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v52 )
[<-recv] success
[send->] (assert (= v53 v26 ) )
[<-recv] success
[send->] (assert (= v54 v22 ) )
[<-recv] success
[send->] (assert (= v55 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v56 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v57 (ite v55 v56 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v57 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v57 )
[<-recv] success
[send->] (assert (= v58 (= v46 0 ) ) )
[<-recv] success
[send->] (assert (= v59 (= v46 1 ) ) )
[<-recv] success
[send->] (assert (= v60 (ite v59 (bytes-literal 3108938406 ) (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v61 (ite v58 (bytes-literal 31217174 ) v60 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v136 ) )
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
[send->] (assert (= v63 txn_value5 ) )
[<-recv] success
[send->] (assert (= v64 (= v63 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v64 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (assert (= v65 (uint256->bytes v53 ) ) )
[<-recv] success
[send->] (assert (= v66 (uint256->bytes v54 ) ) )
[<-recv] success
[send->] (assert (= v67 (msg-cat v65 v66 ) ) )
[<-recv] success
[send->] (assert (= v68 (digest v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (= v31 v68 ) ) )
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
[send->] (assert (= v70 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v72 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (assert (= v73 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (assert (= v79 (ite v75 v78 false ) ) )
[<-recv] success
[send->] (assert (= v80 (- 4 v46 ) ) )
[<-recv] success
[send->] (assert (= v81 (+ v54 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (mod v81 3 ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v78 0 1 ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v75 2 v83 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v79 v82 v84 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v88 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (assert (= v89 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v90 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v91 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v92 (ite v90 v91 false ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v89 false true ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 true v92 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v94 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (assert (= v95 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v96 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v97 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v96 v97 false ) ) )
[<-recv] success
[send->] (assert (= v99 (ite v95 false true ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 true v98 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v100 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (assert (= v101 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v102 (= v54 0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v104 (= v54 1 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v106 (= v54 2 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v109 (= v46 0 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v109 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v110 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v111 (= v46 1 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v111 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v112 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v113 (= v46 2 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v114 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v115 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v116 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v118 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v117 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v117 v118 v2 ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v115 v116 v119 ) ) )
[<-recv] success
[send->] (assert (= v122 (ite v115 0 v120 ) ) )
[<-recv] success
[send->] (assert (= v123 (+ v3 v121 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v123 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v122 ) )
[<-recv] success
[send->] (assert (= v125 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v126 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v127 (ite v125 v126 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v127 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v127 )
[<-recv] success
[send->] (assert (= v128 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v129 (= v85 1 ) ) )
[<-recv] success
[send->] (assert (= v130 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v131 (= v85 3 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v131 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v133 (ite v130 (bytes-literal 1195058296 ) v132 ) ) )
[<-recv] success
[send->] (assert (= v134 (ite v129 (bytes-literal 3502248497 ) v133 ) ) )
[<-recv] success
[send->] (assert (= v135 (ite v128 (bytes-literal 1946528133 ) v134 ) ) )
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
[send->] (assert (= v8 txn_value1 ) )
[<-recv] success
[send->] (assert (= v9 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v10 (= v8 v9 ) ) )
[<-recv] success
[send->] (assert v10 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v139 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v139 ) )
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
[send->] (assert (= v13 txn_value2 ) )
[<-recv] success
[send->] (assert (= v14 (= v13 v2 ) ) )
[<-recv] success
[send->] (assert v14 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v138 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v138 ) )
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
[send->] (assert (= v33 txn_value3 ) )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v137 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v137 ) )
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
[send->] (assert (= v48 txn_value4 ) )
[<-recv] success
[send->] (assert (= v49 (= v48 0 ) ) )
[<-recv] success
[send->] (assert v49 )
[<-recv] success
[send->] (assert (= v50 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v51 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v52 (ite v50 v51 false ) ) )
[<-recv] success
[send->] (assert v52 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v136 ) )
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
[send->] (assert (= v63 txn_value5 ) )
[<-recv] success
[send->] (assert (= v64 (= v63 0 ) ) )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (assert (= v65 (uint256->bytes v53 ) ) )
[<-recv] success
[send->] (assert (= v66 (uint256->bytes v54 ) ) )
[<-recv] success
[send->] (assert (= v67 (msg-cat v65 v66 ) ) )
[<-recv] success
[send->] (assert (= v68 (digest v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (= v31 v68 ) ) )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (assert (= v70 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (assert (= v73 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (assert (= v79 (ite v75 v78 false ) ) )
[<-recv] success
[send->] (assert (= v80 (- 4 v46 ) ) )
[<-recv] success
[send->] (assert (= v81 (+ v54 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (mod v81 3 ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v78 0 1 ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v75 2 v83 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v79 v82 v84 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v88 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (assert (= v89 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v90 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v91 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v92 (ite v90 v91 false ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v89 false true ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 true v92 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v94 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (assert (= v95 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v96 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v97 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v96 v97 false ) ) )
[<-recv] success
[send->] (assert (= v99 (ite v95 false true ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 true v98 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v100 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (assert (= v101 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v102 (= v54 0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v104 (= v54 1 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v106 (= v54 2 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v109 (= v46 0 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v109 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v110 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v111 (= v46 1 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v111 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v112 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v113 (= v46 2 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v114 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v115 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v116 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v118 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v117 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v117 v118 v2 ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v115 v116 v119 ) ) )
[<-recv] success
[send->] (assert (= v122 (ite v115 0 v120 ) ) )
[<-recv] success
[send->] (assert (= v123 (+ v3 v121 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v123 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v122 ) )
[<-recv] success
[send->] (assert (= v125 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v126 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v127 (ite v125 v126 false ) ) )
[<-recv] success
[send->] (assert v127 )
[<-recv] success
[send->] (assert (= v128 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v129 (= v85 1 ) ) )
[<-recv] success
[send->] (assert (= v130 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v131 (= v85 3 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v131 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v133 (ite v130 (bytes-literal 1195058296 ) v132 ) ) )
[<-recv] success
[send->] (assert (= v134 (ite v129 (bytes-literal 3502248497 ) v133 ) ) )
[<-recv] success
[send->] (assert (= v135 (ite v128 (bytes-literal 1946528133 ) v134 ) ) )
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
[send->] (assert (= v8 txn_value1 ) )
[<-recv] success
[send->] (assert (= v9 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v10 (= v8 v9 ) ) )
[<-recv] success
[send->] (assert v10 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v139 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v139 ) )
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
[send->] (assert (= v13 txn_value2 ) )
[<-recv] success
[send->] (assert (= v14 (= v13 v2 ) ) )
[<-recv] success
[send->] (assert v14 )
[<-recv] success
[send->] (assert (= v16 (= v15 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v17 (= v15 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v18 (= v15 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v19 (ite v16 true v17 ) ) )
[<-recv] success
[send->] (assert (= v20 (ite v19 true v18 ) ) )
[<-recv] success
[send->] (assert v20 )
[<-recv] success
[send->] (assert (= v21 (ite v17 1 2 ) ) )
[<-recv] success
[send->] (assert (= v22 (ite v16 0 v21 ) ) )
[<-recv] success
[send->] (assert (= v23 (<= 0 v22 ) ) )
[<-recv] success
[send->] (assert (= v24 (< v22 3 ) ) )
[<-recv] success
[send->] (assert (= v25 (ite v23 v24 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v25 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (assert (= v27 (uint256->bytes v26 ) ) )
[<-recv] success
[send->] (assert (= v28 (uint256->bytes v22 ) ) )
[<-recv] success
[send->] (assert (= v29 (msg-cat v27 v28 ) ) )
[<-recv] success
[send->] (assert (= v30 (digest v29 ) ) )
[<-recv] success
[send->] (assert (= v31 v30 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v138 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v138 ) )
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
[send->] (assert (= v33 txn_value3 ) )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v137 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v137 ) )
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
[send->] (assert (= v48 txn_value4 ) )
[<-recv] success
[send->] (assert (= v49 (= v48 0 ) ) )
[<-recv] success
[send->] (assert v49 )
[<-recv] success
[send->] (assert (= v50 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v51 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v52 (ite v50 v51 false ) ) )
[<-recv] success
[send->] (assert v52 )
[<-recv] success
[send->] (assert (= v53 v26 ) )
[<-recv] success
[send->] (assert (= v54 v22 ) )
[<-recv] success
[send->] (assert (= v55 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v56 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v57 (ite v55 v56 false ) ) )
[<-recv] success
[send->] (assert v57 )
[<-recv] success
[send->] (assert (= v58 (= v46 0 ) ) )
[<-recv] success
[send->] (assert (= v59 (= v46 1 ) ) )
[<-recv] success
[send->] (assert (= v60 (ite v59 (bytes-literal 3108938406 ) (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v61 (ite v58 (bytes-literal 31217174 ) v60 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v136 ) )
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
[send->] (assert (= v63 txn_value5 ) )
[<-recv] success
[send->] (assert (= v64 (= v63 0 ) ) )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (assert (= v65 (uint256->bytes v53 ) ) )
[<-recv] success
[send->] (assert (= v66 (uint256->bytes v54 ) ) )
[<-recv] success
[send->] (assert (= v67 (msg-cat v65 v66 ) ) )
[<-recv] success
[send->] (assert (= v68 (digest v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (= v31 v68 ) ) )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (assert (= v70 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (assert (= v73 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (assert (= v79 (ite v75 v78 false ) ) )
[<-recv] success
[send->] (assert (= v80 (- 4 v46 ) ) )
[<-recv] success
[send->] (assert (= v81 (+ v54 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (mod v81 3 ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v78 0 1 ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v75 2 v83 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v79 v82 v84 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v88 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (assert (= v89 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v90 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v91 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v92 (ite v90 v91 false ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v89 false true ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 true v92 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v94 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (assert (= v95 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v96 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v97 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v96 v97 false ) ) )
[<-recv] success
[send->] (assert (= v99 (ite v95 false true ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 true v98 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v100 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (assert (= v101 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v102 (= v54 0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v104 (= v54 1 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v106 (= v54 2 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v109 (= v46 0 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v109 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v110 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v111 (= v46 1 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v111 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v112 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v113 (= v46 2 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v114 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v115 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v116 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v118 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v117 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v117 v118 v2 ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v115 v116 v119 ) ) )
[<-recv] success
[send->] (assert (= v122 (ite v115 0 v120 ) ) )
[<-recv] success
[send->] (assert (= v123 (+ v3 v121 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v123 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v122 ) )
[<-recv] success
[send->] (assert (= v125 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v126 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v127 (ite v125 v126 false ) ) )
[<-recv] success
[send->] (assert v127 )
[<-recv] success
[send->] (assert (= v128 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v129 (= v85 1 ) ) )
[<-recv] success
[send->] (assert (= v130 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v131 (= v85 3 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v131 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v133 (ite v130 (bytes-literal 1195058296 ) v132 ) ) )
[<-recv] success
[send->] (assert (= v134 (ite v129 (bytes-literal 3502248497 ) v133 ) ) )
[<-recv] success
[send->] (assert (= v135 (ite v128 (bytes-literal 1946528133 ) v134 ) ) )
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
[send->] (assert (= v8 txn_value1 ) )
[<-recv] success
[send->] (assert (= v9 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v10 (= v8 v9 ) ) )
[<-recv] success
[send->] (assert v10 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v139 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v139 ) )
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
[send->] (assert (= v13 txn_value2 ) )
[<-recv] success
[send->] (assert (= v14 (= v13 v2 ) ) )
[<-recv] success
[send->] (assert v14 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v138 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v138 ) )
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
[send->] (assert (= v33 txn_value3 ) )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (assert (= v36 (= v35 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v37 (= v35 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v38 (= v35 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v39 (ite v36 true v37 ) ) )
[<-recv] success
[send->] (assert (= v40 (ite v39 true v38 ) ) )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (assert (= v41 (ite v37 1 2 ) ) )
[<-recv] success
[send->] (assert (= v42 (ite v36 0 v41 ) ) )
[<-recv] success
[send->] (assert (= v43 (<= 0 v42 ) ) )
[<-recv] success
[send->] (assert (= v44 (< v42 3 ) ) )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v45 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v45 )
[<-recv] success
[send->] (assert (= v46 v42 ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v137 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v137 ) )
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
[send->] (assert (= v48 txn_value4 ) )
[<-recv] success
[send->] (assert (= v49 (= v48 0 ) ) )
[<-recv] success
[send->] (assert v49 )
[<-recv] success
[send->] (assert (= v50 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v51 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v52 (ite v50 v51 false ) ) )
[<-recv] success
[send->] (assert v52 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v136 ) )
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
[send->] (assert (= v63 txn_value5 ) )
[<-recv] success
[send->] (assert (= v64 (= v63 0 ) ) )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (assert (= v65 (uint256->bytes v53 ) ) )
[<-recv] success
[send->] (assert (= v66 (uint256->bytes v54 ) ) )
[<-recv] success
[send->] (assert (= v67 (msg-cat v65 v66 ) ) )
[<-recv] success
[send->] (assert (= v68 (digest v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (= v31 v68 ) ) )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (assert (= v70 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (assert (= v73 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (assert (= v79 (ite v75 v78 false ) ) )
[<-recv] success
[send->] (assert (= v80 (- 4 v46 ) ) )
[<-recv] success
[send->] (assert (= v81 (+ v54 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (mod v81 3 ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v78 0 1 ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v75 2 v83 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v79 v82 v84 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v88 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (assert (= v89 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v90 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v91 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v92 (ite v90 v91 false ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v89 false true ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 true v92 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v94 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (assert (= v95 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v96 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v97 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v96 v97 false ) ) )
[<-recv] success
[send->] (assert (= v99 (ite v95 false true ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 true v98 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v100 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (assert (= v101 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v102 (= v54 0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v104 (= v54 1 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v106 (= v54 2 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v109 (= v46 0 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v109 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v110 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v111 (= v46 1 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v111 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v112 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v113 (= v46 2 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v114 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v115 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v116 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v118 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v117 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v117 v118 v2 ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v115 v116 v119 ) ) )
[<-recv] success
[send->] (assert (= v122 (ite v115 0 v120 ) ) )
[<-recv] success
[send->] (assert (= v123 (+ v3 v121 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v123 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v122 ) )
[<-recv] success
[send->] (assert (= v125 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v126 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v127 (ite v125 v126 false ) ) )
[<-recv] success
[send->] (assert v127 )
[<-recv] success
[send->] (assert (= v128 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v129 (= v85 1 ) ) )
[<-recv] success
[send->] (assert (= v130 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v131 (= v85 3 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v131 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v133 (ite v130 (bytes-literal 1195058296 ) v132 ) ) )
[<-recv] success
[send->] (assert (= v134 (ite v129 (bytes-literal 3502248497 ) v133 ) ) )
[<-recv] success
[send->] (assert (= v135 (ite v128 (bytes-literal 1946528133 ) v134 ) ) )
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
[send->] (assert (= v8 txn_value1 ) )
[<-recv] success
[send->] (assert (= v9 (+ v2 v3 ) ) )
[<-recv] success
[send->] (assert (= v10 (= v8 v9 ) ) )
[<-recv] success
[send->] (assert v10 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v139 ctc_balance1 ) )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (- ctc_balance1 v139 ) )
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
[send->] (assert (= v13 txn_value2 ) )
[<-recv] success
[send->] (assert (= v14 (= v13 v2 ) ) )
[<-recv] success
[send->] (assert v14 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v138 ctc_balance2 ) )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (- ctc_balance2 v138 ) )
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
[send->] (assert (= v33 txn_value3 ) )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v137 ctc_balance3 ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v137 ) )
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
[send->] (assert (= v48 txn_value4 ) )
[<-recv] success
[send->] (assert (= v49 (= v48 0 ) ) )
[<-recv] success
[send->] (assert v49 )
[<-recv] success
[send->] (assert (= v50 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v51 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v52 (ite v50 v51 false ) ) )
[<-recv] success
[send->] (assert v52 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v136 ctc_balance4 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v136 ) )
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
[send->] (assert (= v63 txn_value5 ) )
[<-recv] success
[send->] (assert (= v64 (= v63 0 ) ) )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (assert (= v65 (uint256->bytes v53 ) ) )
[<-recv] success
[send->] (assert (= v66 (uint256->bytes v54 ) ) )
[<-recv] success
[send->] (assert (= v67 (msg-cat v65 v66 ) ) )
[<-recv] success
[send->] (assert (= v68 (digest v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (= v31 v68 ) ) )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (assert (= v70 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (assert (= v73 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v74 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v75 (ite v73 v74 false ) ) )
[<-recv] success
[send->] (assert (= v76 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v77 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v78 (ite v76 v77 false ) ) )
[<-recv] success
[send->] (assert (= v79 (ite v75 v78 false ) ) )
[<-recv] success
[send->] (assert (= v80 (- 4 v46 ) ) )
[<-recv] success
[send->] (assert (= v81 (+ v54 v80 ) ) )
[<-recv] success
[send->] (assert (= v82 (mod v81 3 ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v78 0 1 ) ) )
[<-recv] success
[send->] (assert (= v84 (ite v75 2 v83 ) ) )
[<-recv] success
[send->] (assert (= v85 (ite v79 v82 v84 ) ) )
[<-recv] success
[send->] (assert (= v86 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v87 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v88 (ite v86 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v88 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (assert (= v89 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v90 (<= 0 v54 ) ) )
[<-recv] success
[send->] (assert (= v91 (< v54 3 ) ) )
[<-recv] success
[send->] (assert (= v92 (ite v90 v91 false ) ) )
[<-recv] success
[send->] (assert (= v93 (ite v89 false true ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 true v92 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v94 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (assert (= v95 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v96 (<= 0 v46 ) ) )
[<-recv] success
[send->] (assert (= v97 (< v46 3 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v96 v97 false ) ) )
[<-recv] success
[send->] (assert (= v99 (ite v95 false true ) ) )
[<-recv] success
[send->] (assert (= v100 (ite v99 true v98 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v100 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (assert (= v101 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v102 (= v54 0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v104 (= v54 1 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v106 (= v54 2 ) ) )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v108 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v109 (= v46 0 ) ) )
[<-recv] success
[send->] (assert (= v110 (ite v109 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v110 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v111 (= v46 1 ) ) )
[<-recv] success
[send->] (assert (= v112 (ite v111 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v112 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v113 (= v46 2 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 v108 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v114 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v115 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v116 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v117 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v118 (* 2 v2 ) ) )
[<-recv] success
[send->] (assert (= v119 (ite v117 0 v2 ) ) )
[<-recv] success
[send->] (assert (= v120 (ite v117 v118 v2 ) ) )
[<-recv] success
[send->] (assert (= v121 (ite v115 v116 v119 ) ) )
[<-recv] success
[send->] (assert (= v122 (ite v115 0 v120 ) ) )
[<-recv] success
[send->] (assert (= v123 (+ v3 v121 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v123 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v122 ) )
[<-recv] success
[send->] (assert (= v125 (<= 0 v85 ) ) )
[<-recv] success
[send->] (assert (= v126 (< v85 5 ) ) )
[<-recv] success
[send->] (assert (= v127 (ite v125 v126 false ) ) )
[<-recv] success
[send->] (assert v127 )
[<-recv] success
[send->] (assert (= v128 (= v85 0 ) ) )
[<-recv] success
[send->] (assert (= v129 (= v85 1 ) ) )
[<-recv] success
[send->] (assert (= v130 (= v85 2 ) ) )
[<-recv] success
[send->] (assert (= v131 (= v85 3 ) ) )
[<-recv] success
[send->] (assert (= v132 (ite v131 (bytes-literal 2585170148 ) (bytes-literal 1495867592 ) ) ) )
[<-recv] success
[send->] (assert (= v133 (ite v130 (bytes-literal 1195058296 ) v132 ) ) )
[<-recv] success
[send->] (assert (= v134 (ite v129 (bytes-literal 3502248497 ) v133 ) ) )
[<-recv] success
[send->] (assert (= v135 (ite v128 (bytes-literal 1946528133 ) v134 ) ) )
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
