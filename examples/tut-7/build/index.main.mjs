// Automatically generated with Reach 0.1.2
/* eslint-disable no-unused-vars, no-empty-pattern, no-useless-escape, no-loop-func */
export const _version = '0.1.2';


export async function Alice(ctc, interact) {
  const stdlib = ctc.stdlib;
  const v32 = await ctc.creationTime();
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  const v31 = stdlib.protect(stdlib.T_UInt, interact.wager, null);
  
  const txn1 = await (ctc.sendrecv('Alice', 1, 1, stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], [v32, v31], v31, [stdlib.T_UInt], true, true, false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0), v32]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0)]);
    const [v36] = txn1.data;
    const v37 = txn1.value;
    const v41 = txn1.time;
    const v35 = txn1.from;
    
    const v38 = stdlib.eq(v37, v36);
    stdlib.assert(v38, {
      at: './index.rsh:44:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    stdlib.assert(true, {
      at: './index.rsh:44:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v39 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
    const v40 = stdlib.add(v39, v37);
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v35, v36, v40, v41]);
    sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v35, v36, v40]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  const [v36] = txn1.data;
  const v37 = txn1.value;
  const v41 = txn1.time;
  const v35 = txn1.from;
  const v38 = stdlib.eq(v37, v36);
  stdlib.assert(v38, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  stdlib.assert(true, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v39 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
  const v40 = stdlib.add(v39, v37);
  const txn2 = await (ctc.recv('Alice', 2, 0, [], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv('Alice', 10, 0, stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 3), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v40, v41], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 1), v35, v36, v40, v41]);
      sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 1), v35, v36, v40]);
      const [] = txn3.data;
      const v194 = txn3.value;
      const v199 = txn3.time;
      const v193 = txn3.from;
      
      const v195 = stdlib.eq(v194, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v195, {
        at: 'reach standard library:67:7:dot',
        fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      const v196 = stdlib.addressEq(v35, v193);
      stdlib.assert(v196, {
        at: 'reach standard library:67:7:dot',
        fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      
      const v198 = stdlib.add(v40, v194);
      
      
      
      
      sim_r.txns.push({
        amt: v198,
        to: v35
         });
      
      
      
      
      
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.isHalt = true;
      
      return sim_r;
       })));
    const [] = txn3.data;
    const v194 = txn3.value;
    const v199 = txn3.time;
    const v193 = txn3.from;
    const v195 = stdlib.eq(v194, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v195, {
      at: 'reach standard library:67:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v196 = stdlib.addressEq(v35, v193);
    stdlib.assert(v196, {
      at: 'reach standard library:67:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
       });
    
    const v198 = stdlib.add(v40, v194);
    
    
    
    
    ;
    
    
    
    
    
    
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:70:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
       });
    return;
     }
  else {
    const [] = txn2.data;
    const v46 = txn2.value;
    const v50 = txn2.time;
    const v45 = txn2.from;
    const v47 = stdlib.eq(v46, v36);
    stdlib.assert(v47, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    stdlib.assert(true, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    
    const v49 = stdlib.add(v40, v46);
    let v51 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v214 = v49;
    let v215 = v50;
    let v216 = v41;
    
    while ((() => {
      const v61 = stdlib.eq(v51, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v61; })()) {
      
      const v64 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
        at: './index.rsh:59:42:application',
        fs: ['at ./index.rsh:58:15:application call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
         });
      const v66 = stdlib.protect(stdlib.T_UInt, await interact.random(), {
        at: 'reach standard library:59:31:application',
        fs: ['at ./index.rsh:60:52:application call to [unknown function] (defined at: reach standard library:58:8:function exp)', 'at ./index.rsh:58:15:application call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'random',
        who: 'Alice'
         });
      const v67 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v66, v64]);
      const txn3 = await (ctc.sendrecv('Alice', 4, 1, stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 4), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Digest], [v35, v36, v45, v214, v215, v67], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_Digest], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn3) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 3), v35, v36, v45, v214, v215]);
        sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 3), v35, v36, v45, v214]);
        const [v70] = txn3.data;
        const v71 = txn3.value;
        const v76 = txn3.time;
        const v69 = txn3.from;
        
        const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v72, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v73 = stdlib.addressEq(v35, v69);
        stdlib.assert(v73, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
           });
        
        const v75 = stdlib.add(v214, v71);
        
        sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 4), v35, v36, v45, v70, v75, v76]);
        sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 4), v35, v36, v45, v70, v75]);
        sim_r.isHalt = false;
        
        return sim_r;
         })));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.recv('Alice', 9, 0, [], false, false));
        const [] = txn4.data;
        const v154 = txn4.value;
        const v159 = txn4.time;
        const v153 = txn4.from;
        const v155 = stdlib.eq(v154, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v155, {
          at: 'reach standard library:67:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v156 = stdlib.addressEq(v45, v153);
        stdlib.assert(v156, {
          at: 'reach standard library:67:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
           });
        
        const v158 = stdlib.add(v214, v154);
        
        
        
        
        ;
        
        
        
        
        
        
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:70:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
          msg: 'informTimeout',
          who: 'Alice'
           });
        return;
         }
      else {
        const [v70] = txn3.data;
        const v71 = txn3.value;
        const v76 = txn3.time;
        const v69 = txn3.from;
        const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v72, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        const v73 = stdlib.addressEq(v35, v69);
        stdlib.assert(v73, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
           });
        
        const v75 = stdlib.add(v214, v71);
        
        const txn4 = await (ctc.recv('Alice', 5, 1, [stdlib.T_UInt], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv('Alice', 8, 0, stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 5), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v45, v70, v75, v76], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 4), v35, v36, v45, v70, v75, v76]);
            sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 4), v35, v36, v45, v70, v75]);
            const [] = txn5.data;
            const v132 = txn5.value;
            const v137 = txn5.time;
            const v131 = txn5.from;
            
            const v133 = stdlib.eq(v132, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v133, {
              at: 'reach standard library:67:7:dot',
              fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v134 = stdlib.addressEq(v35, v131);
            stdlib.assert(v134, {
              at: 'reach standard library:67:7:dot',
              fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            
            const v136 = stdlib.add(v75, v132);
            
            
            
            
            sim_r.txns.push({
              amt: v136,
              to: v35
               });
            
            
            
            
            
            sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
            sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
            sim_r.isHalt = true;
            
            return sim_r;
             })));
          const [] = txn5.data;
          const v132 = txn5.value;
          const v137 = txn5.time;
          const v131 = txn5.from;
          const v133 = stdlib.eq(v132, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v133, {
            at: 'reach standard library:67:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v134 = stdlib.addressEq(v35, v131);
          stdlib.assert(v134, {
            at: 'reach standard library:67:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
             });
          
          const v136 = stdlib.add(v75, v132);
          
          
          
          
          ;
          
          
          
          
          
          
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:70:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
             });
          return;
           }
        else {
          const [v81] = txn4.data;
          const v82 = txn4.value;
          const v87 = txn4.time;
          const v80 = txn4.from;
          const v83 = stdlib.eq(v82, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v83, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          const v84 = stdlib.addressEq(v45, v80);
          stdlib.assert(v84, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
             });
          
          const v86 = stdlib.add(v75, v82);
          
          const txn5 = await (ctc.sendrecv('Alice', 6, 2, stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 6), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v45, v70, v81, v86, v87, v66, v64], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 5), v35, v36, v45, v70, v81, v86, v87]);
            sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 5), v35, v36, v45, v70, v81, v86]);
            const [v91, v92] = txn5.data;
            const v93 = txn5.value;
            const v98 = txn5.time;
            const v90 = txn5.from;
            
            const v94 = stdlib.eq(v93, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v94, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v95 = stdlib.addressEq(v35, v90);
            stdlib.assert(v95, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
               });
            
            const v97 = stdlib.add(v86, v93);
            const v100 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v91, v92]);
            const v101 = stdlib.eq(v70, v100);
            stdlib.assert(v101, {
              at: 'reach standard library:64:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:63:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v104 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v81);
            const v105 = stdlib.add(v92, v104);
            const v106 = stdlib.mod(v105, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv51 = v106;
            const cv214 = v97;
            const cv215 = v98;
            const cv216 = v87;
            
            (() => {
              const v51 = cv51;
              const v214 = cv214;
              const v215 = cv215;
              const v216 = cv216;
              
              if ((() => {
                const v61 = stdlib.eq(v51, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                
                return v61; })()) {
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v35, v36, v45, v214, v215]);
                sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v35, v36, v45, v214]);
                sim_r.isHalt = false;
                 }
              else {
                const v174 = stdlib.eq(v51, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                
                
                
                const v177 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v36);
                
                const v179 = v174 ? v35 : v45;
                
                
                
                sim_r.txns.push({
                  amt: v177,
                  to: v179
                   });
                
                
                
                
                
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
                sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
                sim_r.isHalt = true;
                 } })();
            return sim_r;
             })));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.recv('Alice', 7, 0, [], false, false));
            const [] = txn6.data;
            const v110 = txn6.value;
            const v115 = txn6.time;
            const v109 = txn6.from;
            const v111 = stdlib.eq(v110, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v111, {
              at: 'reach standard library:67:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v112 = stdlib.addressEq(v45, v109);
            stdlib.assert(v112, {
              at: 'reach standard library:67:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            
            const v114 = stdlib.add(v86, v110);
            
            
            
            
            ;
            
            
            
            
            
            
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:70:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
               });
            return;
             }
          else {
            const [v91, v92] = txn5.data;
            const v93 = txn5.value;
            const v98 = txn5.time;
            const v90 = txn5.from;
            const v94 = stdlib.eq(v93, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v94, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            const v95 = stdlib.addressEq(v35, v90);
            stdlib.assert(v95, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
               });
            
            const v97 = stdlib.add(v86, v93);
            const v100 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v91, v92]);
            const v101 = stdlib.eq(v70, v100);
            stdlib.assert(v101, {
              at: 'reach standard library:64:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:63:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v104 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v81);
            const v105 = stdlib.add(v92, v104);
            const v106 = stdlib.mod(v105, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv51 = v106;
            const cv214 = v97;
            const cv215 = v98;
            const cv216 = v87;
            
            v51 = cv51;
            v214 = cv214;
            v215 = cv215;
            v216 = cv216;
            
            continue; }
           }
         }
       }
    const v174 = stdlib.eq(v51, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    
    
    
    const v177 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v36);
    
    const v179 = v174 ? v35 : v45;
    
    
    
    ;
    
    
    
    
    
    
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v51), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:86:11:application call to [unknown function] (defined at: ./index.rsh:86:23:function exp)'],
      msg: 'seeOutcome',
      who: 'Alice'
       });
    return; }
  
  
   }
export async function Bob(ctc, interact) {
  const stdlib = ctc.stdlib;
  const v32 = await ctc.creationTime();
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  const txn1 = await (ctc.recv('Bob', 1, 1, [stdlib.T_UInt], false, false));
  const [v36] = txn1.data;
  const v37 = txn1.value;
  const v41 = txn1.time;
  const v35 = txn1.from;
  const v38 = stdlib.eq(v37, v36);
  stdlib.assert(v38, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  stdlib.assert(true, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v39 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
  const v40 = stdlib.add(v39, v37);
  
  stdlib.protect(stdlib.T_Null, await interact.acceptWager(v36), {
    at: './index.rsh:49:29:application',
    fs: ['at ./index.rsh:48:13:application call to [unknown function] (defined at: ./index.rsh:48:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
     });
  const txn2 = await (ctc.sendrecv('Bob', 2, 0, stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 3), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v40, v41], v36, [], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v35, v36, v40, v41]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v35, v36, v40]);
    const [] = txn2.data;
    const v46 = txn2.value;
    const v50 = txn2.time;
    const v45 = txn2.from;
    
    const v47 = stdlib.eq(v46, v36);
    stdlib.assert(v47, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    stdlib.assert(true, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    
    const v49 = stdlib.add(v40, v46);
    const v51 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    const v214 = v49;
    const v215 = v50;
    const v216 = v41;
    
    if ((() => {
      const v61 = stdlib.eq(v51, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v61; })()) {
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v35, v36, v45, v214, v215]);
      sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v35, v36, v45, v214]);
      sim_r.isHalt = false;
       }
    else {
      const v174 = stdlib.eq(v51, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
      
      
      
      const v177 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v36);
      
      const v179 = v174 ? v35 : v45;
      
      
      
      sim_r.txns.push({
        amt: v177,
        to: v179
         });
      
      
      
      
      
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.isHalt = true;
       }
    return sim_r;
     })));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.recv('Bob', 10, 0, [], false, false));
    const [] = txn3.data;
    const v194 = txn3.value;
    const v199 = txn3.time;
    const v193 = txn3.from;
    const v195 = stdlib.eq(v194, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v195, {
      at: 'reach standard library:67:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    const v196 = stdlib.addressEq(v35, v193);
    stdlib.assert(v196, {
      at: 'reach standard library:67:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
       });
    
    const v198 = stdlib.add(v40, v194);
    
    
    
    
    ;
    
    
    
    
    
    
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:70:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
       });
    return;
     }
  else {
    const [] = txn2.data;
    const v46 = txn2.value;
    const v50 = txn2.time;
    const v45 = txn2.from;
    const v47 = stdlib.eq(v46, v36);
    stdlib.assert(v47, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    stdlib.assert(true, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    
    const v49 = stdlib.add(v40, v46);
    let v51 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    let v214 = v49;
    let v215 = v50;
    let v216 = v41;
    
    while ((() => {
      const v61 = stdlib.eq(v51, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v61; })()) {
      const txn3 = await (ctc.recv('Bob', 4, 1, [stdlib.T_Digest], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv('Bob', 9, 0, stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 4), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v45, v214, v215], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 3), v35, v36, v45, v214, v215]);
          sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 3), v35, v36, v45, v214]);
          const [] = txn4.data;
          const v154 = txn4.value;
          const v159 = txn4.time;
          const v153 = txn4.from;
          
          const v155 = stdlib.eq(v154, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v155, {
            at: 'reach standard library:67:7:dot',
            fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v156 = stdlib.addressEq(v45, v153);
          stdlib.assert(v156, {
            at: 'reach standard library:67:7:dot',
            fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          
          const v158 = stdlib.add(v214, v154);
          
          
          
          
          sim_r.txns.push({
            amt: v158,
            to: v45
             });
          
          
          
          
          
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
          sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
          sim_r.isHalt = true;
          
          return sim_r;
           })));
        const [] = txn4.data;
        const v154 = txn4.value;
        const v159 = txn4.time;
        const v153 = txn4.from;
        const v155 = stdlib.eq(v154, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v155, {
          at: 'reach standard library:67:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v156 = stdlib.addressEq(v45, v153);
        stdlib.assert(v156, {
          at: 'reach standard library:67:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
           });
        
        const v158 = stdlib.add(v214, v154);
        
        
        
        
        ;
        
        
        
        
        
        
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:70:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
          msg: 'informTimeout',
          who: 'Bob'
           });
        return;
         }
      else {
        const [v70] = txn3.data;
        const v71 = txn3.value;
        const v76 = txn3.time;
        const v69 = txn3.from;
        const v72 = stdlib.eq(v71, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v72, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        const v73 = stdlib.addressEq(v35, v69);
        stdlib.assert(v73, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
           });
        
        const v75 = stdlib.add(v214, v71);
        
        
        const v79 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
          at: './index.rsh:68:52:application',
          fs: ['at ./index.rsh:67:15:application call to [unknown function] (defined at: ./index.rsh:67:19:function exp)'],
          msg: 'getHand',
          who: 'Bob'
           });
        const txn4 = await (ctc.sendrecv('Bob', 5, 1, stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 5), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v45, v70, v75, v76, v79], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 4), v35, v36, v45, v70, v75, v76]);
          sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 4), v35, v36, v45, v70, v75]);
          const [v81] = txn4.data;
          const v82 = txn4.value;
          const v87 = txn4.time;
          const v80 = txn4.from;
          
          const v83 = stdlib.eq(v82, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v83, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v84 = stdlib.addressEq(v45, v80);
          stdlib.assert(v84, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
             });
          
          const v86 = stdlib.add(v75, v82);
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 5), v35, v36, v45, v70, v81, v86, v87]);
          sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 5), v35, v36, v45, v70, v81, v86]);
          sim_r.isHalt = false;
          
          return sim_r;
           })));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv('Bob', 8, 0, [], false, false));
          const [] = txn5.data;
          const v132 = txn5.value;
          const v137 = txn5.time;
          const v131 = txn5.from;
          const v133 = stdlib.eq(v132, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v133, {
            at: 'reach standard library:67:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v134 = stdlib.addressEq(v35, v131);
          stdlib.assert(v134, {
            at: 'reach standard library:67:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          
          const v136 = stdlib.add(v75, v132);
          
          
          
          
          ;
          
          
          
          
          
          
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:70:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
             });
          return;
           }
        else {
          const [v81] = txn4.data;
          const v82 = txn4.value;
          const v87 = txn4.time;
          const v80 = txn4.from;
          const v83 = stdlib.eq(v82, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v83, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          const v84 = stdlib.addressEq(v45, v80);
          stdlib.assert(v84, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
             });
          
          const v86 = stdlib.add(v75, v82);
          const txn5 = await (ctc.recv('Bob', 6, 2, [stdlib.T_UInt, stdlib.T_UInt], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv('Bob', 7, 0, stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 6), [stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v35, v36, v45, v70, v81, v86, v87], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 5), v35, v36, v45, v70, v81, v86, v87]);
              sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 5), v35, v36, v45, v70, v81, v86]);
              const [] = txn6.data;
              const v110 = txn6.value;
              const v115 = txn6.time;
              const v109 = txn6.from;
              
              const v111 = stdlib.eq(v110, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v111, {
                at: 'reach standard library:67:7:dot',
                fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              const v112 = stdlib.addressEq(v45, v109);
              stdlib.assert(v112, {
                at: 'reach standard library:67:7:dot',
                fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              
              const v114 = stdlib.add(v86, v110);
              
              
              
              
              sim_r.txns.push({
                amt: v114,
                to: v45
                 });
              
              
              
              
              
              sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
              sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
              sim_r.isHalt = true;
              
              return sim_r;
               })));
            const [] = txn6.data;
            const v110 = txn6.value;
            const v115 = txn6.time;
            const v109 = txn6.from;
            const v111 = stdlib.eq(v110, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v111, {
              at: 'reach standard library:67:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v112 = stdlib.addressEq(v45, v109);
            stdlib.assert(v112, {
              at: 'reach standard library:67:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            
            const v114 = stdlib.add(v86, v110);
            
            
            
            
            ;
            
            
            
            
            
            
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:39:13:application call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:70:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
               });
            return;
             }
          else {
            const [v91, v92] = txn5.data;
            const v93 = txn5.value;
            const v98 = txn5.time;
            const v90 = txn5.from;
            const v94 = stdlib.eq(v93, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v94, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            const v95 = stdlib.addressEq(v35, v90);
            stdlib.assert(v95, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
               });
            
            const v97 = stdlib.add(v86, v93);
            const v100 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v91, v92]);
            const v101 = stdlib.eq(v70, v100);
            stdlib.assert(v101, {
              at: 'reach standard library:64:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:63:8:function exp)'],
              msg: null,
              who: 'Bob'
               });
            const v104 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v81);
            const v105 = stdlib.add(v92, v104);
            const v106 = stdlib.mod(v105, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv51 = v106;
            const cv214 = v97;
            const cv215 = v98;
            const cv216 = v87;
            
            v51 = cv51;
            v214 = cv214;
            v215 = cv215;
            v216 = cv216;
            
            continue; }
           }
         }
       }
    const v174 = stdlib.eq(v51, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    
    
    
    const v177 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v36);
    
    const v179 = v174 ? v35 : v45;
    
    
    
    ;
    
    
    
    
    
    
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v51), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:86:11:application call to [unknown function] (defined at: ./index.rsh:86:23:function exp)'],
      msg: 'seeOutcome',
      who: 'Bob'
       });
    return; }
  
  
   }

const _ALGO = {
  appApproval: `#pragma version 2
// Check that we're an App
txn TypeEnum
int appl
==
assert
txn RekeyTo
global ZeroAddress
==
assert
// Check that everyone's here
global GroupSize
int 4
>=
assert
// Check txnAppl (us)
txn GroupIndex
int 0
==
assert
// Check txnFromHandler
int 0
gtxn 2 Sender
byte "{{m1}}"
==
||
gtxn 2 Sender
byte "{{m2}}"
==
||
gtxn 2 Sender
byte "{{m4}}"
==
||
gtxn 2 Sender
byte "{{m5}}"
==
||
gtxn 2 Sender
byte "{{m6}}"
==
||
gtxn 2 Sender
byte "{{m7}}"
==
||
gtxn 2 Sender
byte "{{m8}}"
==
||
gtxn 2 Sender
byte "{{m9}}"
==
||
gtxn 2 Sender
byte "{{m10}}"
==
||
assert
byte base64(cw==)
app_global_get
gtxna 2 Args 0
==
assert
byte base64(bA==)
app_global_get
gtxna 2 Args 4
btoi
==
assert
// Don't check anyone else, because Handler does
// Update state
byte base64(cw==)
gtxna 2 Args 1
app_global_put
byte base64(bA==)
global Round
app_global_put
byte base64(aA==)
gtxna 2 Args 2
btoi
app_global_put
byte base64(aA==)
app_global_get
bnz halted
txn OnCompletion
int NoOp
==
assert
b done
halted:
txn OnCompletion
int DeleteApplication
==
assert
done:
int 1
return
`,
  appApproval0: `#pragma version 2
// Check that we're an App
txn TypeEnum
int appl
==
assert
txn RekeyTo
global ZeroAddress
==
assert
txn Sender
byte "{{Deployer}}"
==
assert
txn ApplicationID
bz init
global GroupSize
int 11
==
assert
txn OnCompletion
int UpdateApplication
==
assert
byte base64(cw==)
// compute state in HM_Set
int 0
itob
keccak256
app_global_put
byte base64(bA==)
global Round
app_global_put
byte base64(aA==)
int 0
app_global_put
b done
init:
global GroupSize
int 1
==
assert
txn OnCompletion
int NoOp
==
assert
done:
int 1
return
`,
  appClear: `#pragma version 2
// We're alone
global GroupSize
int 1
==
assert
// We're halted
byte base64(aA==)
app_global_get
int 1
==
assert
done:
int 1
return
`,
  ctc: `#pragma version 2
// Check size
global GroupSize
int 4
>=
assert
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Don't check anything else, because app does
// Check us
txn TypeEnum
int pay
==
assert
txn RekeyTo
global ZeroAddress
==
assert
txn CloseRemainderTo
global ZeroAddress
==
assert
txn GroupIndex
int 4
>=
assert
done:
int 1
return
`,
  stepargs: [0, 89, 129, 0, 193, 201, 217, 201, 193, 161, 129],
  steps: [null, `#pragma version 2
// Handler 1
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
txn NumArgs
int 6
==
assert
// compute state in HM_Check 0
int 0
itob
keccak256
arg 0
==
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:44:9:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
arg 5
btoi
==
assert
// Just "sender correct"
// "./index.rsh:44:9:dot"
// "[]"
int 1
assert
int 0
gtxn 3 Amount
arg 3
btoi
-
+
store 255
// compute state in HM_Set
int 1
itob
gtxn 3 Sender
concat
arg 5
concat
load 255
itob
concat
keccak256
arg 1
==
assert
arg 2
btoi
int 0
==
assert
b done
// Check GroupSize
global GroupSize
int 4
==
assert
arg 3
btoi
int 0
==
assert
// Check time limits
done:
int 1
return
`, `#pragma version 2
// Handler 2
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
txn NumArgs
int 8
==
assert
// compute state in HM_Check 1
int 1
itob
arg 5
concat
arg 6
concat
arg 7
concat
keccak256
arg 0
==
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:50:9:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
arg 6
btoi
==
assert
// Just "sender correct"
// "./index.rsh:50:9:dot"
// "[]"
int 1
assert
arg 7
btoi
gtxn 3 Amount
arg 3
btoi
-
+
store 255
int 1
int 1
==
bz l0
// compute state in HM_Set
int 3
itob
arg 5
concat
arg 6
concat
gtxn 3 Sender
concat
load 255
itob
concat
keccak256
arg 1
==
assert
arg 2
btoi
int 0
==
assert
b done
l0:
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
int 1
int 2
==
arg 5
gtxn 3 Sender
ite
==
assert
gtxn 4 Amount
int 2
arg 6
btoi
*
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
arg 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 5
==
assert
arg 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
arg 4
btoi
int 10
+
dup
gtxn 0 LastValid
==
assert
dup
gtxn 1 LastValid
==
assert
dup
gtxn 2 LastValid
==
assert
dup
gtxn 3 LastValid
==
assert
dup
gtxn 4 LastValid
==
assert
pop
done:
int 1
return
`, null, `#pragma version 2
// Handler 4
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
txn NumArgs
int 10
==
assert
// compute state in HM_Check 3
int 3
itob
arg 5
concat
arg 6
concat
arg 7
concat
arg 8
concat
keccak256
arg 0
==
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:62:11:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./index.rsh:62:11:dot"
// "[]"
arg 5
gtxn 3 Sender
==
assert
arg 8
btoi
gtxn 3 Amount
arg 3
btoi
-
+
store 255
// compute state in HM_Set
int 4
itob
arg 5
concat
arg 6
concat
arg 7
concat
arg 9
concat
load 255
itob
concat
keccak256
arg 1
==
assert
arg 2
btoi
int 0
==
assert
b done
// Check GroupSize
global GroupSize
int 4
==
assert
arg 3
btoi
int 0
==
assert
// Check time limits
arg 4
btoi
int 10
+
dup
gtxn 0 LastValid
==
assert
dup
gtxn 1 LastValid
==
assert
dup
gtxn 2 LastValid
==
assert
dup
gtxn 3 LastValid
==
assert
pop
done:
int 1
return
`, `#pragma version 2
// Handler 5
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
txn NumArgs
int 11
==
assert
// compute state in HM_Check 4
int 4
itob
arg 5
concat
arg 6
concat
arg 7
concat
arg 8
concat
arg 9
concat
keccak256
arg 0
==
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:69:11:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./index.rsh:69:11:dot"
// "[]"
arg 7
gtxn 3 Sender
==
assert
arg 9
btoi
gtxn 3 Amount
arg 3
btoi
-
+
store 255
// compute state in HM_Set
int 5
itob
arg 5
concat
arg 6
concat
arg 7
concat
arg 8
concat
arg 10
concat
load 255
itob
concat
keccak256
arg 1
==
assert
arg 2
btoi
int 0
==
assert
b done
// Check GroupSize
global GroupSize
int 4
==
assert
arg 3
btoi
int 0
==
assert
// Check time limits
arg 4
btoi
int 10
+
dup
gtxn 0 LastValid
==
assert
dup
gtxn 1 LastValid
==
assert
dup
gtxn 2 LastValid
==
assert
dup
gtxn 3 LastValid
==
assert
pop
done:
int 1
return
`, `#pragma version 2
// Handler 6
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
txn NumArgs
int 13
==
assert
// compute state in HM_Check 5
int 5
itob
arg 5
concat
arg 6
concat
arg 7
concat
arg 8
concat
arg 9
concat
arg 10
concat
keccak256
arg 0
==
assert
// Run body
// Just "pay amount correct"
// "./index.rsh:75:11:dot"
// "[]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "./index.rsh:75:11:dot"
// "[]"
arg 5
gtxn 3 Sender
==
assert
// Nothing
// "reach standard library:64:17:application"
// "[at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:63:8:function exp)]"
arg 8
arg 11
arg 12
concat
keccak256
==
assert
arg 10
btoi
gtxn 3 Amount
arg 3
btoi
-
+
store 255
arg 12
btoi
int 4
arg 9
btoi
-
+
int 3
%
dup
store 254
int 1
==
bz l0
// compute state in HM_Set
int 3
itob
arg 5
concat
arg 6
concat
arg 7
concat
load 255
itob
concat
keccak256
arg 1
==
assert
arg 2
btoi
int 0
==
assert
b done
l0:
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
load 254
int 2
==
arg 5
arg 7
ite
==
assert
gtxn 4 Amount
int 2
arg 6
btoi
*
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
arg 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 5
==
assert
arg 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
arg 4
btoi
int 10
+
dup
gtxn 0 LastValid
==
assert
dup
gtxn 1 LastValid
==
assert
dup
gtxn 2 LastValid
==
assert
dup
gtxn 3 LastValid
==
assert
dup
gtxn 4 LastValid
==
assert
pop
done:
int 1
return
`, `#pragma version 2
// Handler 7
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
txn NumArgs
int 11
==
assert
// compute state in HM_Check 5
int 5
itob
arg 5
concat
arg 6
concat
arg 7
concat
arg 8
concat
arg 9
concat
arg 10
concat
keccak256
arg 0
==
assert
// Run body
// Just "pay amount correct"
// "reach standard library:67:7:dot"
// "[at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:67:7:dot"
// "[at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)]"
arg 7
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 7
==
assert
gtxn 4 Amount
arg 10
btoi
gtxn 3 Amount
arg 3
btoi
-
+
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
arg 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 5
==
assert
arg 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
arg 4
btoi
int 10
+
dup
gtxn 0 FirstValid
==
assert
dup
gtxn 1 FirstValid
==
assert
dup
gtxn 2 FirstValid
==
assert
dup
gtxn 3 FirstValid
==
assert
dup
gtxn 4 FirstValid
==
assert
pop
done:
int 1
return
`, `#pragma version 2
// Handler 8
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
txn NumArgs
int 10
==
assert
// compute state in HM_Check 4
int 4
itob
arg 5
concat
arg 6
concat
arg 7
concat
arg 8
concat
arg 9
concat
keccak256
arg 0
==
assert
// Run body
// Just "pay amount correct"
// "reach standard library:67:7:dot"
// "[at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:67:7:dot"
// "[at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)]"
arg 5
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 5
==
assert
gtxn 4 Amount
arg 9
btoi
gtxn 3 Amount
arg 3
btoi
-
+
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
arg 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 5
==
assert
arg 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
arg 4
btoi
int 10
+
dup
gtxn 0 FirstValid
==
assert
dup
gtxn 1 FirstValid
==
assert
dup
gtxn 2 FirstValid
==
assert
dup
gtxn 3 FirstValid
==
assert
dup
gtxn 4 FirstValid
==
assert
pop
done:
int 1
return
`, `#pragma version 2
// Handler 9
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
txn NumArgs
int 9
==
assert
// compute state in HM_Check 3
int 3
itob
arg 5
concat
arg 6
concat
arg 7
concat
arg 8
concat
keccak256
arg 0
==
assert
// Run body
// Just "pay amount correct"
// "reach standard library:67:7:dot"
// "[at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:67:7:dot"
// "[at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:66:8:function exp)]"
arg 7
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 7
==
assert
gtxn 4 Amount
arg 8
btoi
gtxn 3 Amount
arg 3
btoi
-
+
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
arg 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 5
==
assert
arg 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
arg 4
btoi
int 10
+
dup
gtxn 0 FirstValid
==
assert
dup
gtxn 1 FirstValid
==
assert
dup
gtxn 2 FirstValid
==
assert
dup
gtxn 3 FirstValid
==
assert
dup
gtxn 4 FirstValid
==
assert
pop
done:
int 1
return
`, `#pragma version 2
// Handler 10
// Check txnAppl
gtxn 0 TypeEnum
int appl
==
assert
gtxn 0 ApplicationID
byte "{{ApplicationID}}"
btoi
==
assert
// Check txnToHandler
gtxn 1 TypeEnum
int pay
==
assert
gtxn 1 Receiver
txn Sender
==
assert
gtxn 1 Amount
gtxn 2 Fee
==
assert
// Check txnToContract
gtxn 3 TypeEnum
int pay
==
assert
gtxn 3 Receiver
byte "{{ContractAddr}}"
==
assert
// Check txnFromHandler (us)
txn GroupIndex
int 2
==
assert
txn TypeEnum
int pay
==
assert
txn Amount
int 0
==
assert
txn Receiver
gtxn 1 Sender
==
assert
txn NumArgs
int 8
==
assert
// compute state in HM_Check 1
int 1
itob
arg 5
concat
arg 6
concat
arg 7
concat
keccak256
arg 0
==
assert
// Run body
// Just "pay amount correct"
// "reach standard library:67:7:dot"
// "[at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:66:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
// Just "sender correct"
// "reach standard library:67:7:dot"
// "[at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:66:8:function exp)]"
arg 5
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 5
==
assert
gtxn 4 Amount
arg 7
btoi
gtxn 3 Amount
arg 3
btoi
-
+
==
assert
gtxn 4 Sender
byte "{{ContractAddr}}"
==
assert
arg 2
btoi
int 1
==
assert
b done
// Check GroupSize
global GroupSize
int 5
==
assert
arg 3
btoi
gtxn 4 Fee
==
assert
// Check time limits
arg 4
btoi
int 10
+
dup
gtxn 0 FirstValid
==
assert
dup
gtxn 1 FirstValid
==
assert
dup
gtxn 2 FirstValid
==
assert
dup
gtxn 3 FirstValid
==
assert
dup
gtxn 4 FirstValid
==
assert
pop
done:
int 1
return
`],
  unsupported: false
   };
const _ETH = {
  ABI: `[
  {
    "inputs": [],
    "stateMutability": "payable",
    "type": "constructor"
  },
  {
    "anonymous": false,
    "inputs": [],
    "name": "e0",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v32",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a0postsvs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a1msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a1",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e1",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v40",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a1postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a10",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e10",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v40",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a1postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a2",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e2",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v214",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v215",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a3postsvs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v70",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a4msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a4",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e4",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v70",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v75",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v76",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a4postsvs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v81",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a5msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a5",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e5",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v70",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v81",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v86",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v87",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a5postsvs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v91",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v92",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a6msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a6",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e6",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v70",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v81",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v86",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v87",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a5postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a7",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e7",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v70",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v75",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v76",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a4postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a8",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e8",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v214",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v215",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a3postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "indexed": false,
        "internalType": "struct ReachContract.a9",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "e9",
    "type": "event"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v32",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a0postsvs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a1msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.a1",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m1",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v40",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a1postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.a10",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m10",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v40",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a1postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.a2",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m2",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v214",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v215",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a3postsvs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v70",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a4msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.a4",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m4",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v70",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v75",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v76",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a4postsvs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v81",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a5msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.a5",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m5",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v70",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v81",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v86",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v87",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a5postsvs",
            "name": "svs",
            "type": "tuple"
          },
          {
            "components": [
              {
                "internalType": "uint256",
                "name": "v91",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v92",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a6msg",
            "name": "msg",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.a6",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m6",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v70",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v81",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v86",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v87",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a5postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.a7",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m7",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v70",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v75",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v76",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a4postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.a8",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m8",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "components": [
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v214",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v215",
                "type": "uint256"
              }
            ],
            "internalType": "struct ReachContract.a3postsvs",
            "name": "svs",
            "type": "tuple"
          }
        ],
        "internalType": "struct ReachContract.a9",
        "name": "_a",
        "type": "tuple"
      }
    ],
    "name": "m9",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  }
]`,
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a161003461007a565b43815261003f61007a565b8151815260405161005790600090839060200161008d565b60408051601f1981840301815291905280516020909101206000555061009c9050565b6040518060200160405280600081525090565b91825251602082015260400190565b61139f806100ab6000396000f3fe6080604052600436106100865760003560e01c80639532ef01116100595780639532ef01146100d9578063a60cb28b146100ec578063b928f166146100ff578063b97c0f1e14610112578063bfdc69f61461012557610086565b80631c2947e31461008b57806323d1d605146100a057806355f3fcdf146100b35780638b513b1f146100c6575b600080fd5b61009e610099366004610f2a565b610138565b005b61009e6100ae366004610f0f565b6102f2565b61009e6100c1366004610f0f565b610464565b61009e6100d4366004610ec8565b610583565b61009e6100e7366004610ee3565b61069a565b61009e6100fa366004610f3c565b610767565b61009e61010d366004610ef4565b610877565b61009e610120366004610ec8565b610981565b61009e610133366004610ef4565b610a8b565b60405161014c906005908390602001611260565b6040516020818303038152906040528051906020012060001c6000541461017257600080fd5b610181600a60c08301356112e5565b431061018c57600080fd5b341561019757600080fd5b336101a56020830183610ea7565b6001600160a01b0316146101b857600080fd5b6040516101d49060e083013590610100840135906020016112d7565b60408051601f1981840301815291905280516020909101206060820135146101fb57600080fd5b7ff9573e675c9bc3a16b419c409b77495ceab1a0967da38c45725a0e3a3d710d258160405161022a91906110e2565b60405180910390a161023a610cc9565b6102476020830183610ea7565b81516001600160a01b03909116905280516020808401359101526102716060830160408401610ea7565b81516001600160a01b0390911660409091015260036102956080840135600461131c565b6102a4906101008501356112e5565b6102ae9190611333565b6020820151526102c23460a08401356112e5565b60208083018051909101919091528051436040909101525160c08301356060909101526102ee81610bd0565b5050565b6040516103069060049083906020016111f5565b6040516020818303038152906040528051906020012060001c6000541461032c57600080fd5b610334610cee565b610343600a60a08401356112e5565b431061034e57600080fd5b341561035957600080fd5b3361036a6060840160408501610ea7565b6001600160a01b03161461037d57600080fd5b61038b3460808401356112e5565b81526040517f07e87f433158c49148bc85908421c64bbd90c439998a99b53ad987517b07d12e906103bd9084906110c6565b60405180910390a16103cd610d01565b6103da6020840184610ea7565b6001600160a01b03168152602080840135908201526103ff6060840160408501610ea7565b6001600160a01b03166040808301919091526060808501359083015260c0808501356080840152835160a0840152439083015251610444906005908390602001611275565b60408051601f198184030181529190528051602090910120600055505050565b604051610478906005908390602001611260565b6040516020818303038152906040528051906020012060001c6000541461049e57600080fd5b6104ad600a60c08301356112e5565b43101580156104ba575060015b6104c357600080fd5b34156104ce57600080fd5b336104df6060830160408401610ea7565b6001600160a01b0316146104f257600080fd5b6105026060820160408301610ea7565b6001600160a01b03166108fc61051c3460a08501356112e5565b6040518115909202916000818181858888f19350505050158015610544573d6000803e3d6000fd5b507f7ed031221b39c7cece29fe3483202ab9dd5e9229499f90125a41871b667131a881604051610574919061110d565b60405180910390a16000805533ff5b604051610597906001908390602001611146565b6040516020818303038152906040528051906020012060001c600054146105bd57600080fd5b6105cc600a60608301356112e5565b43106105d757600080fd5b346020820135146105e757600080fd5b7f1b6c319555642e7ccd14c9ec83bd3a26d9469f9b1c26d4fa430499a5eb7ec09c81604051610616919061107f565b60405180910390a1610626610cc9565b6106336020830183610ea7565b81516001600160a01b03909116905280516020808401359181019190915281513360409182015290820151600190526106709034908401356112e5565b6020808301805190910191909152805143604090910152516060808401359101526102ee81610bd0565b6040516106ae906000908390602001611137565b6040516020818303038152906040528051906020012060001c600054146106d457600080fd5b6106dc610cee565b346020830135146106ec57600080fd5b6106f73460006112e5565b81526040517ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc16690610729908490611093565b60405180910390a1610739610d50565b338152602080840135818301528251604080840191909152436060840152516104449160019184910161115a565b60405161077b906003908390602001611196565b6040516020818303038152906040528051906020012060001c600054146107a157600080fd5b6107b0600a60808301356112e5565b43101580156107bd575060015b6107c657600080fd5b34156107d157600080fd5b336107e26060830160408401610ea7565b6001600160a01b0316146107f557600080fd5b6108056060820160408301610ea7565b6001600160a01b03166108fc61081f3460608501356112e5565b6040518115909202916000818181858888f19350505050158015610847573d6000803e3d6000fd5b507f5234c2e5474a4b3ee203f8e9bc7a6b9165f1e184026f2ecdf72ea9b0d711924e816040516105749190611129565b60405161088b9060049083906020016111f5565b6040516020818303038152906040528051906020012060001c600054146108b157600080fd5b6108c0600a60a08301356112e5565b43101580156108cd575060015b6108d657600080fd5b34156108e157600080fd5b336108ef6020830183610ea7565b6001600160a01b03161461090257600080fd5b61090f6020820182610ea7565b6001600160a01b03166108fc6109293460808501356112e5565b6040518115909202916000818181858888f19350505050158015610951573d6000803e3d6000fd5b507fbe5b7e1448066eef48ef80b637b416050858d2b00390b046215df248ad038aff81604051610574919061111b565b604051610995906001908390602001611146565b6040516020818303038152906040528051906020012060001c600054146109bb57600080fd5b6109ca600a60608301356112e5565b43101580156109d7575060015b6109e057600080fd5b34156109eb57600080fd5b336109f96020830183610ea7565b6001600160a01b031614610a0c57600080fd5b610a196020820182610ea7565b6001600160a01b03166108fc610a333460408501356112e5565b6040518115909202916000818181858888f19350505050158015610a5b573d6000803e3d6000fd5b507f2eb160a2b43a7b56d73a5bf2556c2a4ee41a4ce71ad77f3ba1774567293583ff81604051610574919061107f565b604051610a9f906003908390602001611196565b6040516020818303038152906040528051906020012060001c60005414610ac557600080fd5b610acd610cee565b610adc600a60808401356112e5565b4310610ae757600080fd5b3415610af257600080fd5b33610b006020840184610ea7565b6001600160a01b031614610b1357600080fd5b610b213460608401356112e5565b81526040517f3d182e6f2fea74a81e189829571ba90ae445412add5653bfab12c4bdf626347e90610b539084906110aa565b60405180910390a1610b63610d81565b610b706020840184610ea7565b6001600160a01b0316815260208084013590820152610b956060840160408501610ea7565b6001600160a01b031660408083019190915260a080850135606084015283516080840152439083015251610444906004908390602001611209565b60208101515160011415610c5957610be6610dc9565b8151516001600160a01b039081168252825160209081015181840152835160409081015190921682840152808401805182015160608501525182015160808401529051610c38916003918491016111aa565b60408051601f19818403018152919052805160209091012060005550610cc6565b602081015151600214610c7157805160400151610c75565b8051515b6001600160a01b03166108fc8260000151602001516002610c9691906112fd565b6040518115909202916000818181858888f19350505050158015610cbe573d6000803e3d6000fd5b506000805533ff5b50565b6040518060400160405280610cdc610e0a565b8152602001610ce9610e2a565b905290565b6040518060200160405280600081525090565b6040518060e0016040528060006001600160a01b031681526020016000815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b604051806080016040528060006001600160a01b031681526020016000815260200160008152602001600081525090565b6040518060c0016040528060006001600160a01b031681526020016000815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b6040518060a0016040528060006001600160a01b031681526020016000815260200160006001600160a01b0316815260200160008152602001600081525090565b604080516060810182526000808252602082018190529181019190915290565b6040518060800160405280600081526020016000815260200160008152602001600081525090565b80356001600160a01b0381168114610e6957600080fd5b919050565b600060808284031215610e7f578081fd5b50919050565b600060c08284031215610e7f578081fd5b600060e08284031215610e7f578081fd5b600060208284031215610eb8578081fd5b610ec182610e52565b9392505050565b600060808284031215610ed9578081fd5b610ec18383610e6e565b600060408284031215610e7f578081fd5b600060c08284031215610f05578081fd5b610ec18383610e85565b600060e08284031215610f20578081fd5b610ec18383610e96565b60006101208284031215610e7f578081fd5b600060a08284031215610e7f578081fd5b6001600160a01b03610f5e82610e52565b1682526020810135602083015260408101356040830152606081013560608301525050565b6001600160a01b0380610f9583610e52565b1683526020820135602084015280610faf60408401610e52565b1660408401525060608181013590830152608090810135910152565b6001600160a01b0380610fdd83610e52565b1683526020820135602084015280610ff760408401610e52565b16604084015250606081013560608301526080810135608083015260a081013560a08301525050565b6001600160a01b038061103283610e52565b168352602082013560208401528061104c60408401610e52565b16604084015250606081013560608301526080810135608083015260a081013560a083015260c081013560c08301525050565b6080810161108d8284610f4d565b92915050565b813581526020918201359181019190915260400190565b60c081016110b88284610f83565b60a092830135919092015290565b60e081016110d48284610fcb565b60c092830135919092015290565b61012081016110f18284611020565b60e083013560e083015261010080840135818401525092915050565b60e0810161108d8284611020565b60c0810161108d8284610fcb565b60a0810161108d8284610f83565b91825235602082015260400190565b82815260a08101610ec16020830184610f4d565b91825280516001600160a01b03166020808401919091528101516040808401919091528101516060808401919091520151608082015260a00190565b82815260c08101610ec16020830184610f83565b91825280516001600160a01b0390811660208085019190915282015160408085019190915282015116606080840191909152810151608080840191909152015160a082015260c00190565b82815260e08101610ec16020830184610fcb565b600060e08201905083825260018060a01b03808451166020840152602084015160408401528060408501511660608401525060608301516080830152608083015160a083015260a083015160c08301529392505050565b8281526101008101610ec16020830184611020565b60006101008201905083825260018060a01b03808451166020840152602084015160408401528060408501511660608401525060608301516080830152608083015160a083015260a083015160c083015260c083015160e08301529392505050565b918252602082015260400190565b600082198211156112f8576112f8611353565b500190565b600081600019048311821515161561131757611317611353565b500290565b60008282101561132e5761132e611353565b500390565b60008261134e57634e487b7160e01b81526012600452602481fd5b500690565b634e487b7160e01b600052601160045260246000fdfea2646970667358221220f7859930b1d56fd3d5c6c827bcd33be756a10d6e1531104611954dfa52d9268b64736f6c63430008000033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

