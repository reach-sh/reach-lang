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
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v41, v36, v40, v35]);
    sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v36, v40, v35]);
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
    const txn3 = await (ctc.sendrecv('Alice', 10, 0, stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address], [v41, v36, v40, v35], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 1), v41, v36, v40, v35]);
      sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 1), v36, v40, v35]);
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
    let v214 = v49;
    let v216 = v41;
    let v215 = v50;
    let v51 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
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
      const txn3 = await (ctc.sendrecv('Alice', 4, 1, stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 4), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Digest], [v36, v35, v45, v214, v215, v67], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_Digest], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn3) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 3), v36, v35, v45, v214, v215]);
        sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 3), v36, v35, v45, v214]);
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
        
        sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 4), v36, v35, v45, v76, v70, v75]);
        sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 4), v36, v35, v45, v70, v75]);
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
          const txn5 = await (ctc.sendrecv('Alice', 8, 0, stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 3), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt], [v36, v35, v45, v76, v70, v75], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 4), v36, v35, v45, v76, v70, v75]);
            sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 4), v36, v35, v45, v70, v75]);
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
          
          const txn5 = await (ctc.sendrecv('Alice', 6, 2, stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 4), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v36, v35, v45, v70, v87, v81, v86, v66, v64], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 5), v36, v35, v45, v70, v87, v81, v86]);
            sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 5), v36, v35, v45, v70, v81, v86]);
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
            const cv214 = v97;
            const cv216 = v87;
            const cv215 = v98;
            const cv51 = v106;
            
            (() => {
              const v214 = cv214;
              const v216 = cv216;
              const v215 = cv215;
              const v51 = cv51;
              
              if ((() => {
                const v61 = stdlib.eq(v51, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                
                return v61; })()) {
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v36, v35, v45, v214, v215]);
                sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v36, v35, v45, v214]);
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
            const cv214 = v97;
            const cv216 = v87;
            const cv215 = v98;
            const cv51 = v106;
            
            v214 = cv214;
            v216 = cv216;
            v215 = cv215;
            v51 = cv51;
            
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
  const txn2 = await (ctc.sendrecv('Bob', 2, 0, stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address], [v41, v36, v40, v35], v36, [], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v41, v36, v40, v35]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v36, v40, v35]);
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
    const v214 = v49;
    const v216 = v41;
    const v215 = v50;
    const v51 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    if ((() => {
      const v61 = stdlib.eq(v51, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v61; })()) {
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v36, v35, v45, v214, v215]);
      sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v36, v35, v45, v214]);
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
    let v214 = v49;
    let v216 = v41;
    let v215 = v50;
    let v51 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    while ((() => {
      const v61 = stdlib.eq(v51, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v61; })()) {
      const txn3 = await (ctc.recv('Bob', 4, 1, [stdlib.T_Digest], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv('Bob', 9, 0, stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 4), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt], [v36, v35, v45, v214, v215], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 3), v36, v35, v45, v214, v215]);
          sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 3), v36, v35, v45, v214]);
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
        const txn4 = await (ctc.sendrecv('Bob', 5, 1, stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 3), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt], [v36, v35, v45, v76, v70, v75, v79], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 4), v36, v35, v45, v76, v70, v75]);
          sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 4), v36, v35, v45, v70, v75]);
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
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 5), v36, v35, v45, v70, v87, v81, v86]);
          sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 5), v36, v35, v45, v70, v81, v86]);
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
            const txn6 = await (ctc.sendrecv('Bob', 7, 0, stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 4), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v36, v35, v45, v70, v87, v81, v86], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 5), v36, v35, v45, v70, v87, v81, v86]);
              sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:67:7:dot', stdlib.UInt_max, 5), v36, v35, v45, v70, v81, v86]);
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
            const cv214 = v97;
            const cv216 = v87;
            const cv215 = v98;
            const cv51 = v106;
            
            v214 = cv214;
            v216 = cv216;
            v215 = cv215;
            v51 = cv51;
            
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
arg 5
concat
load 255
itob
concat
gtxn 3 Sender
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
arg 5
btoi
==
assert
// Just "sender correct"
// "./index.rsh:50:9:dot"
// "[]"
int 1
assert
arg 6
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
arg 7
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
arg 7
gtxn 3 Sender
ite
==
assert
gtxn 4 Amount
int 2
arg 5
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
arg 6
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
arg 6
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
arg 12
btoi
int 4
arg 9
btoi
-
+
int 3
%
store 255
arg 10
btoi
gtxn 3 Amount
arg 3
btoi
-
+
store 254
load 255
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
load 254
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
load 255
int 2
==
arg 6
arg 7
ite
==
assert
gtxn 4 Amount
int 2
arg 5
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
arg 6
gtxn 3 Sender
==
assert
gtxn 4 TypeEnum
int pay
==
assert
gtxn 4 Receiver
arg 6
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
arg 6
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
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
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
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
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
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v76",
                "type": "uint256"
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
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
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
                "name": "v87",
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
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
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
                "name": "v87",
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
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v76",
                "type": "uint256"
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
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
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
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
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
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
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
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v76",
                "type": "uint256"
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
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
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
                "name": "v87",
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
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
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
                "name": "v87",
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
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v45",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v76",
                "type": "uint256"
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
                "internalType": "uint256",
                "name": "v36",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v35",
                "type": "address"
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a161003461007a565b43815261003f61007a565b8151815260405161005790600090839060200161008d565b60408051601f1981840301815291905280516020909101206000555061009c9050565b6040518060200160405280600081525090565b91825251602082015260400190565b6113b0806100ab6000396000f3fe6080604052600436106100865760003560e01c8063b43649a911610059578063b43649a9146100d9578063b8bc1e39146100ec578063d7f6b4cd146100ff578063e2fc8b4b14610112578063eeda42ac1461012557610086565b80633c1726a31461008b5780633e1037b1146100a0578063905436cf146100b35780639532ef01146100c6575b600080fd5b61009e610099366004610f45565b610138565b005b61009e6100ae366004610f18565b610257565b61009e6100c1366004610ed1565b6103c9565b61009e6100d4366004610eec565b6104dd565b61009e6100e7366004610ed1565b6105aa565b61009e6100fa366004610efd565b6106b7565b61009e61010d366004610f18565b6107ff565b61009e610120366004610efd565b61090f565b61009e610133366004610f33565b610a1f565b60405161014c9060039083906020016111a5565b6040516020818303038152906040528051906020012060001c6000541461017257600080fd5b610181600a60808301356112f6565b431015801561018e575060015b61019757600080fd5b34156101a257600080fd5b336101b36060830160408401610eb0565b6001600160a01b0316146101c657600080fd5b6101d66060820160408301610eb0565b6001600160a01b03166108fc6101f03460608501356112f6565b6040518115909202916000818181858888f19350505050158015610218573d6000803e3d6000fd5b507f893e8b5e3088e38bfe39d4bd08bb72efde6587ca2c04cfca2f91b997377e7f38816040516102489190611138565b60405180910390a16000805533ff5b60405161026b906004908390602001611204565b6040516020818303038152906040528051906020012060001c6000541461029157600080fd5b610299610cd2565b6102a8600a60608401356112f6565b43106102b357600080fd5b34156102be57600080fd5b336102cf6060840160408501610eb0565b6001600160a01b0316146102e257600080fd5b6102f03460a08401356112f6565b81526040517f8b364c1cec79e889c00d6309a0d6316af59862f7883ed242d587454e9f375b08906103229084906110d5565b60405180910390a1610332610ce5565b823581526103466040840160208501610eb0565b6001600160a01b031660208201526103646060840160408501610eb0565b6001600160a01b03166040808301919091526080808501356060840152439083015260c08085013560a0840152835190830152516103a9906005908390602001611284565b60408051601f198184030181529190528051602090910120600055505050565b6040516103dd906001908390602001611155565b6040516020818303038152906040528051906020012060001c6000541461040357600080fd5b61040f600a82356112f6565b431061041a57600080fd5b3460208201351461042a57600080fd5b7f5074eecd0995b3a3f596cc2835f720a588e59c1848a49558a0743c6f9bb251ba81604051610459919061108e565b60405180910390a1610469610d34565b8051602083013590526104826080830160608401610eb0565b81516001600160a01b039091166020909101528051336040918201526104ac9034908401356112f6565b6020808301805192909252815184359101528051436040909101525160016060909101526104d981610bd3565b5050565b6040516104f1906000908390602001611146565b6040516020818303038152906040528051906020012060001c6000541461051757600080fd5b61051f610cd2565b3460208301351461052f57600080fd5b61053a3460006112f6565b81526040517ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc1669061056c9084906110a2565b60405180910390a161057c610d59565b438152602080840135818301528251604080840191909152336060840152516103a991600191849101611169565b6040516105be906001908390602001611155565b6040516020818303038152906040528051906020012060001c600054146105e457600080fd5b6105f0600a82356112f6565b43101580156105fd575060015b61060657600080fd5b341561061157600080fd5b336106226080830160608401610eb0565b6001600160a01b03161461063557600080fd5b6106456080820160608301610eb0565b6001600160a01b03166108fc61065f3460408501356112f6565b6040518115909202916000818181858888f19350505050158015610687573d6000803e3d6000fd5b507f5891766ff36501b641f062681fd9d274fb08f40cbfbf3fba5e3c907f9b6f320a81604051610248919061108e565b6040516106cb9060039083906020016111a5565b6040516020818303038152906040528051906020012060001c600054146106f157600080fd5b6106f9610cd2565b610708600a60808401356112f6565b431061071357600080fd5b341561071e57600080fd5b3361072f6040840160208501610eb0565b6001600160a01b03161461074257600080fd5b6107503460608401356112f6565b81526040517ff948ef1ad0a1a495fc3532d2393b1845f35c1073faeaa7a785c3a443ee951747906107829084906110b9565b60405180910390a1610792610d8a565b823581526107a66040840160208501610eb0565b6001600160a01b031660208201526107c46060840160408501610eb0565b6001600160a01b031660408083019190915243606083015260a0808501356080840152835190830152516103a9906004908390602001611218565b60405161081390600590839060200161126f565b6040516020818303038152906040528051906020012060001c6000541461083957600080fd5b610848600a60808301356112f6565b4310158015610855575060015b61085e57600080fd5b341561086957600080fd5b3361087a6060830160408401610eb0565b6001600160a01b03161461088d57600080fd5b61089d6060820160408301610eb0565b6001600160a01b03166108fc6108b73460c08501356112f6565b6040518115909202916000818181858888f193505050501580156108df573d6000803e3d6000fd5b507feb62f87838dee3686c1f4cf7779f83baab34ca940fc98a1a989530712bea078881604051610248919061111c565b604051610923906004908390602001611204565b6040516020818303038152906040528051906020012060001c6000541461094957600080fd5b610958600a60608301356112f6565b4310158015610965575060015b61096e57600080fd5b341561097957600080fd5b3361098a6040830160208401610eb0565b6001600160a01b03161461099d57600080fd5b6109ad6040820160208301610eb0565b6001600160a01b03166108fc6109c73460a08501356112f6565b6040518115909202916000818181858888f193505050501580156109ef573d6000803e3d6000fd5b507f22dfeb9e788c86fb591f8ae2f63009c6f04bff2687696501b1448b747ec9c0af81604051610248919061112a565b604051610a3390600590839060200161126f565b6040516020818303038152906040528051906020012060001c60005414610a5957600080fd5b610a68600a60808301356112f6565b4310610a7357600080fd5b3415610a7e57600080fd5b33610a8f6040830160208401610eb0565b6001600160a01b031614610aa257600080fd5b604051610abe9060e083013590610100840135906020016112e8565b60408051601f198184030181529190528051602090910120606082013514610ae557600080fd5b7f4f661d1e3c80826d8bb1ea490d5b6a2290c8b62fdbe859223656d2a583857a0081604051610b1491906110f1565b60405180910390a1610b24610d34565b805182359052610b3a6040830160208401610eb0565b81516001600160a01b03909116602090910152610b5d6060830160408401610eb0565b81516001600160a01b03909116604090910152610b7e3460c08401356112f6565b60208083018051929092528151608085013591015251436040909101526003610bac60a0840135600461132d565b610bbb906101008501356112f6565b610bc59190611344565b6020820151606001526104d9815b60018160200151606001511415610c5c57610bec610dd2565b815151815281516020908101516001600160a01b0390811682840152835160409081015190911681840152818401805151606085015251810151608084015251610c3b916003918491016111b9565b60408051601f19818403018152919052805160209091012060005550610ccf565b600281602001516060015114610c7757805160400151610c7e565b8051602001515b6001600160a01b03166108fc8260000151600001516002610c9f919061130e565b6040518115909202916000818181858888f19350505050158015610cc7573d6000803e3d6000fd5b506000805533ff5b50565b6040518060200160405280600081525090565b6040518060e001604052806000815260200160006001600160a01b0316815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b6040518060400160405280610d47610e13565b8152602001610d54610e33565b905290565b604051806080016040528060008152602001600081526020016000815260200160006001600160a01b031681525090565b6040518060c001604052806000815260200160006001600160a01b0316815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b6040518060a001604052806000815260200160006001600160a01b0316815260200160006001600160a01b0316815260200160008152602001600081525090565b604080516060810182526000808252602082018190529181019190915290565b6040518060800160405280600081526020016000815260200160008152602001600081525090565b80356001600160a01b0381168114610e7257600080fd5b919050565b600060808284031215610e88578081fd5b50919050565b600060c08284031215610e88578081fd5b600060e08284031215610e88578081fd5b600060208284031215610ec1578081fd5b610eca82610e5b565b9392505050565b600060808284031215610ee2578081fd5b610eca8383610e77565b600060408284031215610e88578081fd5b600060c08284031215610f0e578081fd5b610eca8383610e8e565b600060e08284031215610f29578081fd5b610eca8383610e9f565b60006101208284031215610e88578081fd5b600060a08284031215610e88578081fd5b8035825260208082013590830152604080820135908301526001600160a01b03610f8260608301610e5b565b1660608301525050565b80358252610f9c60208201610e5b565b6001600160a01b03818116602085015280610fb960408501610e5b565b166040850152505060608181013590830152608090810135910152565b80358252610fe660208201610e5b565b6001600160a01b0381811660208501528061100360408501610e5b565b1660408501525050606081013560608301526080810135608083015260a081013560a08301525050565b8035825261103d60208201610e5b565b6001600160a01b0381811660208501528061105a60408501610e5b565b1660408501525050606081013560608301526080810135608083015260a081013560a083015260c081013560c08301525050565b6080810161109c8284610f56565b92915050565b813581526020918201359181019190915260400190565b60c081016110c78284610f8c565b60a092830135919092015290565b60e081016110e38284610fd6565b60c092830135919092015290565b6101208101611100828461102d565b60e083013560e083015261010080840135818401525092915050565b60e0810161109c828461102d565b60c0810161109c8284610fd6565b60a0810161109c8284610f8c565b91825235602082015260400190565b82815260a08101610eca6020830184610f56565b918252805160208084019190915281015160408084019190915281015160608084019190915201516001600160a01b0316608082015260a00190565b82815260c08101610eca6020830184610f8c565b91825280516020808401919091528101516001600160a01b0390811660408085019190915282015116606080840191909152810151608080840191909152015160a082015260c00190565b82815260e08101610eca6020830184610fd6565b91825280516020808401919091528101516001600160a01b039081166040808501919091528201511660608084019190915281015160808084019190915281015160a080840191909152015160c082015260e00190565b8281526101008101610eca602083018461102d565b60006101008201905083825282516020830152602083015160018060a01b038082166040850152806040860151166060850152505060608301516080830152608083015160a083015260a083015160c083015260c083015160e08301529392505050565b918252602082015260400190565b6000821982111561130957611309611364565b500190565b600081600019048311821515161561132857611328611364565b500290565b60008282101561133f5761133f611364565b500390565b60008261135f57634e487b7160e01b81526012600452602481fd5b500690565b634e487b7160e01b600052601160045260246000fdfea26469706673582212205e2960f6ee7b80c68edcd8c51511102722066fb54ec581949a2f87161dbd79d264736f6c63430008000033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

