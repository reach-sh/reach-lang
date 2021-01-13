// Automatically generated with Reach 0.1.2
/* eslint-disable no-unused-vars, no-empty-pattern, no-useless-escape, no-loop-func */
export const _version = '0.1.2';


export async function Alice(ctc, interact) {
  const stdlib = ctc.stdlib;
  const v37 = await ctc.creationTime();
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  const txn1 = await (ctc.sendrecv('Alice', 1, 1, stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], [v37, stdlib.protect(stdlib.T_UInt, interact.wager, null)], stdlib.protect(stdlib.T_UInt, interact.wager, null), [stdlib.T_UInt], true, true, false, ((txn1) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0), v37]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:44:9:dot', stdlib.UInt_max, 0)]);
    const [v41] = txn1.data;
    const v42 = txn1.value;
    const v48 = txn1.time;
    const v40 = txn1.from;
    
    stdlib.assert(true, {
      at: './index.rsh:44:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v43 = stdlib.eq(v42, v41);
    stdlib.assert(v43, {
      at: './index.rsh:44:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    const v44 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
    const v47 = stdlib.add(v44, v42);
    
    
    
    sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v48, v41, v47, v40]);
    sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:46:15:after expr stmt semicolon', stdlib.UInt_max, 1), v41, v47, v40]);
    sim_r.isHalt = false;
    
    return sim_r;
     })));
  const [v41] = txn1.data;
  const v42 = txn1.value;
  const v48 = txn1.time;
  const v40 = txn1.from;
  stdlib.assert(true, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Alice'
     });
  const v43 = stdlib.eq(v42, v41);
  stdlib.assert(v43, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Alice'
     });
  const v44 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
  const v47 = stdlib.add(v44, v42);
  
  
  
  const txn2 = await (ctc.recv('Alice', 2, 0, [], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
  if (txn2.didTimeout) {
    const txn3 = await (ctc.sendrecv('Alice', 10, 0, stdlib.checkedBigNumberify('reach standard library:59:7:dot', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address], [v48, v41, v47, v40], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn3) => {
      const sim_r = { txns: [] };
      sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('reach standard library:59:7:dot', stdlib.UInt_max, 1), v48, v41, v47, v40]);
      sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('reach standard library:59:7:dot', stdlib.UInt_max, 1), v41, v47, v40]);
      const [] = txn3.data;
      const v56 = txn3.value;
      const v63 = txn3.time;
      const v55 = txn3.from;
      
      const v58 = stdlib.addressEq(v40, v55);
      stdlib.assert(v58, {
        at: 'reach standard library:59:7:dot',
        fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
        msg: 'sender correct',
        who: 'Alice'
         });
      const v57 = stdlib.eq(v56, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
      stdlib.assert(v57, {
        at: 'reach standard library:59:7:dot',
        fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
        msg: 'pay amount correct',
        who: 'Alice'
         });
      
      const v62 = stdlib.add(v47, v56);
      
      
      
      
      sim_r.txns.push({
        amt: v62,
        to: v40
         });
      
      
      
      
      
      
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
      sim_r.isHalt = true;
      
      return sim_r;
       })));
    const [] = txn3.data;
    const v56 = txn3.value;
    const v63 = txn3.time;
    const v55 = txn3.from;
    const v58 = stdlib.addressEq(v40, v55);
    stdlib.assert(v58, {
      at: 'reach standard library:59:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v57 = stdlib.eq(v56, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v57, {
      at: 'reach standard library:59:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    
    const v62 = stdlib.add(v47, v56);
    
    
    
    
    ;
    
    
    
    
    
    
    
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:62:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
      msg: 'informTimeout',
      who: 'Alice'
       });
    return;
     }
  else {
    const [] = txn2.data;
    const v82 = txn2.value;
    const v88 = txn2.time;
    const v81 = txn2.from;
    stdlib.assert(true, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Alice'
       });
    const v83 = stdlib.eq(v82, v41);
    stdlib.assert(v83, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Alice'
       });
    
    const v87 = stdlib.add(v47, v82);
    let v265 = v87;
    let v267 = v48;
    let v266 = v88;
    let v90 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    while ((() => {
      const v101 = stdlib.eq(v90, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v101; })()) {
      
      
      
      const v105 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
        at: './index.rsh:59:42:application',
        fs: ['at ./index.rsh:61:51:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'getHand',
        who: 'Alice'
         });
      const v107 = stdlib.protect(stdlib.T_UInt, await interact.random(), {
        at: 'reach standard library:51:31:application',
        fs: ['at ./index.rsh:60:52:application call to [unknown function] (defined at: reach standard library:50:8:function exp)', 'at ./index.rsh:61:51:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:58:19:function exp)'],
        msg: 'random',
        who: 'Alice'
         });
      const v108 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v107, v105]);
      const txn3 = await (ctc.sendrecv('Alice', 4, 1, stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 4), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Digest], [v41, v40, v81, v265, v266, v108], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_Digest], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn3) => {
        const sim_r = { txns: [] };
        sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 3), v41, v40, v81, v265, v266]);
        sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:62:11:dot', stdlib.UInt_max, 3), v41, v40, v81, v265]);
        const [v138] = txn3.data;
        const v139 = txn3.value;
        const v146 = txn3.time;
        const v137 = txn3.from;
        
        const v141 = stdlib.addressEq(v40, v137);
        stdlib.assert(v141, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
           });
        const v140 = stdlib.eq(v139, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v140, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        
        const v145 = stdlib.add(v265, v139);
        
        
        
        
        sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 4), v41, v40, v81, v146, v138, v145]);
        sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:64:17:after expr stmt semicolon', stdlib.UInt_max, 4), v41, v40, v81, v138, v145]);
        sim_r.isHalt = false;
        
        return sim_r;
         })));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.recv('Alice', 9, 0, [], false, false));
        const [] = txn4.data;
        const v112 = txn4.value;
        const v119 = txn4.time;
        const v111 = txn4.from;
        const v114 = stdlib.addressEq(v81, v111);
        stdlib.assert(v114, {
          at: 'reach standard library:59:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
          msg: 'sender correct',
          who: 'Alice'
           });
        const v113 = stdlib.eq(v112, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v113, {
          at: 'reach standard library:59:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        
        const v118 = stdlib.add(v265, v112);
        
        
        
        
        ;
        
        
        
        
        
        
        
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:62:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
          msg: 'informTimeout',
          who: 'Alice'
           });
        return;
         }
      else {
        const [v138] = txn3.data;
        const v139 = txn3.value;
        const v146 = txn3.time;
        const v137 = txn3.from;
        const v141 = stdlib.addressEq(v40, v137);
        stdlib.assert(v141, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Alice'
           });
        const v140 = stdlib.eq(v139, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v140, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Alice'
           });
        
        const v145 = stdlib.add(v265, v139);
        
        
        
        
        const txn4 = await (ctc.recv('Alice', 5, 1, [stdlib.T_UInt], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.sendrecv('Alice', 8, 0, stdlib.checkedBigNumberify('reach standard library:59:7:dot', stdlib.UInt_max, 3), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt], [v41, v40, v81, v146, v138, v145], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:59:7:dot', stdlib.UInt_max, 4), v41, v40, v81, v146, v138, v145]);
            sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:59:7:dot', stdlib.UInt_max, 4), v41, v40, v81, v138, v145]);
            const [] = txn5.data;
            const v154 = txn5.value;
            const v161 = txn5.time;
            const v153 = txn5.from;
            
            const v156 = stdlib.addressEq(v40, v153);
            stdlib.assert(v156, {
              at: 'reach standard library:59:7:dot',
              fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v155 = stdlib.eq(v154, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v155, {
              at: 'reach standard library:59:7:dot',
              fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            
            const v160 = stdlib.add(v145, v154);
            
            
            
            
            sim_r.txns.push({
              amt: v160,
              to: v40
               });
            
            
            
            
            
            
            sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
            sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
            sim_r.isHalt = true;
            
            return sim_r;
             })));
          const [] = txn5.data;
          const v154 = txn5.value;
          const v161 = txn5.time;
          const v153 = txn5.from;
          const v156 = stdlib.addressEq(v40, v153);
          stdlib.assert(v156, {
            at: 'reach standard library:59:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v155 = stdlib.eq(v154, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v155, {
            at: 'reach standard library:59:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          
          const v160 = stdlib.add(v145, v154);
          
          
          
          
          ;
          
          
          
          
          
          
          
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:62:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
            msg: 'informTimeout',
            who: 'Alice'
             });
          return;
           }
        else {
          const [v180] = txn4.data;
          const v181 = txn4.value;
          const v188 = txn4.time;
          const v179 = txn4.from;
          const v183 = stdlib.addressEq(v81, v179);
          stdlib.assert(v183, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Alice'
             });
          const v182 = stdlib.eq(v181, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v182, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Alice'
             });
          
          const v187 = stdlib.add(v145, v181);
          
          
          
          const txn5 = await (ctc.sendrecv('Alice', 6, 2, stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 4), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v41, v40, v81, v138, v188, v180, v187, v107, v105], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn5) => {
            const sim_r = { txns: [] };
            sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 5), v41, v40, v81, v138, v188, v180, v187]);
            sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:75:11:dot', stdlib.UInt_max, 5), v41, v40, v81, v138, v180, v187]);
            const [v221, v222] = txn5.data;
            const v223 = txn5.value;
            const v230 = txn5.time;
            const v220 = txn5.from;
            
            const v225 = stdlib.addressEq(v40, v220);
            stdlib.assert(v225, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v224 = stdlib.eq(v223, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v224, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            
            const v229 = stdlib.add(v187, v223);
            const v233 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v221, v222]);
            const v234 = stdlib.eq(v138, v233);
            stdlib.assert(v234, {
              at: 'reach standard library:56:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:55:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v238 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v180);
            const v241 = stdlib.add(v222, v238);
            const v242 = stdlib.mod(v241, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv265 = v229;
            const cv267 = v188;
            const cv266 = v230;
            const cv90 = v242;
            
            (() => {
              const v265 = cv265;
              const v267 = cv267;
              const v266 = cv266;
              const v90 = cv90;
              
              if ((() => {
                const v101 = stdlib.eq(v90, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
                
                return v101; })()) {
                
                
                
                sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v41, v40, v81, v265, v266]);
                sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v41, v40, v81, v265]);
                sim_r.isHalt = false;
                 }
              else {
                const v244 = stdlib.eq(v90, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
                
                
                
                const v247 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v41);
                
                const v250 = v244 ? v40 : v81;
                
                
                
                sim_r.txns.push({
                  amt: v247,
                  to: v250
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
            const v195 = txn6.value;
            const v202 = txn6.time;
            const v194 = txn6.from;
            const v197 = stdlib.addressEq(v81, v194);
            stdlib.assert(v197, {
              at: 'reach standard library:59:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v196 = stdlib.eq(v195, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v196, {
              at: 'reach standard library:59:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            
            const v201 = stdlib.add(v187, v195);
            
            
            
            
            ;
            
            
            
            
            
            
            
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:62:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
              msg: 'informTimeout',
              who: 'Alice'
               });
            return;
             }
          else {
            const [v221, v222] = txn5.data;
            const v223 = txn5.value;
            const v230 = txn5.time;
            const v220 = txn5.from;
            const v225 = stdlib.addressEq(v40, v220);
            stdlib.assert(v225, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Alice'
               });
            const v224 = stdlib.eq(v223, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v224, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Alice'
               });
            
            const v229 = stdlib.add(v187, v223);
            const v233 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v221, v222]);
            const v234 = stdlib.eq(v138, v233);
            stdlib.assert(v234, {
              at: 'reach standard library:56:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:55:8:function exp)'],
              msg: null,
              who: 'Alice'
               });
            const v238 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v180);
            const v241 = stdlib.add(v222, v238);
            const v242 = stdlib.mod(v241, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv265 = v229;
            const cv267 = v188;
            const cv266 = v230;
            const cv90 = v242;
            
            v265 = cv265;
            v267 = cv267;
            v266 = cv266;
            v90 = cv90;
            
            continue; }
           }
         }
       }
    const v244 = stdlib.eq(v90, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    
    
    
    const v247 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v41);
    
    const v250 = v244 ? v40 : v81;
    
    
    
    ;
    
    
    
    
    
    
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v90), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:87:41:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:86:23:function exp)'],
      msg: 'seeOutcome',
      who: 'Alice'
       });
    return; }
  
  
   }
export async function Bob(ctc, interact) {
  const stdlib = ctc.stdlib;
  const v37 = await ctc.creationTime();
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  const txn1 = await (ctc.recv('Bob', 1, 1, [stdlib.T_UInt], false, false));
  const [v41] = txn1.data;
  const v42 = txn1.value;
  const v48 = txn1.time;
  const v40 = txn1.from;
  stdlib.assert(true, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'sender correct',
    who: 'Bob'
     });
  const v43 = stdlib.eq(v42, v41);
  stdlib.assert(v43, {
    at: './index.rsh:44:9:dot',
    fs: [],
    msg: 'pay amount correct',
    who: 'Bob'
     });
  const v44 = stdlib.checkedBigNumberify('./index.rsh:compileDApp', stdlib.UInt_max, 0);
  const v47 = stdlib.add(v44, v42);
  
  
  
  
  stdlib.protect(stdlib.T_Null, await interact.acceptWager(v41), {
    at: './index.rsh:49:29:application',
    fs: ['at ./index.rsh:49:40:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:48:17:function exp)'],
    msg: 'acceptWager',
    who: 'Bob'
     });
  const txn2 = await (ctc.sendrecv('Bob', 2, 0, stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 0), [stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address], [v48, v41, v47, v40], v41, [], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn2) => {
    const sim_r = { txns: [] };
    sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v48, v41, v47, v40]);
    sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address]), [stdlib.checkedBigNumberify('./index.rsh:50:9:dot', stdlib.UInt_max, 1), v41, v47, v40]);
    const [] = txn2.data;
    const v82 = txn2.value;
    const v88 = txn2.time;
    const v81 = txn2.from;
    
    stdlib.assert(true, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v83 = stdlib.eq(v82, v41);
    stdlib.assert(v83, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    
    const v87 = stdlib.add(v47, v82);
    const v265 = v87;
    const v267 = v48;
    const v266 = v88;
    const v90 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    if ((() => {
      const v101 = stdlib.eq(v90, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v101; })()) {
      
      
      
      sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v41, v40, v81, v265, v266]);
      sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:56:17:after expr stmt semicolon', stdlib.UInt_max, 3), v41, v40, v81, v265]);
      sim_r.isHalt = false;
       }
    else {
      const v244 = stdlib.eq(v90, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
      
      
      
      const v247 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v41);
      
      const v250 = v244 ? v40 : v81;
      
      
      
      sim_r.txns.push({
        amt: v247,
        to: v250
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
    const v56 = txn3.value;
    const v63 = txn3.time;
    const v55 = txn3.from;
    const v58 = stdlib.addressEq(v40, v55);
    stdlib.assert(v58, {
      at: 'reach standard library:59:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v57 = stdlib.eq(v56, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
    stdlib.assert(v57, {
      at: 'reach standard library:59:7:dot',
      fs: ['at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    
    const v62 = stdlib.add(v47, v56);
    
    
    
    
    ;
    
    
    
    
    
    
    
    stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
      at: './index.rsh:40:33:application',
      fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:62:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
      msg: 'informTimeout',
      who: 'Bob'
       });
    return;
     }
  else {
    const [] = txn2.data;
    const v82 = txn2.value;
    const v88 = txn2.time;
    const v81 = txn2.from;
    stdlib.assert(true, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'sender correct',
      who: 'Bob'
       });
    const v83 = stdlib.eq(v82, v41);
    stdlib.assert(v83, {
      at: './index.rsh:50:9:dot',
      fs: [],
      msg: 'pay amount correct',
      who: 'Bob'
       });
    
    const v87 = stdlib.add(v47, v82);
    let v265 = v87;
    let v267 = v48;
    let v266 = v88;
    let v90 = stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1);
    
    while ((() => {
      const v101 = stdlib.eq(v90, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 1));
      
      return v101; })()) {
      
      
      
      const txn3 = await (ctc.recv('Bob', 4, 1, [stdlib.T_Digest], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
      if (txn3.didTimeout) {
        const txn4 = await (ctc.sendrecv('Bob', 9, 0, stdlib.checkedBigNumberify('reach standard library:59:7:dot', stdlib.UInt_max, 4), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt], [v41, v40, v81, v265, v266], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:59:7:dot', stdlib.UInt_max, 3), v41, v40, v81, v265, v266]);
          sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:59:7:dot', stdlib.UInt_max, 3), v41, v40, v81, v265]);
          const [] = txn4.data;
          const v112 = txn4.value;
          const v119 = txn4.time;
          const v111 = txn4.from;
          
          const v114 = stdlib.addressEq(v81, v111);
          stdlib.assert(v114, {
            at: 'reach standard library:59:7:dot',
            fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v113 = stdlib.eq(v112, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v113, {
            at: 'reach standard library:59:7:dot',
            fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          
          const v118 = stdlib.add(v265, v112);
          
          
          
          
          sim_r.txns.push({
            amt: v118,
            to: v81
             });
          
          
          
          
          
          
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
          sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
          sim_r.isHalt = true;
          
          return sim_r;
           })));
        const [] = txn4.data;
        const v112 = txn4.value;
        const v119 = txn4.time;
        const v111 = txn4.from;
        const v114 = stdlib.addressEq(v81, v111);
        stdlib.assert(v114, {
          at: 'reach standard library:59:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
          msg: 'sender correct',
          who: 'Bob'
           });
        const v113 = stdlib.eq(v112, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v113, {
          at: 'reach standard library:59:7:dot',
          fs: ['at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        
        const v118 = stdlib.add(v265, v112);
        
        
        
        
        ;
        
        
        
        
        
        
        
        stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
          at: './index.rsh:40:33:application',
          fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:62:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
          msg: 'informTimeout',
          who: 'Bob'
           });
        return;
         }
      else {
        const [v138] = txn3.data;
        const v139 = txn3.value;
        const v146 = txn3.time;
        const v137 = txn3.from;
        const v141 = stdlib.addressEq(v40, v137);
        stdlib.assert(v141, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'sender correct',
          who: 'Bob'
           });
        const v140 = stdlib.eq(v139, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
        stdlib.assert(v140, {
          at: './index.rsh:62:11:dot',
          fs: [],
          msg: 'pay amount correct',
          who: 'Bob'
           });
        
        const v145 = stdlib.add(v265, v139);
        
        
        
        
        const v151 = stdlib.protect(stdlib.T_UInt, await interact.getHand(), {
          at: './index.rsh:68:52:application',
          fs: ['at ./index.rsh:68:59:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:67:19:function exp)'],
          msg: 'getHand',
          who: 'Bob'
           });
        const txn4 = await (ctc.sendrecv('Bob', 5, 1, stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 3), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt], [v41, v40, v81, v146, v138, v145, v151], stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0), [stdlib.T_UInt], true, true, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10), ((txn4) => {
          const sim_r = { txns: [] };
          sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_UInt, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 4), v41, v40, v81, v146, v138, v145]);
          sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:69:11:dot', stdlib.UInt_max, 4), v41, v40, v81, v138, v145]);
          const [v180] = txn4.data;
          const v181 = txn4.value;
          const v188 = txn4.time;
          const v179 = txn4.from;
          
          const v183 = stdlib.addressEq(v81, v179);
          stdlib.assert(v183, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v182 = stdlib.eq(v181, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v182, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          
          const v187 = stdlib.add(v145, v181);
          
          
          
          sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 5), v41, v40, v81, v138, v188, v180, v187]);
          sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('./index.rsh:71:17:after expr stmt semicolon', stdlib.UInt_max, 5), v41, v40, v81, v138, v180, v187]);
          sim_r.isHalt = false;
          
          return sim_r;
           })));
        if (txn4.didTimeout) {
          const txn5 = await (ctc.recv('Bob', 8, 0, [], false, false));
          const [] = txn5.data;
          const v154 = txn5.value;
          const v161 = txn5.time;
          const v153 = txn5.from;
          const v156 = stdlib.addressEq(v40, v153);
          stdlib.assert(v156, {
            at: 'reach standard library:59:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v155 = stdlib.eq(v154, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v155, {
            at: 'reach standard library:59:7:dot',
            fs: ['at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          
          const v160 = stdlib.add(v145, v154);
          
          
          
          
          ;
          
          
          
          
          
          
          
          stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
            at: './index.rsh:40:33:application',
            fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:62:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
            msg: 'informTimeout',
            who: 'Bob'
             });
          return;
           }
        else {
          const [v180] = txn4.data;
          const v181 = txn4.value;
          const v188 = txn4.time;
          const v179 = txn4.from;
          const v183 = stdlib.addressEq(v81, v179);
          stdlib.assert(v183, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'sender correct',
            who: 'Bob'
             });
          const v182 = stdlib.eq(v181, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
          stdlib.assert(v182, {
            at: './index.rsh:69:11:dot',
            fs: [],
            msg: 'pay amount correct',
            who: 'Bob'
             });
          
          const v187 = stdlib.add(v145, v181);
          
          
          
          const txn5 = await (ctc.recv('Bob', 6, 2, [stdlib.T_UInt, stdlib.T_UInt], false, stdlib.checkedBigNumberify('./index.rsh:32:18:decimal', stdlib.UInt_max, 10)));
          if (txn5.didTimeout) {
            const txn6 = await (ctc.sendrecv('Bob', 7, 0, stdlib.checkedBigNumberify('reach standard library:59:7:dot', stdlib.UInt_max, 4), [stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt], [v41, v40, v81, v138, v188, v180, v187], stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0), [], true, true, false, ((txn6) => {
              const sim_r = { txns: [] };
              sim_r.prevSt = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:59:7:dot', stdlib.UInt_max, 5), v41, v40, v81, v138, v188, v180, v187]);
              sim_r.prevSt_noPrevTime = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt, stdlib.T_Address, stdlib.T_Address, stdlib.T_Digest, stdlib.T_UInt, stdlib.T_UInt]), [stdlib.checkedBigNumberify('reach standard library:59:7:dot', stdlib.UInt_max, 5), v41, v40, v81, v138, v180, v187]);
              const [] = txn6.data;
              const v195 = txn6.value;
              const v202 = txn6.time;
              const v194 = txn6.from;
              
              const v197 = stdlib.addressEq(v81, v194);
              stdlib.assert(v197, {
                at: 'reach standard library:59:7:dot',
                fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
                msg: 'sender correct',
                who: 'Bob'
                 });
              const v196 = stdlib.eq(v195, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
              stdlib.assert(v196, {
                at: 'reach standard library:59:7:dot',
                fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
                msg: 'pay amount correct',
                who: 'Bob'
                 });
              
              const v201 = stdlib.add(v187, v195);
              
              
              
              
              sim_r.txns.push({
                amt: v201,
                to: v81
                 });
              
              
              
              
              
              
              sim_r.nextSt = stdlib.digest(stdlib.T_Tuple([]), []);
              sim_r.nextSt_noTime = stdlib.digest(stdlib.T_Tuple([]), []);
              sim_r.isHalt = true;
              
              return sim_r;
               })));
            const [] = txn6.data;
            const v195 = txn6.value;
            const v202 = txn6.time;
            const v194 = txn6.from;
            const v197 = stdlib.addressEq(v81, v194);
            stdlib.assert(v197, {
              at: 'reach standard library:59:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v196 = stdlib.eq(v195, stdlib.checkedBigNumberify('reach standard library:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v196, {
              at: 'reach standard library:59:7:dot',
              fs: ['at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            
            const v201 = stdlib.add(v187, v195);
            
            
            
            
            ;
            
            
            
            
            
            
            
            stdlib.protect(stdlib.T_Null, await interact.informTimeout(), {
              at: './index.rsh:40:33:application',
              fs: ['at ./index.rsh:40:39:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:39:25:function exp)', 'at reach standard library:62:8:application call to [unknown function] (defined at: ./index.rsh:38:32:function exp)', 'at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)'],
              msg: 'informTimeout',
              who: 'Bob'
               });
            return;
             }
          else {
            const [v221, v222] = txn5.data;
            const v223 = txn5.value;
            const v230 = txn5.time;
            const v220 = txn5.from;
            const v225 = stdlib.addressEq(v40, v220);
            stdlib.assert(v225, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'sender correct',
              who: 'Bob'
               });
            const v224 = stdlib.eq(v223, stdlib.checkedBigNumberify('./index.rsh:decimal', stdlib.UInt_max, 0));
            stdlib.assert(v224, {
              at: './index.rsh:75:11:dot',
              fs: [],
              msg: 'pay amount correct',
              who: 'Bob'
               });
            
            const v229 = stdlib.add(v187, v223);
            const v233 = stdlib.digest(stdlib.T_Tuple([stdlib.T_UInt, stdlib.T_UInt]), [v221, v222]);
            const v234 = stdlib.eq(v138, v233);
            stdlib.assert(v234, {
              at: 'reach standard library:56:17:application',
              fs: ['at ./index.rsh:77:24:application call to [unknown function] (defined at: reach standard library:55:8:function exp)'],
              msg: null,
              who: 'Bob'
               });
            const v238 = stdlib.sub(stdlib.checkedBigNumberify('./index.rsh:7:18:decimal', stdlib.UInt_max, 4), v180);
            const v241 = stdlib.add(v222, v238);
            const v242 = stdlib.mod(v241, stdlib.checkedBigNumberify('./index.rsh:7:32:decimal', stdlib.UInt_max, 3));
            const cv265 = v229;
            const cv267 = v188;
            const cv266 = v230;
            const cv90 = v242;
            
            v265 = cv265;
            v267 = cv267;
            v266 = cv266;
            v90 = cv90;
            
            continue; }
           }
         }
       }
    const v244 = stdlib.eq(v90, stdlib.checkedBigNumberify('./index.rsh:makeEnum', stdlib.UInt_max, 2));
    
    
    
    const v247 = stdlib.mul(stdlib.checkedBigNumberify('./index.rsh:83:16:decimal', stdlib.UInt_max, 2), v41);
    
    const v250 = v244 ? v40 : v81;
    
    
    
    ;
    
    
    
    
    
    
    stdlib.protect(stdlib.T_Null, await interact.seeOutcome(v90), {
      at: './index.rsh:87:28:application',
      fs: ['at ./index.rsh:87:41:after expr stmt semicolon call to [unknown function] (defined at: ./index.rsh:86:23:function exp)'],
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
// Just "sender correct"
// "./index.rsh:44:9:dot"
// "[]"
int 1
assert
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
int 0
gtxn 3 Amount
arg 3
btoi
-
+
store 255
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
// Just "sender correct"
// "./index.rsh:50:9:dot"
// "[]"
int 1
assert
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
// Just "sender correct"
// "./index.rsh:62:11:dot"
// "[]"
arg 6
gtxn 3 Sender
==
assert
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
// Just "sender correct"
// "./index.rsh:69:11:dot"
// "[]"
arg 7
gtxn 3 Sender
==
assert
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
// Just "sender correct"
// "./index.rsh:75:11:dot"
// "[]"
arg 6
gtxn 3 Sender
==
assert
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
// Just "sender correct"
// "reach standard library:59:7:dot"
// "[at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)]"
arg 7
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "reach standard library:59:7:dot"
// "[at ./index.rsh:76:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
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
// Just "sender correct"
// "reach standard library:59:7:dot"
// "[at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)]"
arg 6
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "reach standard library:59:7:dot"
// "[at ./index.rsh:70:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
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
// Just "sender correct"
// "reach standard library:59:7:dot"
// "[at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)]"
arg 7
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "reach standard library:59:7:dot"
// "[at ./index.rsh:63:43:application call to [unknown function] (defined at: reach standard library:58:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
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
// Just "sender correct"
// "reach standard library:59:7:dot"
// "[at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:58:8:function exp)]"
arg 7
gtxn 3 Sender
==
assert
// Just "pay amount correct"
// "reach standard library:59:7:dot"
// "[at ./index.rsh:51:41:application call to [unknown function] (defined at: reach standard library:58:8:function exp)]"
gtxn 3 Amount
arg 3
btoi
-
int 0
==
assert
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
                "name": "v37",
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
                "name": "v41",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
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
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v81",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v265",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v266",
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
                "name": "v138",
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
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v81",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v146",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v138",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v145",
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
                "name": "v180",
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
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v81",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v138",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v188",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v180",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v187",
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
                "name": "v221",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v222",
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
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v81",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v138",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v188",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v180",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v187",
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
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v81",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v146",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v138",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v145",
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
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v81",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v265",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v266",
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
                "name": "v37",
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
                "name": "v41",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
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
                "name": "v48",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v47",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
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
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v81",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v265",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v266",
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
                "name": "v138",
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
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v81",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v146",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v138",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v145",
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
                "name": "v180",
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
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v81",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v138",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v188",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v180",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v187",
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
                "name": "v221",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v222",
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
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v81",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v138",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v188",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v180",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v187",
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
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v81",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v146",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v138",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v145",
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
                "name": "v41",
                "type": "uint256"
              },
              {
                "internalType": "address payable",
                "name": "v40",
                "type": "address"
              },
              {
                "internalType": "address payable",
                "name": "v81",
                "type": "address"
              },
              {
                "internalType": "uint256",
                "name": "v265",
                "type": "uint256"
              },
              {
                "internalType": "uint256",
                "name": "v266",
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
  Bytecode: `0x608060408190527f49ff028a829527a47ec6839c7147b484eccf5a2a94853eddac09cef44d9d4e9e90600090a161003461007a565b43815261003f61007a565b8151815260405161005790600090839060200161008d565b60408051601f1981840301815291905280516020909101206000555061009c9050565b6040518060200160405280600081525090565b91825251602082015260400190565b6113b0806100ab6000396000f3fe6080604052600436106100865760003560e01c8063b43649a911610059578063b43649a9146100d9578063b8bc1e39146100ec578063d7f6b4cd146100ff578063e2fc8b4b14610112578063eeda42ac1461012557610086565b80633c1726a31461008b5780633e1037b1146100a0578063905436cf146100b35780639532ef01146100c6575b600080fd5b61009e610099366004610f45565b610138565b005b61009e6100ae366004610f18565b610257565b61009e6100c1366004610ed1565b6103c9565b61009e6100d4366004610eec565b6104dd565b61009e6100e7366004610ed1565b6105aa565b61009e6100fa366004610efd565b6106b7565b61009e61010d366004610f18565b6107ff565b61009e610120366004610efd565b61090f565b61009e610133366004610f33565b610a1f565b60405161014c9060039083906020016111a5565b6040516020818303038152906040528051906020012060001c6000541461017257600080fd5b610181600a60808301356112f6565b431015801561018e575060015b61019757600080fd5b336101a86060830160408401610eb0565b6001600160a01b0316146101bb57600080fd5b34156101c657600080fd5b6101d66060820160408301610eb0565b6001600160a01b03166108fc6101f03460608501356112f6565b6040518115909202916000818181858888f19350505050158015610218573d6000803e3d6000fd5b507f893e8b5e3088e38bfe39d4bd08bb72efde6587ca2c04cfca2f91b997377e7f38816040516102489190611138565b60405180910390a16000805533ff5b60405161026b906004908390602001611204565b6040516020818303038152906040528051906020012060001c6000541461029157600080fd5b610299610cd2565b6102a8600a60608401356112f6565b43106102b357600080fd5b336102c46060840160408501610eb0565b6001600160a01b0316146102d757600080fd5b34156102e257600080fd5b6102f03460a08401356112f6565b81526040517f8b364c1cec79e889c00d6309a0d6316af59862f7883ed242d587454e9f375b08906103229084906110d5565b60405180910390a1610332610ce5565b823581526103466040840160208501610eb0565b6001600160a01b031660208201526103646060840160408501610eb0565b6001600160a01b03166040808301919091526080808501356060840152439083015260c08085013560a0840152835190830152516103a9906005908390602001611284565b60408051601f198184030181529190528051602090910120600055505050565b6040516103dd906001908390602001611155565b6040516020818303038152906040528051906020012060001c6000541461040357600080fd5b61040f600a82356112f6565b431061041a57600080fd5b3460208201351461042a57600080fd5b7f5074eecd0995b3a3f596cc2835f720a588e59c1848a49558a0743c6f9bb251ba81604051610459919061108e565b60405180910390a1610469610d34565b8051602083013590526104826080830160608401610eb0565b81516001600160a01b039091166020909101528051336040918201526104ac9034908401356112f6565b6020808301805192909252815184359101528051436040909101525160016060909101526104d981610bd3565b5050565b6040516104f1906000908390602001611146565b6040516020818303038152906040528051906020012060001c6000541461051757600080fd5b61051f610cd2565b3460208301351461052f57600080fd5b61053a3460006112f6565b81526040517ff2c62eba998811305a23599b2e6d212befbd7ded3a73f4c08bfb9aefe08dc1669061056c9084906110a2565b60405180910390a161057c610d59565b438152602080840135818301528251604080840191909152336060840152516103a991600191849101611169565b6040516105be906001908390602001611155565b6040516020818303038152906040528051906020012060001c600054146105e457600080fd5b6105f0600a82356112f6565b43101580156105fd575060015b61060657600080fd5b336106176080830160608401610eb0565b6001600160a01b03161461062a57600080fd5b341561063557600080fd5b6106456080820160608301610eb0565b6001600160a01b03166108fc61065f3460408501356112f6565b6040518115909202916000818181858888f19350505050158015610687573d6000803e3d6000fd5b507f5891766ff36501b641f062681fd9d274fb08f40cbfbf3fba5e3c907f9b6f320a81604051610248919061108e565b6040516106cb9060039083906020016111a5565b6040516020818303038152906040528051906020012060001c600054146106f157600080fd5b6106f9610cd2565b610708600a60808401356112f6565b431061071357600080fd5b336107246040840160208501610eb0565b6001600160a01b03161461073757600080fd5b341561074257600080fd5b6107503460608401356112f6565b81526040517ff948ef1ad0a1a495fc3532d2393b1845f35c1073faeaa7a785c3a443ee951747906107829084906110b9565b60405180910390a1610792610d8a565b823581526107a66040840160208501610eb0565b6001600160a01b031660208201526107c46060840160408501610eb0565b6001600160a01b031660408083019190915243606083015260a0808501356080840152835190830152516103a9906004908390602001611218565b60405161081390600590839060200161126f565b6040516020818303038152906040528051906020012060001c6000541461083957600080fd5b610848600a60808301356112f6565b4310158015610855575060015b61085e57600080fd5b3361086f6060830160408401610eb0565b6001600160a01b03161461088257600080fd5b341561088d57600080fd5b61089d6060820160408301610eb0565b6001600160a01b03166108fc6108b73460c08501356112f6565b6040518115909202916000818181858888f193505050501580156108df573d6000803e3d6000fd5b507feb62f87838dee3686c1f4cf7779f83baab34ca940fc98a1a989530712bea078881604051610248919061111c565b604051610923906004908390602001611204565b6040516020818303038152906040528051906020012060001c6000541461094957600080fd5b610958600a60608301356112f6565b4310158015610965575060015b61096e57600080fd5b3361097f6040830160208401610eb0565b6001600160a01b03161461099257600080fd5b341561099d57600080fd5b6109ad6040820160208301610eb0565b6001600160a01b03166108fc6109c73460a08501356112f6565b6040518115909202916000818181858888f193505050501580156109ef573d6000803e3d6000fd5b507f22dfeb9e788c86fb591f8ae2f63009c6f04bff2687696501b1448b747ec9c0af81604051610248919061112a565b604051610a3390600590839060200161126f565b6040516020818303038152906040528051906020012060001c60005414610a5957600080fd5b610a68600a60808301356112f6565b4310610a7357600080fd5b33610a846040830160208401610eb0565b6001600160a01b031614610a9757600080fd5b3415610aa257600080fd5b604051610abe9060e083013590610100840135906020016112e8565b60408051601f198184030181529190528051602090910120606082013514610ae557600080fd5b7f4f661d1e3c80826d8bb1ea490d5b6a2290c8b62fdbe859223656d2a583857a0081604051610b1491906110f1565b60405180910390a1610b24610d34565b805182359052610b3a6040830160208401610eb0565b81516001600160a01b03909116602090910152610b5d6060830160408401610eb0565b81516001600160a01b03909116604090910152610b7e3460c08401356112f6565b60208083018051929092528151608085013591015251436040909101526003610bac60a0840135600461132d565b610bbb906101008501356112f6565b610bc59190611344565b6020820151606001526104d9815b60018160200151606001511415610c5c57610bec610dd2565b815151815281516020908101516001600160a01b0390811682840152835160409081015190911681840152818401805151606085015251810151608084015251610c3b916003918491016111b9565b60408051601f19818403018152919052805160209091012060005550610ccf565b600281602001516060015114610c7757805160400151610c7e565b8051602001515b6001600160a01b03166108fc8260000151600001516002610c9f919061130e565b6040518115909202916000818181858888f19350505050158015610cc7573d6000803e3d6000fd5b506000805533ff5b50565b6040518060200160405280600081525090565b6040518060e001604052806000815260200160006001600160a01b0316815260200160006001600160a01b03168152602001600081526020016000815260200160008152602001600081525090565b6040518060400160405280610d47610e13565b8152602001610d54610e33565b905290565b604051806080016040528060008152602001600081526020016000815260200160006001600160a01b031681525090565b6040518060c001604052806000815260200160006001600160a01b0316815260200160006001600160a01b031681526020016000815260200160008152602001600081525090565b6040518060a001604052806000815260200160006001600160a01b0316815260200160006001600160a01b0316815260200160008152602001600081525090565b604080516060810182526000808252602082018190529181019190915290565b6040518060800160405280600081526020016000815260200160008152602001600081525090565b80356001600160a01b0381168114610e7257600080fd5b919050565b600060808284031215610e88578081fd5b50919050565b600060c08284031215610e88578081fd5b600060e08284031215610e88578081fd5b600060208284031215610ec1578081fd5b610eca82610e5b565b9392505050565b600060808284031215610ee2578081fd5b610eca8383610e77565b600060408284031215610e88578081fd5b600060c08284031215610f0e578081fd5b610eca8383610e8e565b600060e08284031215610f29578081fd5b610eca8383610e9f565b60006101208284031215610e88578081fd5b600060a08284031215610e88578081fd5b8035825260208082013590830152604080820135908301526001600160a01b03610f8260608301610e5b565b1660608301525050565b80358252610f9c60208201610e5b565b6001600160a01b03818116602085015280610fb960408501610e5b565b166040850152505060608181013590830152608090810135910152565b80358252610fe660208201610e5b565b6001600160a01b0381811660208501528061100360408501610e5b565b1660408501525050606081013560608301526080810135608083015260a081013560a08301525050565b8035825261103d60208201610e5b565b6001600160a01b0381811660208501528061105a60408501610e5b565b1660408501525050606081013560608301526080810135608083015260a081013560a083015260c081013560c08301525050565b6080810161109c8284610f56565b92915050565b813581526020918201359181019190915260400190565b60c081016110c78284610f8c565b60a092830135919092015290565b60e081016110e38284610fd6565b60c092830135919092015290565b6101208101611100828461102d565b60e083013560e083015261010080840135818401525092915050565b60e0810161109c828461102d565b60c0810161109c8284610fd6565b60a0810161109c8284610f8c565b91825235602082015260400190565b82815260a08101610eca6020830184610f56565b918252805160208084019190915281015160408084019190915281015160608084019190915201516001600160a01b0316608082015260a00190565b82815260c08101610eca6020830184610f8c565b91825280516020808401919091528101516001600160a01b0390811660408085019190915282015116606080840191909152810151608080840191909152015160a082015260c00190565b82815260e08101610eca6020830184610fd6565b91825280516020808401919091528101516001600160a01b039081166040808501919091528201511660608084019190915281015160808084019190915281015160a080840191909152015160c082015260e00190565b8281526101008101610eca602083018461102d565b60006101008201905083825282516020830152602083015160018060a01b038082166040850152806040860151166060850152505060608301516080830152608083015160a083015260a083015160c083015260c083015160e08301529392505050565b918252602082015260400190565b6000821982111561130957611309611364565b500190565b600081600019048311821515161561132857611328611364565b500290565b60008282101561133f5761133f611364565b500390565b60008261135f57634e487b7160e01b81526012600452602481fd5b500690565b634e487b7160e01b600052601160045260246000fdfea2646970667358221220d7c0c59b7ff09796cbfa512e2567acd7488f9398fd1fb042a6c4d3757454c7d564736f6c63430008000033`,
  deployMode: `DM_constructor`
   };

export const _Connectors = {
  ALGO: _ALGO,
  ETH: _ETH
   };

