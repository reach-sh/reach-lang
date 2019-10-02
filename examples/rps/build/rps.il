#lang reach/il

#:participants
(define-participant (0, (A, address))
  [1/_wagerAmount:uint256]
  [2/_escrowAmount:uint256])
(define-participant (3, (B, address)))
(define-participant (4, (O, address)))

#:main
(@ ( 0
, ( A
, address ) ) (define 5/Declassify:uint256 (declassify 1/_wagerAmount:uint256)))
(@ ( 0
, ( A
, address ) ) (define 6/Declassify:uint256 (declassify 2/_escrowAmount:uint256)))
(@ (0, (A, address)) (define 7/Interact:bool (interact.params bool )))
(@ ( 0
, ( A
, address ) ) (define 8/PrimApp:uint256 (ADD 5/Declassify:uint256 6/Declassify:uint256)))
(@ True 0/A:address 
  (publish! (5/Declassify:uint256 6/Declassify:uint256))
  (pay! 8/PrimApp:uint256)
  (timeout  10 
    (commit!)
    (@ CTC (require! True))
    "Alice quits"))
(@ CTC (define 9/PrimApp:uint256 (TXN_VALUE)))
(@ CTC (define 10/PrimApp:uint256 (ADD 5/Declassify:uint256 6/Declassify:uint256)))
(@ CTC (define 11/PrimApp:bool (PEQ 9/PrimApp:uint256 10/PrimApp:uint256)))
(@ CTC (require! 11/PrimApp:bool))
(commit!)
(@ ( 3
, ( B
, address ) ) (define 12/Interact:bool (interact.accepts bool 5/Declassify:uint256 6/Declassify:uint256)))
(@ True 3/B:address 
  (publish! ())
  (pay! 5/Declassify:uint256)
  (timeout (0, (A, address)) 10 
    (@ CTC (define 139/PrimApp:uint256 (BALANCE)))
    (@ CTC (transfer! 0/A:address 139/PrimApp:uint256))
    (commit!)
    (@ CTC (require! True))
    "Bob quits"))
(@ CTC (define 13/PrimApp:uint256 (TXN_VALUE)))
(@ CTC (define 14/PrimApp:bool (PEQ 13/PrimApp:uint256 5/Declassify:uint256)))
(@ CTC (require! 14/PrimApp:bool))
(commit!)
(@ (0, (A, address)) (define 15/Interact:bytes (interact.getHand bytes )))
(@ ( 0
, (A, address) ) (define 16/PrimApp:bool (BYTES_EQ 15/Interact:bytes "ROCK")))
(@ ( 0
, (A, address) ) (define 17/PrimApp:bool (BYTES_EQ 15/Interact:bytes "PAPER")))
(@ ( 0
, ( A
, address ) ) (define 18/PrimApp:bool (BYTES_EQ 15/Interact:bytes "SCISSORS")))
(@ ( 0
, ( A
, address ) ) (define 19/PureIf:bool (IF_THEN_ELSE 16/PrimApp:bool True 17/PrimApp:bool)))
(@ ( 0
, ( A
, address ) ) (define 20/PureIf:bool (IF_THEN_ELSE 19/PureIf:bool True 18/PrimApp:bool)))
(@ (0, (A, address)) (assume! 20/PureIf:bool))
(@ ( 0
, (A, address) ) (define 21/PureIf:uint256 (IF_THEN_ELSE 17/PrimApp:bool 1 2)))
(@ ( 0
, ( A
, address ) ) (define 22/PureIf:uint256 (IF_THEN_ELSE 16/PrimApp:bool 0 21/PureIf:uint256)))
(@ (0, (A, address)) (define 23/PrimApp:bool (PLE 0 22/PureIf:uint256)))
(@ (0, (A, address)) (define 24/PrimApp:bool (PLT 22/PureIf:uint256 3)))
(@ ( 0
, ( A
, address ) ) (define 25/PureIf:bool (IF_THEN_ELSE 23/PrimApp:bool 24/PrimApp:bool False)))
(@ (0, (A, address)) (assert! 25/PureIf:bool))
(@ (0, (A, address)) (define 26/PrimApp:uint256 (RANDOM)))
(@ ( 0
, ( A
, address ) ) (define 27/PrimApp:bytes (UINT256_TO_BYTES 26/PrimApp:uint256)))
(@ ( 0
, (A, address) ) (define 28/PrimApp:bytes (UINT256_TO_BYTES 22/PureIf:uint256)))
(@ ( 0
, ( A
, address ) ) (define 29/PrimApp:bytes (BCAT 27/PrimApp:bytes 28/PrimApp:bytes)))
(@ (0, (A, address)) (define 30/PrimApp:uint256 (DIGEST 29/PrimApp:bytes)))
(@ ( 0
, (A, address) ) (define 31/Declassify:uint256 (declassify 30/PrimApp:uint256)))
(@ (0, (A, address)) (define 32/Interact:bool (interact.commits bool )))
(@ False 0/A:address 
  (publish! (31/Declassify:uint256))
  (pay! 0)
  (timeout (3, (B, address)) 10 
    (@ CTC (define 138/PrimApp:uint256 (BALANCE)))
    (@ CTC (transfer! 3/B:address 138/PrimApp:uint256))
    (commit!)
    (@ CTC (require! True))
    "Alice quits"))
(@ CTC (define 33/PrimApp:uint256 (TXN_VALUE)))
(@ CTC (define 34/PrimApp:bool (PEQ 33/PrimApp:uint256 0)))
(@ CTC (require! 34/PrimApp:bool))
(commit!)
(@ (3, (B, address)) (define 35/Interact:bytes (interact.getHand bytes )))
(@ ( 3
, (B, address) ) (define 36/PrimApp:bool (BYTES_EQ 35/Interact:bytes "ROCK")))
(@ ( 3
, (B, address) ) (define 37/PrimApp:bool (BYTES_EQ 35/Interact:bytes "PAPER")))
(@ ( 3
, ( B
, address ) ) (define 38/PrimApp:bool (BYTES_EQ 35/Interact:bytes "SCISSORS")))
(@ ( 3
, ( B
, address ) ) (define 39/PureIf:bool (IF_THEN_ELSE 36/PrimApp:bool True 37/PrimApp:bool)))
(@ ( 3
, ( B
, address ) ) (define 40/PureIf:bool (IF_THEN_ELSE 39/PureIf:bool True 38/PrimApp:bool)))
(@ (3, (B, address)) (assume! 40/PureIf:bool))
(@ ( 3
, (B, address) ) (define 41/PureIf:uint256 (IF_THEN_ELSE 37/PrimApp:bool 1 2)))
(@ ( 3
, ( B
, address ) ) (define 42/PureIf:uint256 (IF_THEN_ELSE 36/PrimApp:bool 0 41/PureIf:uint256)))
(@ (3, (B, address)) (define 43/PrimApp:bool (PLE 0 42/PureIf:uint256)))
(@ (3, (B, address)) (define 44/PrimApp:bool (PLT 42/PureIf:uint256 3)))
(@ ( 3
, ( B
, address ) ) (define 45/PureIf:bool (IF_THEN_ELSE 43/PrimApp:bool 44/PrimApp:bool False)))
(@ (3, (B, address)) (assert! 45/PureIf:bool))
(@ ( 3
, (B, address) ) (define 46/Declassify:uint256 (declassify 42/PureIf:uint256)))
(@ (3, (B, address)) (define 47/Interact:bool (interact.shows bool )))
(@ False 3/B:address 
  (publish! (46/Declassify:uint256))
  (pay! 0)
  (timeout (0, (A, address)) 10 
    (@ CTC (define 137/PrimApp:uint256 (BALANCE)))
    (@ CTC (transfer! 0/A:address 137/PrimApp:uint256))
    (commit!)
    (@ CTC (require! True))
    "Bob quits"))
(@ CTC (define 48/PrimApp:uint256 (TXN_VALUE)))
(@ CTC (define 49/PrimApp:bool (PEQ 48/PrimApp:uint256 0)))
(@ CTC (require! 49/PrimApp:bool))
(@ CTC (define 50/PrimApp:bool (PLE 0 46/Declassify:uint256)))
(@ CTC (define 51/PrimApp:bool (PLT 46/Declassify:uint256 3)))
(@ CTC (define 52/PureIf:bool (IF_THEN_ELSE 50/PrimApp:bool 51/PrimApp:bool False)))
(@ CTC (require! 52/PureIf:bool))
(commit!)
(@ ( 0
, (A, address) ) (define 53/Declassify:uint256 (declassify 26/PrimApp:uint256)))
(@ ( 0
, (A, address) ) (define 54/Declassify:uint256 (declassify 22/PureIf:uint256)))
(@ (0, (A, address)) (define 55/PrimApp:bool (PLE 0 46/Declassify:uint256)))
(@ (0, (A, address)) (define 56/PrimApp:bool (PLT 46/Declassify:uint256 3)))
(@ ( 0
, ( A
, address ) ) (define 57/PureIf:bool (IF_THEN_ELSE 55/PrimApp:bool 56/PrimApp:bool False)))
(@ (0, (A, address)) (require! 57/PureIf:bool))
(@ (0, (A, address)) (define 58/PrimApp:bool (PEQ 46/Declassify:uint256 0)))
(@ (0, (A, address)) (define 59/PrimApp:bool (PEQ 46/Declassify:uint256 1)))
(@ ( 0
, ( A
, address ) ) (define 60/PureIf:bytes (IF_THEN_ELSE 59/PrimApp:bool "PAPER" "SCISSORS")))
(@ ( 0
, ( A
, address ) ) (define 61/PureIf:bytes (IF_THEN_ELSE 58/PrimApp:bool "ROCK" 60/PureIf:bytes)))
(@ ( 0
, ( A
, address ) ) (define 62/Interact:bool (interact.reveals bool 61/PureIf:bytes)))
(@ False 0/A:address 
  (publish! (53/Declassify:uint256 54/Declassify:uint256))
  (pay! 0)
  (timeout (3, (B, address)) 10 
    (@ CTC (define 136/PrimApp:uint256 (BALANCE)))
    (@ CTC (transfer! 3/B:address 136/PrimApp:uint256))
    (commit!)
    (@ CTC (require! True))
    "Alice quits"))
(@ CTC (define 63/PrimApp:uint256 (TXN_VALUE)))
(@ CTC (define 64/PrimApp:bool (PEQ 63/PrimApp:uint256 0)))
(@ CTC (require! 64/PrimApp:bool))
(@ CTC (define 65/PrimApp:bytes (UINT256_TO_BYTES 53/Declassify:uint256)))
(@ CTC (define 66/PrimApp:bytes (UINT256_TO_BYTES 54/Declassify:uint256)))
(@ CTC (define 67/PrimApp:bytes (BCAT 65/PrimApp:bytes 66/PrimApp:bytes)))
(@ CTC (define 68/PrimApp:uint256 (DIGEST 67/PrimApp:bytes)))
(@ CTC (define 69/PrimApp:bool (PEQ 31/Declassify:uint256 68/PrimApp:uint256)))
(@ CTC (require! 69/PrimApp:bool))
(@ CTC (define 70/PrimApp:bool (PLE 0 54/Declassify:uint256)))
(@ CTC (define 71/PrimApp:bool (PLT 54/Declassify:uint256 3)))
(@ CTC (define 72/PureIf:bool (IF_THEN_ELSE 70/PrimApp:bool 71/PrimApp:bool False)))
(@ CTC (require! 72/PureIf:bool))
(@ CTC (define 73/PrimApp:bool (PLE 0 54/Declassify:uint256)))
(@ CTC (define 74/PrimApp:bool (PLT 54/Declassify:uint256 3)))
(@ CTC (define 75/PureIf:bool (IF_THEN_ELSE 73/PrimApp:bool 74/PrimApp:bool False)))
(@ CTC (define 76/PrimApp:bool (PLE 0 46/Declassify:uint256)))
(@ CTC (define 77/PrimApp:bool (PLT 46/Declassify:uint256 3)))
(@ CTC (define 78/PureIf:bool (IF_THEN_ELSE 76/PrimApp:bool 77/PrimApp:bool False)))
(@ CTC (define 79/PureIf:bool (IF_THEN_ELSE 75/PureIf:bool 78/PureIf:bool False)))
(@ CTC (define 80/PrimApp:uint256 (SUB 4 46/Declassify:uint256)))
(@ CTC (define 81/PrimApp:uint256 (ADD 54/Declassify:uint256 80/PrimApp:uint256)))
(@ CTC (define 82/PrimApp:uint256 (MOD 81/PrimApp:uint256 3)))
(@ CTC (define 83/PureIf:uint256 (IF_THEN_ELSE 78/PureIf:bool 0 1)))
(@ CTC (define 84/PureIf:uint256 (IF_THEN_ELSE 75/PureIf:bool 2 83/PureIf:uint256)))
(@ CTC (define 85/PureIf:uint256 (IF_THEN_ELSE 79/PureIf:bool 82/PrimApp:uint256 84/PureIf:uint256)))
(@ CTC (define 86/PrimApp:bool (PLE 0 85/PureIf:uint256)))
(@ CTC (define 87/PrimApp:bool (PLT 85/PureIf:uint256 5)))
(@ CTC (define 88/PureIf:bool (IF_THEN_ELSE 86/PrimApp:bool 87/PrimApp:bool False)))
(@ CTC (assert! 88/PureIf:bool))
(@ CTC (define 89/PrimApp:bool (PEQ 85/PureIf:uint256 2)))
(@ CTC (define 90/PrimApp:bool (PLE 0 54/Declassify:uint256)))
(@ CTC (define 91/PrimApp:bool (PLT 54/Declassify:uint256 3)))
(@ CTC (define 92/PureIf:bool (IF_THEN_ELSE 90/PrimApp:bool 91/PrimApp:bool False)))
(@ CTC (define 93/PureIf:bool (IF_THEN_ELSE 89/PrimApp:bool False True)))
(@ CTC (define 94/PureIf:bool (IF_THEN_ELSE 93/PureIf:bool True 92/PureIf:bool)))
(@ CTC (assert! 94/PureIf:bool))
(@ CTC (define 95/PrimApp:bool (PEQ 85/PureIf:uint256 0)))
(@ CTC (define 96/PrimApp:bool (PLE 0 46/Declassify:uint256)))
(@ CTC (define 97/PrimApp:bool (PLT 46/Declassify:uint256 3)))
(@ CTC (define 98/PureIf:bool (IF_THEN_ELSE 96/PrimApp:bool 97/PrimApp:bool False)))
(@ CTC (define 99/PureIf:bool (IF_THEN_ELSE 95/PrimApp:bool False True)))
(@ CTC (define 100/PureIf:bool (IF_THEN_ELSE 99/PureIf:bool True 98/PureIf:bool)))
(@ CTC (assert! 100/PureIf:bool))
(@ CTC (define 101/PrimApp:bool (PEQ 85/PureIf:uint256 2)))
(@ CTC (define 102/PrimApp:bool (PEQ 54/Declassify:uint256 0)))
(@ CTC (define 103/PureIf:bool (IF_THEN_ELSE 102/PrimApp:bool 101/PrimApp:bool False)))
(@ CTC (possible? 103/PureIf:bool))
(@ CTC (define 104/PrimApp:bool (PEQ 54/Declassify:uint256 1)))
(@ CTC (define 105/PureIf:bool (IF_THEN_ELSE 104/PrimApp:bool 101/PrimApp:bool False)))
(@ CTC (possible? 105/PureIf:bool))
(@ CTC (define 106/PrimApp:bool (PEQ 54/Declassify:uint256 2)))
(@ CTC (define 107/PureIf:bool (IF_THEN_ELSE 106/PrimApp:bool 101/PrimApp:bool False)))
(@ CTC (possible? 107/PureIf:bool))
(@ CTC (define 108/PrimApp:bool (PEQ 85/PureIf:uint256 0)))
(@ CTC (define 109/PrimApp:bool (PEQ 46/Declassify:uint256 0)))
(@ CTC (define 110/PureIf:bool (IF_THEN_ELSE 109/PrimApp:bool 108/PrimApp:bool False)))
(@ CTC (possible? 110/PureIf:bool))
(@ CTC (define 111/PrimApp:bool (PEQ 46/Declassify:uint256 1)))
(@ CTC (define 112/PureIf:bool (IF_THEN_ELSE 111/PrimApp:bool 108/PrimApp:bool False)))
(@ CTC (possible? 112/PureIf:bool))
(@ CTC (define 113/PrimApp:bool (PEQ 46/Declassify:uint256 2)))
(@ CTC (define 114/PureIf:bool (IF_THEN_ELSE 113/PrimApp:bool 108/PrimApp:bool False)))
(@ CTC (possible? 114/PureIf:bool))
(@ CTC (define 115/PrimApp:bool (PEQ 85/PureIf:uint256 2)))
(@ CTC (define 116/PrimApp:uint256 (MUL 2 5/Declassify:uint256)))
(@ CTC (define 117/PrimApp:bool (PEQ 85/PureIf:uint256 0)))
(@ CTC (define 118/PrimApp:uint256 (MUL 2 5/Declassify:uint256)))
(@ CTC (define 119/PureIf:uint256 (IF_THEN_ELSE 117/PrimApp:bool 0 5/Declassify:uint256)))
(@ CTC (define 120/PureIf:uint256 (IF_THEN_ELSE 117/PrimApp:bool 118/PrimApp:uint256 5/Declassify:uint256)))
(@ CTC (define 121/PureIf:uint256 (IF_THEN_ELSE 115/PrimApp:bool 116/PrimApp:uint256 119/PureIf:uint256)))
(@ CTC (define 122/PureIf:uint256 (IF_THEN_ELSE 115/PrimApp:bool 0 120/PureIf:uint256)))
(@ CTC (define 123/PrimApp:uint256 (ADD 6/Declassify:uint256 121/PureIf:uint256)))
(@ CTC (transfer! 0/A:address 123/PrimApp:uint256))
(@ CTC (transfer! 3/B:address 122/PureIf:uint256))
(commit!)
(@ CTC (define 124/Interact:bool (interact.outcome bool )))
(@ CTC (define 125/PrimApp:bool (PLE 0 85/PureIf:uint256)))
(@ CTC (define 126/PrimApp:bool (PLT 85/PureIf:uint256 5)))
(@ CTC (define 127/PureIf:bool (IF_THEN_ELSE 125/PrimApp:bool 126/PrimApp:bool False)))
(@ CTC (require! 127/PureIf:bool))
(@ CTC (define 128/PrimApp:bool (PEQ 85/PureIf:uint256 0)))
(@ CTC (define 129/PrimApp:bool (PEQ 85/PureIf:uint256 1)))
(@ CTC (define 130/PrimApp:bool (PEQ 85/PureIf:uint256 2)))
(@ CTC (define 131/PrimApp:bool (PEQ 85/PureIf:uint256 3)))
(@ CTC (define 132/PureIf:bytes (IF_THEN_ELSE 131/PrimApp:bool "Alice quits" "Bob quits")))
(@ CTC (define 133/PureIf:bytes (IF_THEN_ELSE 130/PrimApp:bool "Alice wins" 132/PureIf:bytes)))
(@ CTC (define 134/PureIf:bytes (IF_THEN_ELSE 129/PrimApp:bool "Draw" 133/PureIf:bytes)))
(@ CTC (define 135/PureIf:bytes (IF_THEN_ELSE 128/PrimApp:bool "Bob wins" 134/PureIf:bytes)))
135/PureIf:bytes