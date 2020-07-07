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
  )
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
  (timeout 10 
    (@ CTC (require! True))
    (@ False 0/A:address 
      (publish! ())
      (pay! 0)
      )
    (@ CTC (define 139/PrimApp:uint256 (TXN_VALUE)))
    (@ CTC (define 140/PrimApp:bool (PEQ 139/PrimApp:uint256 0)))
    (@ CTC (require! 140/PrimApp:bool))
    (@ CTC (define 141/PrimApp:uint256 (BALANCE)))
    (@ CTC (transfer! 0/A:address 141/PrimApp:uint256))
    (commit!)
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
, address ) ) (define 27/Digest:uint256 (digest 26/PrimApp:uint256 22/PureIf:uint256)))
(@ ( 0
, (A, address) ) (define 28/Declassify:uint256 (declassify 27/Digest:uint256)))
(@ (0, (A, address)) (define 29/Interact:bool (interact.commits bool )))
(@ False 0/A:address 
  (publish! (28/Declassify:uint256))
  (pay! 0)
  (timeout 10 
    (@ CTC (require! True))
    (@ False 3/B:address 
      (publish! ())
      (pay! 0)
      )
    (@ CTC (define 136/PrimApp:uint256 (TXN_VALUE)))
    (@ CTC (define 137/PrimApp:bool (PEQ 136/PrimApp:uint256 0)))
    (@ CTC (require! 137/PrimApp:bool))
    (@ CTC (define 138/PrimApp:uint256 (BALANCE)))
    (@ CTC (transfer! 3/B:address 138/PrimApp:uint256))
    (commit!)
    "Alice quits"))
(@ CTC (define 30/PrimApp:uint256 (TXN_VALUE)))
(@ CTC (define 31/PrimApp:bool (PEQ 30/PrimApp:uint256 0)))
(@ CTC (require! 31/PrimApp:bool))
(commit!)
(@ (3, (B, address)) (define 32/Interact:bytes (interact.getHand bytes )))
(@ ( 3
, (B, address) ) (define 33/PrimApp:bool (BYTES_EQ 32/Interact:bytes "ROCK")))
(@ ( 3
, (B, address) ) (define 34/PrimApp:bool (BYTES_EQ 32/Interact:bytes "PAPER")))
(@ ( 3
, ( B
, address ) ) (define 35/PrimApp:bool (BYTES_EQ 32/Interact:bytes "SCISSORS")))
(@ ( 3
, ( B
, address ) ) (define 36/PureIf:bool (IF_THEN_ELSE 33/PrimApp:bool True 34/PrimApp:bool)))
(@ ( 3
, ( B
, address ) ) (define 37/PureIf:bool (IF_THEN_ELSE 36/PureIf:bool True 35/PrimApp:bool)))
(@ (3, (B, address)) (assume! 37/PureIf:bool))
(@ ( 3
, (B, address) ) (define 38/PureIf:uint256 (IF_THEN_ELSE 34/PrimApp:bool 1 2)))
(@ ( 3
, ( B
, address ) ) (define 39/PureIf:uint256 (IF_THEN_ELSE 33/PrimApp:bool 0 38/PureIf:uint256)))
(@ (3, (B, address)) (define 40/PrimApp:bool (PLE 0 39/PureIf:uint256)))
(@ (3, (B, address)) (define 41/PrimApp:bool (PLT 39/PureIf:uint256 3)))
(@ ( 3
, ( B
, address ) ) (define 42/PureIf:bool (IF_THEN_ELSE 40/PrimApp:bool 41/PrimApp:bool False)))
(@ (3, (B, address)) (assert! 42/PureIf:bool))
(@ ( 3
, (B, address) ) (define 43/Declassify:uint256 (declassify 39/PureIf:uint256)))
(@ (3, (B, address)) (define 44/Interact:bool (interact.shows bool )))
(@ False 3/B:address 
  (publish! (43/Declassify:uint256))
  (pay! 0)
  (timeout 10 
    (@ CTC (require! True))
    (@ False 0/A:address 
      (publish! ())
      (pay! 0)
      )
    (@ CTC (define 133/PrimApp:uint256 (TXN_VALUE)))
    (@ CTC (define 134/PrimApp:bool (PEQ 133/PrimApp:uint256 0)))
    (@ CTC (require! 134/PrimApp:bool))
    (@ CTC (define 135/PrimApp:uint256 (BALANCE)))
    (@ CTC (transfer! 0/A:address 135/PrimApp:uint256))
    (commit!)
    "Bob quits"))
(@ CTC (define 45/PrimApp:uint256 (TXN_VALUE)))
(@ CTC (define 46/PrimApp:bool (PEQ 45/PrimApp:uint256 0)))
(@ CTC (require! 46/PrimApp:bool))
(@ CTC (define 47/PrimApp:bool (PLE 0 43/Declassify:uint256)))
(@ CTC (define 48/PrimApp:bool (PLT 43/Declassify:uint256 3)))
(@ CTC (define 49/PureIf:bool (IF_THEN_ELSE 47/PrimApp:bool 48/PrimApp:bool False)))
(@ CTC (require! 49/PureIf:bool))
(commit!)
(@ ( 0
, (A, address) ) (define 50/Declassify:uint256 (declassify 26/PrimApp:uint256)))
(@ ( 0
, (A, address) ) (define 51/Declassify:uint256 (declassify 22/PureIf:uint256)))
(@ (0, (A, address)) (define 52/PrimApp:bool (PLE 0 43/Declassify:uint256)))
(@ (0, (A, address)) (define 53/PrimApp:bool (PLT 43/Declassify:uint256 3)))
(@ ( 0
, ( A
, address ) ) (define 54/PureIf:bool (IF_THEN_ELSE 52/PrimApp:bool 53/PrimApp:bool False)))
(@ (0, (A, address)) (require! 54/PureIf:bool))
(@ (0, (A, address)) (define 55/PrimApp:bool (PEQ 43/Declassify:uint256 0)))
(@ (0, (A, address)) (define 56/PrimApp:bool (PEQ 43/Declassify:uint256 1)))
(@ ( 0
, ( A
, address ) ) (define 57/PureIf:bytes (IF_THEN_ELSE 56/PrimApp:bool "PAPER" "SCISSORS")))
(@ ( 0
, ( A
, address ) ) (define 58/PureIf:bytes (IF_THEN_ELSE 55/PrimApp:bool "ROCK" 57/PureIf:bytes)))
(@ ( 0
, ( A
, address ) ) (define 59/Interact:bool (interact.reveals bool 58/PureIf:bytes)))
(@ False 0/A:address 
  (publish! (50/Declassify:uint256 51/Declassify:uint256))
  (pay! 0)
  (timeout 10 
    (@ CTC (require! True))
    (@ False 3/B:address 
      (publish! ())
      (pay! 0)
      )
    (@ CTC (define 130/PrimApp:uint256 (TXN_VALUE)))
    (@ CTC (define 131/PrimApp:bool (PEQ 130/PrimApp:uint256 0)))
    (@ CTC (require! 131/PrimApp:bool))
    (@ CTC (define 132/PrimApp:uint256 (BALANCE)))
    (@ CTC (transfer! 3/B:address 132/PrimApp:uint256))
    (commit!)
    "Alice quits"))
(@ CTC (define 60/PrimApp:uint256 (TXN_VALUE)))
(@ CTC (define 61/PrimApp:bool (PEQ 60/PrimApp:uint256 0)))
(@ CTC (require! 61/PrimApp:bool))
(@ CTC (define 62/Digest:uint256 (digest 50/Declassify:uint256 51/Declassify:uint256)))
(@ CTC (define 63/PrimApp:bool (PEQ 28/Declassify:uint256 62/Digest:uint256)))
(@ CTC (require! 63/PrimApp:bool))
(@ CTC (define 64/PrimApp:bool (PLE 0 51/Declassify:uint256)))
(@ CTC (define 65/PrimApp:bool (PLT 51/Declassify:uint256 3)))
(@ CTC (define 66/PureIf:bool (IF_THEN_ELSE 64/PrimApp:bool 65/PrimApp:bool False)))
(@ CTC (require! 66/PureIf:bool))
(@ CTC (define 67/PrimApp:bool (PLE 0 51/Declassify:uint256)))
(@ CTC (define 68/PrimApp:bool (PLT 51/Declassify:uint256 3)))
(@ CTC (define 69/PureIf:bool (IF_THEN_ELSE 67/PrimApp:bool 68/PrimApp:bool False)))
(@ CTC (define 70/PrimApp:bool (PLE 0 43/Declassify:uint256)))
(@ CTC (define 71/PrimApp:bool (PLT 43/Declassify:uint256 3)))
(@ CTC (define 72/PureIf:bool (IF_THEN_ELSE 70/PrimApp:bool 71/PrimApp:bool False)))
(@ CTC (define 73/PureIf:bool (IF_THEN_ELSE 69/PureIf:bool 72/PureIf:bool False)))
(@ CTC (define 74/PrimApp:uint256 (SUB 4 43/Declassify:uint256)))
(@ CTC (define 75/PrimApp:uint256 (ADD 51/Declassify:uint256 74/PrimApp:uint256)))
(@ CTC (define 76/PrimApp:uint256 (MOD 75/PrimApp:uint256 3)))
(@ CTC (define 77/PureIf:uint256 (IF_THEN_ELSE 72/PureIf:bool 0 1)))
(@ CTC (define 78/PureIf:uint256 (IF_THEN_ELSE 69/PureIf:bool 2 77/PureIf:uint256)))
(@ CTC (define 79/PureIf:uint256 (IF_THEN_ELSE 73/PureIf:bool 76/PrimApp:uint256 78/PureIf:uint256)))
(@ CTC (define 80/PrimApp:bool (PLE 0 79/PureIf:uint256)))
(@ CTC (define 81/PrimApp:bool (PLT 79/PureIf:uint256 5)))
(@ CTC (define 82/PureIf:bool (IF_THEN_ELSE 80/PrimApp:bool 81/PrimApp:bool False)))
(@ CTC (assert! 82/PureIf:bool))
(@ CTC (define 83/PrimApp:bool (PEQ 79/PureIf:uint256 2)))
(@ CTC (define 84/PrimApp:bool (PLE 0 51/Declassify:uint256)))
(@ CTC (define 85/PrimApp:bool (PLT 51/Declassify:uint256 3)))
(@ CTC (define 86/PureIf:bool (IF_THEN_ELSE 84/PrimApp:bool 85/PrimApp:bool False)))
(@ CTC (define 87/PureIf:bool (IF_THEN_ELSE 83/PrimApp:bool False True)))
(@ CTC (define 88/PureIf:bool (IF_THEN_ELSE 87/PureIf:bool True 86/PureIf:bool)))
(@ CTC (assert! 88/PureIf:bool))
(@ CTC (define 89/PrimApp:bool (PEQ 79/PureIf:uint256 0)))
(@ CTC (define 90/PrimApp:bool (PLE 0 43/Declassify:uint256)))
(@ CTC (define 91/PrimApp:bool (PLT 43/Declassify:uint256 3)))
(@ CTC (define 92/PureIf:bool (IF_THEN_ELSE 90/PrimApp:bool 91/PrimApp:bool False)))
(@ CTC (define 93/PureIf:bool (IF_THEN_ELSE 89/PrimApp:bool False True)))
(@ CTC (define 94/PureIf:bool (IF_THEN_ELSE 93/PureIf:bool True 92/PureIf:bool)))
(@ CTC (assert! 94/PureIf:bool))
(@ CTC (define 95/PrimApp:bool (PEQ 79/PureIf:uint256 2)))
(@ CTC (define 96/PrimApp:bool (PEQ 51/Declassify:uint256 0)))
(@ CTC (define 97/PureIf:bool (IF_THEN_ELSE 96/PrimApp:bool 95/PrimApp:bool False)))
(@ CTC (possible? 97/PureIf:bool))
(@ CTC (define 98/PrimApp:bool (PEQ 51/Declassify:uint256 1)))
(@ CTC (define 99/PureIf:bool (IF_THEN_ELSE 98/PrimApp:bool 95/PrimApp:bool False)))
(@ CTC (possible? 99/PureIf:bool))
(@ CTC (define 100/PrimApp:bool (PEQ 51/Declassify:uint256 2)))
(@ CTC (define 101/PureIf:bool (IF_THEN_ELSE 100/PrimApp:bool 95/PrimApp:bool False)))
(@ CTC (possible? 101/PureIf:bool))
(@ CTC (define 102/PrimApp:bool (PEQ 79/PureIf:uint256 0)))
(@ CTC (define 103/PrimApp:bool (PEQ 43/Declassify:uint256 0)))
(@ CTC (define 104/PureIf:bool (IF_THEN_ELSE 103/PrimApp:bool 102/PrimApp:bool False)))
(@ CTC (possible? 104/PureIf:bool))
(@ CTC (define 105/PrimApp:bool (PEQ 43/Declassify:uint256 1)))
(@ CTC (define 106/PureIf:bool (IF_THEN_ELSE 105/PrimApp:bool 102/PrimApp:bool False)))
(@ CTC (possible? 106/PureIf:bool))
(@ CTC (define 107/PrimApp:bool (PEQ 43/Declassify:uint256 2)))
(@ CTC (define 108/PureIf:bool (IF_THEN_ELSE 107/PrimApp:bool 102/PrimApp:bool False)))
(@ CTC (possible? 108/PureIf:bool))
(@ CTC (define 109/PrimApp:bool (PEQ 79/PureIf:uint256 2)))
(@ CTC (define 110/PrimApp:uint256 (MUL 2 5/Declassify:uint256)))
(@ CTC (define 111/PrimApp:bool (PEQ 79/PureIf:uint256 0)))
(@ CTC (define 112/PrimApp:uint256 (MUL 2 5/Declassify:uint256)))
(@ CTC (define 113/PureIf:uint256 (IF_THEN_ELSE 111/PrimApp:bool 0 5/Declassify:uint256)))
(@ CTC (define 114/PureIf:uint256 (IF_THEN_ELSE 111/PrimApp:bool 112/PrimApp:uint256 5/Declassify:uint256)))
(@ CTC (define 115/PureIf:uint256 (IF_THEN_ELSE 109/PrimApp:bool 110/PrimApp:uint256 113/PureIf:uint256)))
(@ CTC (define 116/PureIf:uint256 (IF_THEN_ELSE 109/PrimApp:bool 0 114/PureIf:uint256)))
(@ CTC (define 117/PrimApp:uint256 (ADD 6/Declassify:uint256 115/PureIf:uint256)))
(@ CTC (transfer! 0/A:address 117/PrimApp:uint256))
(@ CTC (transfer! 3/B:address 116/PureIf:uint256))
(commit!)
(@ CTC (define 118/Interact:bool (interact.outcome bool )))
(@ CTC (define 119/PrimApp:bool (PLE 0 79/PureIf:uint256)))
(@ CTC (define 120/PrimApp:bool (PLT 79/PureIf:uint256 5)))
(@ CTC (define 121/PureIf:bool (IF_THEN_ELSE 119/PrimApp:bool 120/PrimApp:bool False)))
(@ CTC (require! 121/PureIf:bool))
(@ CTC (define 122/PrimApp:bool (PEQ 79/PureIf:uint256 0)))
(@ CTC (define 123/PrimApp:bool (PEQ 79/PureIf:uint256 1)))
(@ CTC (define 124/PrimApp:bool (PEQ 79/PureIf:uint256 2)))
(@ CTC (define 125/PrimApp:bool (PEQ 79/PureIf:uint256 3)))
(@ CTC (define 126/PureIf:bytes (IF_THEN_ELSE 125/PrimApp:bool "Alice quits" "Bob quits")))
(@ CTC (define 127/PureIf:bytes (IF_THEN_ELSE 124/PrimApp:bool "Alice wins" 126/PureIf:bytes)))
(@ CTC (define 128/PureIf:bytes (IF_THEN_ELSE 123/PrimApp:bool "Draw" 127/PureIf:bytes)))
(@ CTC (define 129/PureIf:bytes (IF_THEN_ELSE 122/PrimApp:bool "Bob wins" 128/PureIf:bytes)))
129/PureIf:bytes