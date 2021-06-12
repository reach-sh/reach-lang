'reach 0.1';
'use strict';

export const main =
  Reach.App(
    {},
    [ Participant('A', {}),
      View('NFT', { owner: Address }),
      View('NFT', { owner: Address }),
    ],
    (A, vNFT1, vNFT2) => {
      exit();
    });

