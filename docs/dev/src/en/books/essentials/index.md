---
hasOtp: true
menuItem: mi-docs
---

# Reach Essentials

Reach is a programming language for building blockchain applications and the smart contracts they deploy to consensus networks. View the carousel to learn more:

<div id="develop-and-deploy-carousel" class="carousel slide carousel-fade" data-bs-interval="false" style="background:black;">
  <div class="carousel-inner" style="text-align:center;background:black;">
    <div class="carousel-item active"><img src="slide01.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide02.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide03.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide04.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide05.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide06.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide07.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide08.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide09.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide10.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide11.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide12.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide13.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide14.png" class="img-fluid" width=880 height=493></div>
    <div class="carousel-item"><img src="slide15.png" class="img-fluid" width=880 height=493></div>
  </div>
  <button class="carousel-control-prev" type="button" data-bs-target="#develop-and-deploy-carousel" data-bs-slide="prev" style="justify-content: left;">
    <span class="carousel-control-prev-icon" aria-hidden="true"></span>
    <span class="visually-hidden">Previous</span>
  </button>
  <button class="carousel-control-next" type="button" data-bs-target="#develop-and-deploy-carousel" data-bs-slide="next" style="justify-content: right;">
    <span class="carousel-control-next-icon" aria-hidden="true"></span>
    <span class="visually-hidden">Next</span>
  </button>
</div>

# Reach DApps

Reach decentralized applications (DApps) enable participants to perform valid negotiations and transactions via smart contracts running on consensus networks. Consider this diagram:

<div><img src="reach-dapp.png" class="img-fluid my-4 d-block" width=800 height=439 loading="lazy"></div>

Match the items below to the numbers in the diagram:

1. Human beings give direction and receive information via user interfaces.
1. The deployer deploys the contract to a consensus network.
1. The attacher attaches to the contract and interacts with the deployer and/or other attachers.
1. Participant frontends include UIs and participant interact objects which facilitate communication with backends.
1. Participant backends facilitate communication between frontends and contracts.
1. Participants represent human beings in contract negotiations and transactions.
1. Contracts enable participants to interact, and they enforce rules including order of operation.
1. Buyer accounts *pay* currency to contract accounts which *transfer* currency to seller accounts.

In Reach, a programmer need only specify the actions of participants, what they do individually and what they do in unison. The Reach compiler automatically derives a contract for the consensus network via a connector that enforces these rules.

# Frontend and Backend

In Reach, the terms *frontend* and *backend* denote two halves of a Reach DApp, halves that communicate via interact objects. The following diagram highlights the frontend:

<div><img src="frontend-programming.png" class="img-fluid my-4 d-block" width=600 height=367 loading="lazy"></div>

Actually, developers write frontends in one of two ways:

1. Using JavaScript and the Reach JS Standard Library as seen in the diagram.
1. Using C#, Go, JavaScript, or Python and the relevant RPC library accessing the Reach RPC Server.

A pre-compiled backend, on the other hand, is written in the Reach programming language as highlighted in this diagram:

<div><img src="backend-programming.png" class="img-fluid my-4 d-block" width=600 height=367 loading="lazy"></div>

The Reach compiler transforms Reach code into the executable backend (JavaScript) and the smart contract (network-specific bytecode) as highlighted in this diagram:

<div><img src="backend-compiled.png" class="img-fluid my-4 d-block" width=600 height=367 loading="lazy"></div>

# Interact Objects

For each participant, a Reach DApp requires two interact objects, a frontend JavaScript object and a corresponding backend Reach object. The following diagram shows the frontend and backend interact objects for some *seller* participant:

<div><img src="matching-interacts.png" class="img-fluid my-4 d-block" width=700 height=151 loading="lazy"></div>

Frontend/backend pairs of interact objects form a communication pipeline between participant frontends and backends. In the diagram above, for example, the backend can call `reportReady` (passing an unsigned integer representing a price) which enables the frontend to do something with `price` like display it to the user.

You can factor properties and methods common to multiple participant interact objects into a `commonInteract` object, and then *spread* the common object into the various participant objects:

<div><img src="matching-interacts2.png" class="img-fluid my-4 d-block" width=700 loading="lazy"></div>

The following diagram provides a broader perspective:

<div><img src="design.png" class="img-fluid my-4 d-block" width=900 height=475 loading="lazy"></div>