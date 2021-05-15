'reach 0.1';

export const main =
    Reach.App(
        {},
        [
            Participant('Admin', {
                announce: Fun([Address,UInt], UInt),
            }),
            ParticipantClass('Nominee', {
                getParams: Fun([Address], UInt),
            }),
            ParticipantClass('Voter', {
                shouldBuyTicket: Fun([Tuple(UInt,UInt),Tuple(UInt,UInt)] , Tuple(UInt, UInt)),
                getBalance: Fun([Address] , UInt),
                shouldPay: Fun([] , Bool),
            })
        ],
        (Admin, Nominee, Voter) => {
            Admin.only(() => {
                const dummyAddress = this;
            });
            Admin.publish(dummyAddress);

            const idea = [dummyAddress, 0, 0];
            const getIndex = (m) => fromMaybe(m, (() => 0), ((x) => x));
            const [ideas,addresses ,ideaCount, titles] = 
            parallelReduce([Array.replicate(2, idea),Array.replicate(2, dummyAddress),0,Array.replicate(2, 0)])
            .invariant(balance() == 0)
            .while(ideaCount<=1)
            .case(Nominee,
                (() => {
                    const title = declassify(interact.getParams(this));
                    const addr = this; 
                    const index = getIndex(addresses.indexOf(this));
                return ({
                    msg : [title,addr]
                });
                }),
                ((bid) => (0)),
                ((msg) => {

                const newArray = Array.set(ideas,ideaCount, [msg[1], ideaCount,msg[0]]);
                const newAdress = Array.set(addresses,ideaCount, msg[1]);

                return addresses.includes(msg[1]) ? [ideas, addresses, ideaCount,titles] : [newArray, newAdress , ideaCount+1,titles.set(ideaCount,msg[0])];
                }))
            .timeout(false,()=>{
                return [ideas,addresses, 6,titles];
            }
            );

            const [ timeRemaining, keepGoing ] = makeDeadline(9);
            const [oylar , toplamPara] = parallelReduce([Array.replicate(2, 0),0])
                .invariant(balance()  == toplamPara)
                .while(keepGoing())
                .case(Voter,
                    (() => {
                        const mbid = declassify(interact.shouldBuyTicket([titles[0],titles[1]],[oylar[0],oylar[1]]));
                        return ({
                            msg: mbid
                        });
                    }),
                        ((bid) => bid[1]),
                        ((choices) => {
                        if(choices[0]>=2){
                            transfer(choices[1]).to(this);
                            return [oylar,toplamPara];
                        }else{
                            return [oylar.set(choices[0],oylar[choices[0]]+1) ,toplamPara+choices[1]];
                        }
                    })
                ).timeRemaining(timeRemaining());

            const winnerIndex = getIndex(oylar.indexOf(oylar.max()));
            const winnerAddress = ideas[winnerIndex][0];
            transfer(balance()).to(winnerAddress);

            commit();
            Admin.only(() => {
                const winnerTitle = ideas[winnerIndex][2];
            });
            Admin.publish(winnerTitle);            

            commit();
            exit();
     });
