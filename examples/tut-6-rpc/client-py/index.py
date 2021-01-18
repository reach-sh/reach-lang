import json
import os
import random
import requests
from concurrent.futures import ALL_COMPLETED, ThreadPoolExecutor, wait
from time               import sleep


def mk_rpc(proto='http',
           host=os.environ['REACH_RPC_SERVER'],
           port=os.environ['REACH_RPC_PORT']):

    def rpc(m, *args):
        lab = 'RPC %s %s' % (m, json.dumps([*args]))
        print(lab)
        ans = requests.post('%s://%s:%s%s' % (proto, host, port, m),
                            json=[*args])
        print('%s ==> %s' % (lab, json.dumps(ans.json())))
        return ans.json()

    def rpc_callbacks(m, arg, cbacks):
        vals  = {k: v    for k, v in cbacks.items() if not callable(v)}
        meths = {k: True for k, v in cbacks.items() if     callable(v)}
        p     = rpc(m, arg, vals, meths)

        while True:
            if p['t'] == 'Done':
                return p

            elif p['t'] == 'Kont':
                cback = cbacks[p['m']]
                ans   = cback(*p['args'])
                p     = rpc('/kont', p['kid'], ans)

            else:
                # TODO fix swallowed exceptions
                raise Exception('Illegal callback return: %s' % json.dumps(p))

    return rpc, rpc_callbacks


def main():
    print('I am the client')
    sleep(3)  # TODO wait until server becomes available to fulfill requests

    rpc, rpc_callbacks = mk_rpc()

    starting_balance = rpc('/stdlib/parseCurrency', 10)
    acc_alice        = rpc('/stdlib/newTestAccount', starting_balance)
    acc_bob          = rpc('/stdlib/newTestAccount', starting_balance)

    fmt          = lambda x: rpc('/stdlib/formatCurrency', x, 4)
    get_balance  = lambda w: fmt(rpc('/stdlib/balanceOf', w))

    before_alice = get_balance(acc_alice)
    before_bob   = get_balance(acc_bob)

    ctc_alice    = rpc('/acc/deploy', acc_alice)
    ctc_bob      = rpc('/acc/attach', acc_bob, rpc('/ctc/getInfo', ctc_alice))

    HAND         = ['Rock', 'Paper', 'Scissors']
    OUTCOME      = ['Bob wins', 'Draw', 'Alice wins']

    def getHand(who):
        hand = random.randint(0, 2)  # TODO better source of randomness(?)
        print('%s played %s' % (who, HAND[hand]))
        return hand

    acceptWager = lambda amt: print('Bob accepts the wager of %s' % fmt(amt))

    player = lambda who: \
        {'stdlib.hasRandom': True,
         'getHand':       lambda:   getHand(who),
         'informTimeout': lambda:   print('%s observed a timeout' % who),
         'seeOutcome':    lambda n: print('%s saw outcome %s'
            % (who, OUTCOME[rpc('/stdlib/bigNumberToNumber', n)]))
         }

    play_alice = lambda: rpc_callbacks(
        '/backend/Alice',
        ctc_alice,
        dict(wager=rpc('/stdlib/parseCurrency', 5), **player('Alice')))

    play_bob = lambda: rpc_callbacks(
        '/backend/Bob',
        ctc_bob,
        dict(acceptWager=acceptWager, **player('Bob')))

    with ThreadPoolExecutor() as ex:
        t_alice = ex.submit(play_alice)
        t_bob   = ex.submit(play_bob)

        wait([t_alice, t_bob], return_when=ALL_COMPLETED)

    after_alice = get_balance(acc_alice)
    after_bob   = get_balance(acc_bob)

    print('Alice went from %s to %s' % (before_alice, after_alice))
    print('  Bob went from %s to %s' % (before_bob,   after_bob))

    rpc('/stop')


if __name__ == '__main__':
    main()
