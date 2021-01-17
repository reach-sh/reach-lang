import json
import os
import random
import requests
from concurrent.futures import ALL_COMPLETED, ThreadPoolExecutor, wait
from time               import sleep


def mk_rpc(proto, host, port):
    def rpc(m, *args):
        lab = 'RPC %s %s' % (m, json.dumps([*args]))
        print(lab)
        ans = requests.post('%s://%s:%s%s' % (proto, host, port, m),
                            json=[*args])
        print('%s ==> %s' % (lab, json.dumps(ans.json())))
        return ans.json()

    def rpc_callbacks(m, arg, cbacks):
        vals  = {k: v    for k, v in cbacks.items() if not callable(v)}
        meths = {k: True for k, v in cbacks.items() if     callable(v)}  # noqa
        p = rpc(m, arg, vals, meths)
        print(p)
        return p

    return rpc, rpc_callbacks


def main():
    print('I am the client')
    sleep(3)  # TODO wait until server becomes available to fulfill requests
    host = os.environ['REACH_RPC_SERVER']
    port = os.environ['REACH_RPC_PORT']

    rpc, rpc_callbacks = mk_rpc('http', host, port)

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
        hand = random.randint(0, 2)
        print('%s played %s' % (who, HAND[hand]))
        return hand

    acceptWager = lambda amt: print('Bob accepts the wager of %s' % fmt(amt))

    player = lambda who: dict(
        getHand=lambda: getHand(who),
        informTimeout=lambda: print('%s observed a timeout' % who),
        seeOutcome=lambda o: print('%s saw outcome %s' % (who, OUTCOME[o])))

    play_alice = lambda: rpc_callbacks(
        '/backend/Alice',
        ctc_alice,
        dict(wager=rpc('/stdlib/parseCurrency', 5), **player('Alice')))

    play_bob = lambda: rpc_callbacks(
        '/backend/Bob',
        ctc_bob,
        dict(acceptWager=acceptWager), **player('Bob'))

    with ThreadPoolExecutor(max_workers=2) as ex:
        t_alice = ex.submit(play_alice)
        t_bob   = ex.submit(play_bob)

        wait([t_alice, t_bob], return_when=ALL_COMPLETED)

    after_alice = get_balance(acc_alice)
    after_bob   = get_balance(acc_bob)

    print('Alice went from %s to %s' % (before_alice, after_alice))
    print('  Bob went from %s to %s' % (before_bob,   after_bob))

    rpc('/quit')


if __name__ == '__main__':
    main()
