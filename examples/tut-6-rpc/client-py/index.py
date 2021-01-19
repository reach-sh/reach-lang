import json
import os
import random
import requests
import socket
import time
from threading import Thread


# https://gist.github.com/butla/2d9a4c0f35ea47b7452156c96a4e7b12
def wait_for_port(port, host='localhost', timeout=5.0):
    """Wait until a port starts accepting TCP connections.
    Args:
        port (int): Port number.
        host (str): Host address on which the port should exist.
        timeout (float): In seconds. How long to wait before raising errors.
    Raises:
        TimeoutError: The port isn't accepting connections after time specified
        in `timeout`.
    """
    start_time = time.perf_counter()
    while True:
        try:
            with socket.create_connection((host, port), timeout=timeout):
                break
        except OSError as ex:
            time.sleep(0.01)
            if time.perf_counter() - start_time >= timeout:
                raise TimeoutError('Waited too long for the port {} '
                                   'on host {} to start accepting connections.'
                                   .format(port, host)) from ex


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

    wait_for_port(port, host)
    return rpc, rpc_callbacks


def main():
    print('I am the client')

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

    alice = Thread(target=play_alice)
    bob   = Thread(target=play_bob)

    alice.start()
    bob.start()

    alice.join()
    bob.join()

    after_alice = get_balance(acc_alice)
    after_bob   = get_balance(acc_bob)

    print('Alice went from %s to %s' % (before_alice, after_alice))
    print('  Bob went from %s to %s' % (before_bob,   after_bob))

    rpc('/stop')


if __name__ == '__main__':
    main()
