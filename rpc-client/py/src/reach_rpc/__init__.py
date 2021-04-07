# flake8: noqa

import json
import os
import requests
import socket
import time
import urllib3


def mk_rpc(opts={}):
    def opt_of(field, envvar, default=None, f=lambda x: x):
        opt =  f(opts.get(field))        if opts.get(field)        is not None \
          else f(os.environ.get(envvar)) if os.environ.get(envvar) is not None \
          else default

        if opt is None:
            raise RuntimeError('Mandatory configuration unset for: %s' % field)

        return opt

    host    = opt_of('host',    'REACH_RPC_SERVER')
    port    = opt_of('port',    'REACH_RPC_PORT')
    key     = opt_of('key',     'REACH_RPC_KEY')
    timeout = opt_of('timeout', 'REACH_RPC_TIMEOUT',               f=int, default=5)
    verify  = opt_of('verify',  'REACH_RPC_TLS_REJECT_UNVERIFIED', f=lambda x: x != '0')

    if not verify:
        urllib3.disable_warnings()
        print('\n*** Warning! TLS verification disabled! ***\n')
        print(' This is highly insecure in Real Life™ applications and must')
        print(' only be permitted under controlled conditions (such as')
        print(' during development).\n')

    # From: https://gist.github.com/butla/2d9a4c0f35ea47b7452156c96a4e7b12
    start_time = time.perf_counter()
    while True:
        try:
            with socket.create_connection((host, port), timeout=timeout):
                break
        except OSError as ex:
            time.sleep(0.01)
            if time.perf_counter() - start_time >= timeout:
                raise TimeoutError('Waited too long for the port {} '
                                   'on host {} to accept connection.'
                                   .format(port, host)) from ex

    def debug(s):
        if os.environ.get('REACH_DEBUG') is not None:
            print(s)

    def rpc(m, *args):
        lab = 'RPC %s %s' % (m, json.dumps([*args]))
        debug(lab)
        ans = requests.post('https://%s:%s%s' % (host, port, m),
                            json    = [*args],
                            headers = {'X-API-Key': key},
                            verify  = verify)
        ans.raise_for_status()
        debug('%s ==> %s' % (lab, json.dumps(ans.json())))
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
                raise Exception('Illegal callback return: %s' % json.dumps(p))

    return rpc, rpc_callbacks
