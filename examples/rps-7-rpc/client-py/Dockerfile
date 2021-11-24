FROM       python:3.8-alpine
WORKDIR    /app
ARG        VERSION

COPY       ./lib/ ./lib
RUN        python3.8 -m venv venv

# This example `pip install`s a local wheel distribution in order to remain
# in-sync with the repository's client code, but frontend authors will normally
# install a pinned version directly from PyPI like so:
#   $ `pip install reach-rpc-client==x.y.z`

# Always opt for newest `pip`; pin everything else
# hadolint ignore=DL3013,SC1091
RUN        . /app/venv/bin/activate \
             && pip install --upgrade pip \
             && pip install "lib/reach_rpc_client-$VERSION-py3-none-any.whl"

COPY       index.py /app/

ENTRYPOINT ["/app/venv/bin/python", "-u"]
CMD        ["index.py"]
