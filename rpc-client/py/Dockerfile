FROM    python:3.7-alpine
WORKDIR /lib

RUN     python3.7 -m venv venv
RUN     ./venv/bin/pip install build

COPY    src ./src
COPY    LICENSE README.md VERSION pyproject.toml setup.cfg ./

RUN     ./venv/bin/python -m build
