FROM       python:3.8-alpine

WORKDIR    /app

# Install and cache `pip` dependencies
RUN        python3.8 -m venv venv
COPY       pip-deps.txt /app
RUN        source /app/venv/bin/activate \
             && pip install --upgrade pip \
             && pip install -r pip-deps.txt

COPY       tox.ini index.py /app/
RUN        /app/venv/bin/flake8 --config /app/tox.ini /app/index.py

ENTRYPOINT ["/app/venv/bin/python"]
CMD        ["index.py"]
