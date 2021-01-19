FROM       python:3.8-alpine
WORKDIR    /app

RUN        python3.8 -m venv venv

# Always opt for newest `pip`; pin everything else
# hadolint ignore=DL3013,SC1091
RUN        . /app/venv/bin/activate \
             && pip install --upgrade pip \
             && pip install certifi==2020.12.5 \
                            chardet==4.0.0 \
                            idna==2.10 \
                            requests==2.25.1 \
                            urllib3==1.26.2

COPY       index.py /app/

ENTRYPOINT ["/app/venv/bin/python"]
CMD        ["index.py"]
