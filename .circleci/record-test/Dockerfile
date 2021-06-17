FROM cimg/base:stable-18.04

RUN mkdir /tmp/workspace
COPY example/ /tmp/workspace/record
COPY record.sh /

ENTRYPOINT ["/record.sh"]
