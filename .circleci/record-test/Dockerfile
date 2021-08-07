FROM cimg/base:stable-18.04

RUN mkdir -p /tmp/workspace
COPY record/ /tmp/workspace/record/
ENV CIRCLE_USERNAME="jeapostrophe"
ENV CIRCLE_BRANCH="master"
ENV CIRCLE_BUILD_URL="https://circleci.com/gh/reach-sh/reach-lang/27839"
ENV CIRCLE_SHA1="sha1"

ENV HOME="/home/circleci"
ENV PROJECT="${HOME}/project"
ENV PROCIRC="${PROJECT}/.circleci"
RUN mkdir -p "${PROCIRC}"
COPY xfail.txt xtime.txt record.py "${PROCIRC}"
COPY examples/ "${PROJECT}/examples/"

WORKDIR "${PROCIRC}"
ENTRYPOINT ["./record.py"]
