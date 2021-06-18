FROM cimg/base:stable-18.04

RUN mkdir /tmp/workspace
COPY example/ /tmp/workspace/record
ENV CIRCLE_USERNAME="jeapostrophe"
ENV CIRCLE_BRANCH="master"
ENV CIRCLE_BUILD_URL="https://circleci.com/gh/reach-sh/reach-lang/27839"

COPY record.py /

ENTRYPOINT ["/record.py"]
