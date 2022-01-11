# Start with an image that has Node.js.
FROM node:16.13.0

# create a WORKDIR to prevent
# ERROR EPERM: operation not permitted
WORKDIR /extension-publishing-helper-dir

# copy current directory to image
COPY . .

# this is to avoid sh: 1: tsc: not found
# DO NOT do "RUN npm install"! This
# installs ALL dependencies, instead of
# just the bare minimum necessary to
# publish the extension. This could
# easily increase the build time of this
# step anywhere from 10x to 20x.
RUN npm install -g typescript

# install tool for extension publishing
RUN npm install -g vsce

# Publish our Visual Studio extension!
# It is very important to use ["vsce"]
# instead of, say, vsce. Running this
# command fails if used without the
# brackets [] and quotation marks "".
ENTRYPOINT ["vsce"]