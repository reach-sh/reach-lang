# Start with an image that has Node.js.
ARG NODE_IMAGE
FROM $NODE_IMAGE

# create a WORKDIR to prevent
# ERROR EPERM: operation not permitted
WORKDIR /extension-publishing-helper-dir

# copy current directory to image
COPY . .

# Install all of the dependencies necessary
# to bundle our extension using Webpack.
RUN npm install

# install tool for extension publishing
RUN npm install -g vsce

# Publish our Visual Studio extension!
# It is very important to use ["vsce"]
# instead of, say, vsce. Running this
# command fails if used without the
# brackets [] and quotation marks "".
ENTRYPOINT ["vsce"]