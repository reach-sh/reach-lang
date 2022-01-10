echo "Building Docker image to publish"
echo "version $VER of our extension..."
# Give the new image a tag of "img".
docker build -t img .

# Give "publish $VER -p $PAT" all
# as arguments to vsce, which is the
# tool we use to publish our extension.
# We need the "publish" argument to
# publish our extension at all.
# We need the "$VER" argument to have
# our extension's version synchronize
# with the rest of our versioning.
# We need the "-p $PAT" arguments to
# have the permissions necessary to
# publish this extension under our
# organization's name.
echo "Running Docker image to publish"
echo "version $VER of our extension..."
# ^--^ SC2086: Double quote to prevent
# globbing and word splitting.
docker run img publish "$VER" -p "$PAT"