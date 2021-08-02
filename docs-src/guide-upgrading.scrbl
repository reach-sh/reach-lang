#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-upgrading"]{Upgrading Reach}

The directions below show you how to upgrade Reach. They assume that you installed the @link["https://github.com/reach-sh/reach-lang/blob/master/reach"]{reach} script file in @exec{~/reach}.

@itemize[#:style 'ordered

@item{
  Download the latest @exec{reach} script file:
  @cmd{cd ~/reach}
  @cmd{reach upgrade}
  This command renames your current @link["https://github.com/reach-sh/reach-lang/blob/master/reach"]{reach} script file to something like @emph{reach.20040}, and downloads the lastest.
}

@item{
  Remove @emph{all} Docker @emph{containers} (including those NOT associated with Reach):
  @cmd{reach docker-reset}
}

@item{
  Download the latest Reach Docker images:
  @cmd{reach update}

  You can list the new Reach Docker images by running @exec{reach hashes} or @exec{docker images} as in the examples below:

  @verbatim{
    $ reach hashes
    reach: c0117e03
    devnet-eth: c0117e03
    devnet-algo: c0117e03
    devnet-cfx: c0117e03
    runner: c0117e03
    react-runner: c0117e03
    rpc-server: c0117e03
  }

  @verbatim{
    $ docker images -f=reference="reachsh/*"
    REPOSITORY             TAG       IMAGE ID       CREATED        SIZE
    reachsh/reach          0.1       1af484204d66   9 hours ago    326MB
    reachsh/react-runner   0.1       fd9c65d0d585   9 hours ago    1.68GB
    reachsh/rpc-server     0.1       8c570f769913   9 hours ago    1.39GB
    reachsh/runner         0.1       5479d59a7902   9 hours ago    1.39GB
    reachsh/devnet-algo    0.1       3a29609a035a   6 days ago     151MB
    reachsh/devnet-cfx     0.1       1c152c76bb54   3 weeks ago    115MB
    reachsh/devnet-eth     0.1       93e1bf156a88   4 months ago   45.7MB
  }
}
]