# Instructions for developers

## How to check Python release procedure locally

Python packages are released using
[cibuildwheel](https://pypi.org/project/cibuildwheel/). First step is to setup
it. Usually it means setup docker and install the package from PyPi (see [setup
instructions](https://cibuildwheel.pypa.io/en/stable/setup/#local)).

There are additional preparations to be made before running it. First of all
`libhyperonc` library should be built and installed in a build environment. It
is done by `install-hyperonc.sh` script which is called using
[CIBW_BEFORE_ALL](https://cibuildwheel.pypa.io/en/stable/options/#before-all)
environment variable:
```
export CIBW_BEFORE_ALL='sh -c "./python/install-hyperonc.sh -u <git-repo-url> -r <git-branch>"'
```

One should replace `<git-repo-url>` and `<git-branch>` by the repo URL and
branch which are used in release. It is required because `cibuildwheel` uses
isolated docker container for each kind of platform it supports. Only code of
the Python package is copied into container automatically. Code of the
`libhyperonc` library should be downloaded from outside. It means one need to
have the code in some repo accessible from the container before starting
release. The simplest way is to push the changes in your GitHub repo fork.

Some platform are not supported. Use
[CIBW_SKIP](https://cibuildwheel.pypa.io/en/stable/options/#build-skip) to skip
such platforms:
```
export CIBW_SKIP="*musllinux*"
```

Also one can start from building the only platform to quickly check whether
release works. This can be done using
[CIBW_BUILD](https://cibuildwheel.pypa.io/en/stable/options/#build-skip)
variable:
```
export CIBW_BUILD=cp37-manylinux_x86_64
```

After exporting the variables above one can start release by executing
`cibuildwheel` from the `./python` directory of the repo. See [cibuildwheel
documentation](https://cibuildwheel.pypa.io/en/stable/) for details.
