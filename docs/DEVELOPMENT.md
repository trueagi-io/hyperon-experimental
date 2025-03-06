# Instructions for developers

## How to release Python distribution packages locally

Python packages are released using
[cibuildwheel](https://pypi.org/project/cibuildwheel/). First step is to setup
it. Usually it means setup docker and install the package from PyPi (see [setup
instructions](https://cibuildwheel.pypa.io/en/stable/setup/#local)).

There are additional preparations to be made. First of all it is needed to
allow building and installing `libhyperonc` library on a build environment.
`cibuildwheel` uses isolated docker container for each kind of platform it
supports. Only code of the Python package is copied into container
automatically. Code of the `libhyperonc` library should be downloaded from
outside. It means one need to have the code in some repo accessible from the
container before starting release. The simplest way is to push the changes in
your GitHub repo fork.

By default library downloads and installs version from the `main` branch of the
`trueagi-io/hyperon-experimental` repository. Using a custom branch is done by
passing custom parameters to the `install-hyperonc.sh` script through
[CIBW_BEFORE_ALL](https://cibuildwheel.pypa.io/en/stable/options/#before-all)
environment variable:
```
export CIBW_BEFORE_ALL='sh -c "./python/install-hyperonc.sh -u <git-repo-url> -r <git-branch>"'
```
One should replace `<git-repo-url>` and `<git-branch>` by the repo URL and
branch which are used in release.

Also it is possible to start from building the only platform to quickly check
whether release works. This can be done using
[CIBW_BUILD](https://cibuildwheel.pypa.io/en/stable/options/#build-skip)
variable:
```
export CIBW_BUILD=cp37-manylinux_x86_64
```

After exporting the variables above one can start release by executing
`cibuildwheel ./python` from the root directory of the repo. See [cibuildwheel
documentation](https://cibuildwheel.pypa.io/en/stable/) for details.

## How to update the version

Usually it is needed before releasing the artifacts or before making a test
release.

There are three locations to update:
- [/Cargo.toml](/Cargo.toml) file:
  - `workspace.package.version` property
  - `workspace.dependencies.hyperon.version` property
- [/python/VERSION](/python/VERSION) file

All three locations should contain the same version.

## How to release binaries

Update the version [How to update the version](#how-to-update-the-version) in
the main branch of the repository, raise PR and merge it. Use [Create a new
release link](https://github.com/trueagi-io/hyperon-experimental/releases/new)
on the main page of the GitHub repo. Press `Choose a tag` control and type new
tag which should be in form of `v<version>` (for example if version is
`0.1.7` then tag is `v0.1.7`). After typing the tag press `Create new tag
on publish`. Now press `Generate release notes` button. It will automatically
fill the `Release title` and `Release description` fields. Tick `Set as a
pre-release` checkbox if needed and press `Publish release` button.  Now you
have published new GitHub release and triggered a job to build release
artifacts.

After release job is finished one need to approve publishing artifacts to the
PyPi repository. Before approving one can download and test Python wheels
built. To check the job status go the `Actions/release-pyhon` page, and select
last workflow run. Links to the archives with the artifacts are located at the
bottom of the page.

If distribution wheels are good then one can approve the publishing process. At
the top of the workflow run page there are two blocks `Publish to Test PyPi`
and `Publish to PyPi`. First press `Publish to Test PyPi` block approve it and
wait for publishing. It is critical to start from Test PyPi because release
cannot be removed from the PyPi after publishing.

After release is published check it can be installed executing:
```
python3 -m pip install --index-url https://test.pypi.org/simple/ hyperon
```
Check that the latest published version was downloaded and installed. If you
are making a test release then you should not publish it to the PyPi. If it is
a production release then proceed with `Publish to PyPi` block.

## How to check release job in fork

First you need to select the test release version. It should contain an
additional version digit after the latest officially released version. Let's
say the latest released version is `0.1.7`. Then the test release version
should be `0.1.7.x` for instance `0.1.7.1`. Start from 1 and increment it after
each release you published successfully.

Make a separate branch to release the code. It is not necessary but it is
highly recommended to not pollute the main branch of the fork. In order to be
able releasing from the branch one need to temporary make it default branch. It
is done by using GitHub repo `Settings/General/Default branch` control.

[Update the version](#how-to-update-the-version) in the branch to the test
release version you constructed. Commit and push this change in your test
branch. Now you are ready to make a test release. See [release
binaries instruction](#how-to-release-binaries).

After testing the release procedure remove the commit with version update from
your branch. And set default branch setting to the previous value.
