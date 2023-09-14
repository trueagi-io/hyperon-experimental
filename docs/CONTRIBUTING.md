# How to contribute

## Making changes

Before making changes create a personal fork of the repository. Sync fork and
create new branch from the latest version of the `main` branch. Create separate
branch for each change. Thus it is simpler to support consistent state of the
`main` in your fork.

Prefer incremental commits to one big commit which contains the whole change.
Prefer commits which passes all of the tests. If it is not possible to satisfy
both requirements at once you can make single commit which passes tests or mark
tests as ignored until change is done.

Each commit should have a message in the following format:
```
Change summary in 50 characters (less than 73 characters)

Optional detailed description of the change when required. Each line
is less than 73 character long.
```
Such commits looks better in GitHub history.

Please avoid generic commit messages like `Update README.md`.
Good commit message should describe the change, not a fact of the change.
For example `Add troubleshooting section about No module named 'hyperonpy'`.
By looking to the commit message history the reviewer should understand
the order and brief description of changes.
  
Please don't include number and description of the issue into a commit summary
line. Use `Fixes #<issue-number>` in the pull request description instead
to link the PR and the issue.

PR should satisfy the following requirement before being merged:
- contain latest changes from the repo;
- pass tests;
- be reviewed.

Feel free to raise draft PR if you need an advice or help with your changes.

## Code style

We have small set of code style rules for now. The rule of thumb is to take a look
at the existing code and stick to its style.

### General

If you want to leave some reminder in code, for example to fix something later,
you can do it by two ways. Add a comment starting with `FIXME` to mark something
which should be done before the PR is merged. Add a comment starting with `TODO`
to mark the improvement which can be postponed and done later by a separate PR.
The main purpose of a `TODO` comment is to trigger a developer who looks at the
code after you and make him fix the issue if it is appropriate. If the change or
question is big enough or it affects the API of the module it is better to raise
an issue instead.

### Libraries

When adding new library into the project please ensure you specify the exact
version instead of using ranges. The minor update of the library can break the
build unexpectedly. The broken build is a real burden because most of the users
build the project from the source.

### Rust

When working on Rust C API prefer making `unsafe` blocks as small as possible.
This makes it easier to find blocks that might be source of issues. Usually it is
not required to mark C API functions `unsafe` because they are not intended to
be used from the Rust safe code.

