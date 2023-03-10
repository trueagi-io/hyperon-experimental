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

Please don't include number and description of the issue into a commit summary
line. Use `Fixes #<issue-number>` in the pull request description to link the
PR to the issue.

PR should satisfy the following requirement before being merged:
- contain latest changes from the repo;
- pass tests;
- be reviewed.

Feel free to raise draft PR if you advice or help with your change.

## Code style

We have no specific code style rules for now. Please take a look at the
existing code and stick to its style.
