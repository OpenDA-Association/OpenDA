# Contributing to OpenDA

This document briefly describes the policy for contributing changes to OpenDA.
First we would like to emphasize that anyone can create a local copy and make your own
local modifications of OpenDA using the normal commands of github and git. Only when you want to
share your code with the OpenDA community this document applies.

## Share your code with the OpenDA community

For OpenDA we distinguish two methods for contributing:

1.  Create a fork on github and then a pull request. When you contribute for the first time, then this is probably what you want.
2.  Ask a core-developer of OpenDA to create a branch where you can push you contributions. If you do not know who the core-developers of OpenDA are then you can contact one at info@openda.org

## Repository structure

As a general rule all code changes are first committed to a branch. These branches are usually personal, so you can work here in your without interference of other developments. There are a few special branches, with the name master or starting with release. These names are reserved and should not be used for development.

## Step by step

### Fork scenario

1.  Create a fork on github

2.  Clone your newly created fork `git clone https://github.com/<your account>/OpenDA.git`

3.  Create a branch with a name that describes your intended developments. `git checkout -b my_branch`

4.  Write and test your code in your branch.

5.  Commit to your local copy and push to your fork of OpenDA

    -   `git add bla`
    -   `git commit -m "my description"`
    -   `git push origin my_branch`

6.  Once finished or more regularly: merge changes in OpenDA into your work

    -   Update your fork of OpenDA on github
    -   `git fetch origin master` to update your local copy (from your fork)
    -   `git merge master`

7.  Push changes to the server again `git push origin my_branch` (to your fork)

8.  Create a pull request on github

### Directly on main repository scenario

1.  Clone your OpenDA repository `git clone https://github.com/OpenDA-Association/OpenDA.git`

2.  Create a branch with a name that describes your intended developments.  `git checkout -b my_branch`

3.  Write and test your code in your branch.

4.  Commit to local copy and push to your fork of OpenDA

    -   `git add bla`
    -   `git commit -m "my description"`
    -   `git push origin my_branch`

5.  Once finished or more regularly: merge changes in OpenDA into your work

    -   `git fetch origin master` to update your local copy
    -   `git merge master`

6.  Push changes to OpenDA on github again `git pus origin my_branch`

7.  Create a pull request on github

## Questions

Any questions? You can post them on the [Forum](https://sourceforge.net/p/openda/discussion/?source=navbar) or mail us at info@openda.org
