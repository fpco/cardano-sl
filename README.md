# Review of cardano-sl project

This is the `master` branch, it contains a README explaining how review works.

The second branch is [`upstream`](https://github.com/fpco/cardano-sl/tree/upstream), which contains a recent version of the cardano-sl master branch.

The third branch is [`reviewed`](https://github.com/fpco/cardano-sl/tree/reviewed), which contains fragments of the cardano project which have actually been reviewed by a pull request process.

Finally, sections of the project are committed into a branch and opened as a PR to be reviewed by one or more members of the team. Once reviews are complete and the code is merged, that part of the code is considered "reviewed". We may add notes to this master branch from our findings of reviewing the different components of the codebase.

The team recommends starting at this points:

- [Node entry point](https://github.com/fpco/cardano-sl/blob/upstream/src/node/Main.hs)
- [Core node logic](https://github.com/fpco/cardano-sl/blob/upstream/src/Pos/Launcher.hs), and the exported modules.
- [Listeners](https://github.com/fpco/cardano-sl/blob/upstream/src/Pos/Communication/)
- [Workers](https://github.com/fpco/cardano-sl/blob/upstream/src/Pos/Worker/)
