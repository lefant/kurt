![kurt logo][logo]


Kurt is a [Computer Go][computer_go] program written in Haskell.

It is named after the logician [Kurt Gödel][kurt_goedel] and written by
[Fabian Linzberger][lefant_net].


Source code is [available online on github][github_kurt]. 

Comments and contributions always welcome!


It has also played online on the Kiseido go server ([KGS][kgs]) with
the nick "kurtBot". It is only playing on 9x9 and its strength there
is about 25 kyu.


KGS also provides an archive of [all of kurtBots games][kgs_kurt].


## running and testing in an Amp orb

Fresh orbs prepare the Haskell toolchain and build Kurt automatically. See [Accessing Kurt in an Amp orb](docs/orb-access.md) for Terminal testing, optional temporary SSH access, and the limitations of portals for stdin/stdout GTP engines.



## running stdin/stdout gtp through docker (for example to attach gogui)

```
docker run -i lefant/kurt
```


## running through docker-compose on kgs

(adjust env variables!)
```
NAME=myBot PASSWORD=secret KGSGTP_ARGS='opponent=lefant' docker-compose up
```

## Documentation & Process

This is a small legacy maintenance repository. Add documentation only when it has
durable value; do not create empty documentation directories.

- **`docs/orb-access.md`**: Current operational access guidance.
- **`docs/brainstorms/`**: Requirements and design exploration.
- **`docs/plans/`**: Implementation plans for non-trivial changes.
- **`docs/solutions/`**: Verified reusable Haskell, GTP, and workflow lessons,
  organized by category with YAML frontmatter (`module`, `tags`, `problem_type`).
- **`docs/research/`**, **`docs/devlog/`**, **`docs/specs/`**,
  **`docs/decisions/`**, and **`docs/changelog/`**: Create only when a change needs
  that type of durable record.

**Workflow**: Research/Plan → Implement → Review → Compound

Understand the current GTP behavior and old-toolchain constraints before planning.
Implement the smallest change, run the relevant Cabal and GTP checks, review protocol
correctness and coverage, then capture only genuinely reusable learning in
`docs/solutions/`. Put durable research in `docs/research/`, plans in `docs/plans/`,
and meaningful session outcomes in `docs/devlog/`.

Prefer Compound Engineering skills or an equivalent workflow bundle when available.
Use verified names and output paths. Supporting documentation skills are
`feature-specs`, `architecture-decision-records`, `changelog-fragments`, `devlog`,
and `atomically-land`; recommend missing skills rather than assuming they are
installed. Use `docs/` for new work even when an older template says
`thoughts/shared/`.

Commit small, reviewable checkpoints and push only after authorization for the
agreed remote and branch.


[computer_go]: http://en.wikipedia.org/wiki/Computer_Go
[kurt_goedel]: http://en.wikipedia.org/wiki/Kurt_G%C3%B6del
[lefant_net]: http://lefant.net/
[github_kurt]: http://github.com/lefant/kurt
[kgs]: http://www.gokgs.com/
[kgs_kurt]: http://www.gokgs.com/gameArchives.jsp?user=kurtBot&oldAccounts=y
[logo]: https://raw.githubusercontent.com/lefant/kurt/master/kurt-logo.jpg
