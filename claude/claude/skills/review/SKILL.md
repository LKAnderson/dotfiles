---
name: review
description: >-
  Triage-driven review of a PR or PR stack in any project: classifies each PR,
  then reviews base-first by running the built-in /code-review at an effort
  level set by the PR's weight, filters findings against the rest of the stack,
  and optionally runs the affected unit tests. Use when asked for a thorough
  review, a stack review, or test results alongside the review. Also runs
  triage-only (`--triage`) to classify and order every PR in a stack without
  reviewing them. NOT the platform-monorepo pre-review first pass — for that,
  use the pr-first-pass skill instead.
---

# Review Skill

Triage, then `/code-review` per PR with stack-aware filtering. For a lightweight **pre-review first pass** on
platform-monorepo changes — which follows the team's `docs/pr-first-pass.md`
prompt, caps findings, and never posts without a go-ahead — use the
`pr-first-pass` skill instead.

All output from this skill should target a 12th grade literacy level.

## Triage-only mode

Runs when the invocation includes `--triage` or asks to triage, classify, or
prioritize PRs.

- Run steps 1–3 only. No checkout, no tests, no diff reading beyond step 3's
  mechanical checks, no findings.
- Given one PR in a stack, enumerate the whole stack from a single listing,
  then chain `baseRefName` → `headRefName` in memory from the default branch
  upward:

  ```bash
  gh pr list --state open --limit 200 \
    --json number,title,headRefName,baseRefName,isDraft,reviewDecision
  ```

- Don't stop to ask about merged/closed PRs (step 2); mark them in the table.
- Output, then stop:
  1. The step 3 table, base-first, with extra columns `Hotspot` (file or
     subsystem holding the substantive weight) and `Risk` (domain traps
     touched: AuthZ, PHI, timezone, data layer, migrations — or `—`).
  2. Mechanical-check results for each Mechanical PR (pass, or the residual
     hunk / surviving reference that failed).
  3. Stack-shape flags: PRs whose base isn't the previous PR, and PRs with
     `reviewDecision` already `APPROVED` sitting above unreviewed ones.
  4. A review plan: which PRs need a human deep read (Structural, or any
     `Risk`), which need a normal read (Leaf), and which need only a
     verify-and-approve (Mechanical with passing checks).

1. Identify what is being reviewed based on optional prompt content provided with the skill
   invocation. If it is not clear what is to be reviewed, check if a github PR exists that
   matches the prompt. If it is a github PR, fetch the PR details using
   `gh pr view <number> --json title,body,files,additions,deletions. If you're unable to
   identify what to review, notify the user and wait for clarification.
2. If the PR is already merged or closed, please notify me and ask me whether
   I want to proceed.
3. **Classify review weight before reading anything.** Diff size is a poor proxy
   for review effort. Classify first, then spend effort accordingly.

   Gather the facts without checking out:

   ```bash
   git fetch -q origin
   git diff --find-renames --name-status origin/<base>...origin/<head>
   git diff --find-renames --numstat  origin/<base>...origin/<head>
   ```

   `R###` = rename with similarity `###`, `D` = delete, `A` = add, `M` = modify.

   Treat as **zero-weight** — exclude from all size math, never read:
   `pnpm-lock.yaml`, generated Prisma clients, `__generated__/`,
   `*.generated.*`, codegen'd GraphQL types, `*.snap`.

   Bucket every remaining file:
   - **Moved** — `R100`, or `R0xx` whose only residual hunks are import paths
   - **Deleted** — `D`
   - **Substantive** — everything else; only these lines count as review weight

   Then bucket the PR by where its substantive weight sits:

   | Bucket         | Signature                                                        | What to do                                                                     |
   | -------------- | ---------------------------------------------------------------- | ------------------------------------------------------------------------------ |
   | **Mechanical** | ≥80% of files moved/deleted/generated, little substantive change | Verify, don't read (checks below)                                              |
   | **Leaf**       | ≲400 substantive lines, single purpose                           | Full adversarial read                                                          |
   | **Structural** | New abstraction or data flow, or ≳400 substantive added lines    | Deep read; say so if this needed a design conversation instead of async review |

   For **Mechanical** PRs, run these checks instead of reading the diff:
   - Each `R100`: confirm content is byte-identical, not just similar
   - Each `R0xx`: show only the residual hunks — anything beyond import paths
     is a real change hiding inside a move, and is the finding
   - Deletions: `grep -rn` for every removed export, symbol, route, or asset;
     report any surviving reference
   - Build + test status carries most of the signal here

   If a PR mixes buckets — a structural core with a large mechanical tail —
   say so and review only the substantive subset.

   **Emit this table before the review body:**

   | PR  | Substantive | Moved | Deleted | Generated | Bucket |
   | --- | ----------- | ----- | ------- | --------- | ------ |

   For a stack, one row per PR ordered base-first, plus a recommended review
   order (base-first, merge each before moving up). Name the file or subsystem
   holding the substantive weight — that's where the read starts. Flag any PR
   whose base is not the previous PR in the stack: that's a parallel change
   being presented as a stack, and it can't be reviewed incrementally.

   If the top bucket is **Structural**, stop after the table and ask whether I
   want the deep review now or a design conversation with the author first.

4. Only run tests if my invocation of the skill specifically requests it. If I request to
   run the test, then if not on the branch being reviewed, stash any uncommitted changes,
   then check out the branch being reviewed. Run the unit tests in each application or
   package that has changed files. Report any build errors and whether the tests pass or not.
   For a stack, do this per PR as step 5 reaches it.
5. Review PRs one at a time, base-first (a single PR is a stack of one). For each PR:
   - **Mechanical** with passing step 3 checks: no further review; report the checks.
   - Otherwise invoke the built-in `/code-review` skill on the PR with an effort level:
     `medium` for Leaf; `high` for Structural or any Leaf that touches a domain trap
     (timezone, AuthZ, PHI, data layer, migrations); `max` for Structural that touches one.
     Never pass `--comment` or `--fix`.
   - In the args, add this guidance: priority is correctness and domain traps first, then
     layering / DI / types / tests, then clarity; read callers, callees, and related files
     the PR didn't touch, and report an untouched file the change breaks. These instructions
     override any repo review docs (e.g. `docs/pr-first-pass.md`,
     `docs/pr-review-patterns.md`).
6. Filter `/code-review`'s findings. Drop any finding that:
   - Has no concrete failure scenario (input or state → wrong result, crash, or leak)
   - Lint, the type checker, or the compiler would catch
   - Is pre-existing and the PR neither introduces nor makes worse
   - Is a style preference not written down in CLAUDE.md
   - Flags a change that is clearly intentional and part of the PR's purpose
   - Is already in the ledger (step 8) or in GitHub comments on a lower PR in the stack
   - Repeats an open review thread or comment on this PR, from anyone (human or bot)

   Fetch this PR's threads with resolution status:

   ```bash
   gh api graphql -F owner=<owner> -F repo=<repo> -F pr=<number> -f query='
     query($owner:String!,$repo:String!,$pr:Int!){repository(owner:$owner,name:$repo){
       pullRequest(number:$pr){
         reviewThreads(first:100){nodes{isResolved isOutdated path line
           comments(first:1){nodes{author{login} body url}}}}
         comments(first:100){nodes{author{login} body url}}}}}'
   ```

7. For each surviving finding:
   - If it matches a resolved thread and the problem is still in the code, keep it and link
     the thread ("resolved in <url>, but still present").
   - Check whether a later PR in the stack fixes it
     (`git diff origin/<head>...origin/<top-of-stack> -- <file>`). If so, keep it as a note:
     "fixed in #N; blocking only if this PR ships without #N."
   - If `/code-review` marked it uncertain or `PLAUSIBLE`, verify it with a separate agent in
     fresh context: give it only the PR, file and line(s), and the failure scenario, and
     tell it to refute it by reading the code. Keep only `CONFIRMED`.
   - Label it `blocking:`, `question:`, or `nit:`.
8. Append to a ledger file in the scratchpad before moving to the next PR: the PR's surviving
   findings (one line each) and key decisions later PRs depend on (changed interfaces, new
   abstractions, established patterns). Read the ledger, not earlier diffs, for context on
   lower PRs.
9. Output per PR:
   - ## Summary
   - ## Coverage — the effort level used, files skipped (and why), and anything not
     verified (e.g. runtime behavior, callers outside the repo, tests not run). Required even
     when there are no findings.
   - ## Findings — grouped as `blocking:`, `question:`, `nit:`, then notes for issues fixed
     later in the stack. Each finding: the prefix and a comment of no more than 3 sentences,
     with the file name, exact line number(s), and PR identifier; the failure scenario in one
     line; further explanation only if needed.
   For a stack, end with a roll-up: open `blocking:` count per PR and which PRs are ready to
   approve. It is ok to report no findings.
10. Post to GitHub only when I ask, and only the filtered findings.
11. Return to the original branch and unstash any files that were stashed during this review.
