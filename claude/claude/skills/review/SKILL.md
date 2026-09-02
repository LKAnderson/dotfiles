---
name: review
description: >-
  Deep adversarial review of a branch or PR in any project: checks out the
  branch, runs the affected unit tests, reports build and test status, and
  returns a Summary / Strengths / Issues / Suggestions writeup. Use when asked
  for a thorough or adversarial review, or when test and build results are
  wanted alongside the review. NOT the platform-monorepo pre-review first pass —
  for that, use the pr-first-pass skill instead.
---

# Deep Review Skill

Adversarial, test-running review. For a lightweight **pre-review first pass** on
platform-monorepo changes — which follows the team's `docs/pr-first-pass.md`
prompt, caps findings, and never posts without a go-ahead — use the
`pr-first-pass` skill instead.

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

   | Bucket | Signature | What to do |
   | --- | --- | --- |
   | **Mechanical** | ≥80% of files moved/deleted/generated, little substantive change | Verify, don't read (checks below) |
   | **Leaf** | ≲400 substantive lines, single purpose | Full adversarial read |
   | **Structural** | New abstraction or data flow, or ≳400 substantive added lines | Deep read; say so if this needed a design conversation instead of async review |

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

   | PR | Substantive | Moved | Deleted | Generated | Bucket |
   | --- | --- | --- | --- | --- | --- |

   For a stack, one row per PR ordered base-first, plus a recommended review
   order (base-first, merge each before moving up). Name the file or subsystem
   holding the substantive weight — that's where the read starts. Flag any PR
   whose base is not the previous PR in the stack: that's a parallel change
   being presented as a stack, and it can't be reviewed incrementally.

   If the top bucket is **Structural**, stop after the table and ask whether I
   want the deep review now or a design conversation with the author first.
4. If not on the branch being reviewed, stash any uncommitted changes, then check out the branch
   being reviewed. Run the unit tests in each application or package that has changed files.
   Report any build errors and whether the tests pass or not.
5. If a Github PR is being reviewed, and the PR is part of a set of Stacked PRs, gather context
   from the previous PRs in the stack, as needed to assist in understanding this PR.
6. When reviewing the changes, assume an adversarial reviewer perspsective, where the approach
   is influenced by a need to find non-compliance with coding guideline, security issues and
   hard-to-see bugs.
7. Provide a structured review with sections:
   - ## Summary
   - ## Strengths
   - ## Issues (Critical/Minor)
   - ## Suggestions
8. Keep review focused on the actual diff - do not suggest unrelated refactors. When interfaces
   (functions, data structures) are changed, check that uses of the interfaces are compatible
   with the changes.
9. Be sure to return to the original branch and unstash any files that may have been stashed
   while doing this review.
