# PR Review Skill

1. Identify what is being reviewed based on optional prompt content provided with the skill
   invocation. If it is not clear what is to be reviewed, check if a github PR exists that
   matches the prompt. If it is a github PR, fetch the PR details using
   `gh pr view <number> --json title,body,files,additions,deletions. If you're unable to
   identify what to review, notify the user and wait for clarification.
2. If the PR is already merged or closed, please notify me and ask me whether
   I want to proceed.
3. If not on the branch being reviewed, stash any uncommitted changes, then check out the branch
   being reviewed. Run the unit tests in each application or package that has changed files.
   Report any build errors and whether the tests pass or not.
4. When reviewing the changes, assume an adversarial reviewer perspsective, where the approach
   is influenced by a need to find non-compliance with coding guideline, security issues and
   hard-to-see bugs.
5. Provide a structured review with sections:
   - ## Summary
   - ## Strengths
   - ## Issues (Critical/Minor)
   - ## Suggestions
6. Keep review focused on the actual diff - do not suggest unrelated refactors. When interfaces
   (functions, data structures) are changed, check that uses of the interfaces are compatible
   with the changes.
7. Be sure to return to the original branch and unstash any files that may have been stashed
   while doing this review.
