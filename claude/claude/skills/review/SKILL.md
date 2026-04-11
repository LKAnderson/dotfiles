# PR Review Skill
1. Fetch the PR details using `gh pr view <number> --json  title,body,files,additions,deletions.
2. If not on the branch being reviewed, stash any uncommitted changes, then check out the branch
   being reviewed. Run the unit tests in each application or package that has changed files.
   Report any build errors and whether the tests pass or not.
3. Provide a structured review with sections:
    * ## Summary
    * ## Strengths
    * ## Issues (Critical/Minor)
    * ## Suggestions
4. Keep review focused on the actual diff - do not suggest unrelated refactors. When interfaces
   (functions, data structures) are changed, check that uses of the interfaces are compatible
   with the changes.
5. Be sure to return to the original branch and unstash any files that may have been stashed
   whild doing this review.
