---
name: changelog-release
description: Create changelog
---

Create a release commit

1) Look at the diff against the origin branch
   - If on a feature branch already check against main/master
   - Otherwise compare against origin of main/master
2) Update the CHANGELOG with these RULES:
   - Keep it minimal and easy to read for humans
   - No LLM talk
   - Dont get too much into the details about files
   - Do not mention what has been tested
   - Make the message contain semantic meaning on not just a description of the changed lines of code
   - Do not mention updating of changelog
3) Create a commit with the commit message
   - Stage the changelog
   - Only use staged files
   - Do NOT ADD Co-Authored-By:
   - Do NOT override the author
4) Create a feature branch in git flow convention
5) Create a PR with description and title from the changelog 
   - Use same rules as for the CHANGELOG
   - Do not include a section with the test plan

Rules:
   - When making a new commit just include the changelog with that commit otherwise create a commit just for the changelog
   - Do not mention updating of changelog
   - Do not mention any adding of tests or documentation this is self-explanatory

DONTS EXAMPLES:

```
# ❌ SELF EXPLANATORY TEST COVERAGE COMMENT
Added comprehensive test coverage for custom schema directory functionality

# ❌ SELF EXPLANATORY DOCUMENTATION COMMENT
- Documentation for bundling from custom schema directories
```
