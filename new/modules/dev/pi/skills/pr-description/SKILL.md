---
name: pr-description
description: create a pr description
---

Create a PR description from the diff agains the main origin branch (main|master|etc)

- Keep it minimal and easy to read for humans
- No LLM talk
- Dont get too much into the details about files
- Do not mention what has been tested
- Make the message contain semantic meaning on not just a description of the changed lines of code
- Do not mention updating of changelog
- do not add co authored by 

DONTS EXAMPLES:

```
# ❌ SELF EXPLANATORY TEST COVERAGE COMMENT
Added comprehensive test coverage for custom schema directory functionality

# ❌ SELF EXPLANATORY DOCUMENTATION COMMENT
- Documentation for bundling from custom schema directories
```
