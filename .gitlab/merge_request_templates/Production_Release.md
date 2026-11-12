# Brief description of the changes

*Release Notes here*

# Developer checklist
- [ ] Code
- [ ] New features documented
- [ ] Release notes complete
- [ ] Tests
- [ ] Information security (if applicable)
    - [ ] User input is sanitized before being processed (have size limits/data type checks etc.)
    - [ ] Sensitive data protected via authentication/authorization
    - [ ] Communication with external systems (databases/network communications/file I/O etc.) is secure
    - [ ] Cryptography requirements of ISMS are met
    - [ ] Error messages do not expose sensitive information and no sensitive data ends up in log files
    - [ ] All relevant events are logged (but do not contain sensitive data)


# Reviewer checklist
- [ ] Code
- [ ] New features documented
- [ ] Release notes complete
- [ ] Tests
- [ ] Information security (if applicable)
    - [ ] User input is sanitized before being processed (have size limits/data type checks etc.)
    - [ ] Sensitive data protected via authentication/authorization
    - [ ] Communication with external systems (databases/network communications/file I/O etc.) is secure
    - [ ] Cryptography requirements of ISMS are met
    - [ ] Error messages do not expose sensitive information and no sensitive data ends up in log files
    - [ ] All relevant events are logged (but do not contain sensitive data)
- [ ] Acceptance tests (are all requirements met, is the feature easy to be used and well documented, if applicable)


# Manual test checklist

## MIRO Desktop

- [ ] Preferences can be opened, changed, saved, and restored after restart
- [ ] Engine login via Google account and via token/API key works
- [ ] "What's new" dialog is displayed correctly and contains expected release information

## MIRO Server

- [ ] Default scenario permissions work as expected
- [ ] Access groups work as expected
