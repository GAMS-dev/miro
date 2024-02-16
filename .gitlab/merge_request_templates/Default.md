# Brief description of the changes
%{first_multiline_commit}

# Developer checklist
- [ ] Code
- [ ] Documentation
- [ ] Release notes
- [ ] Tests
- [ ] Configuration Mode (if applicable)
- [ ] Information security (if applicable)
    - [ ] User input is sanitized before being processed (have size limits/data type checks etc.)
    - [ ] Sensitive data protected via authentication/authorization
    - [ ] Communication with external systems (databases/network communications/file I/O etc.) is secure
    - [ ] Cryptography requirements of ISMS are met
    - [ ] Error messages do not expose sensitive information and no sensitive data ends up in log files
    - [ ] All relevant events are logged (but do not contain sensitive data)


# Reviewer checklist
- [ ] Code
- [ ] Documentation
- [ ] Release notes
- [ ] Tests
- [ ] Configuration Mode (if applicable)
- [ ] Information security (if applicable)
    - [ ] User input is sanitized before being processed (have size limits/data type checks etc.)
    - [ ] Sensitive data protected via authentication/authorization
    - [ ] Communication with external systems (databases/network communications/file I/O etc.) is secure
    - [ ] Cryptography requirements of ISMS are met
    - [ ] Error messages do not expose sensitive information and no sensitive data ends up in log files
    - [ ] All relevant events are logged (but do not contain sensitive data)
- [ ] Acceptance tests (are all requirements met, is the feature easy to be used and well documented, if applicable)
