Saven: Scheme dependency manager
================================

`saven` is a Scheme dependency manager and deliverable builder. This tool
resolved the project dependencies, make deliverable, and runs tests.

How to install
==============

You can install `saven` with the following command:
```
curl https://raw.githubusercontent.com/ktakashi/saven/master/install.sh | /bin/sh
```

Hot to use
==========

`saven` requires specific directory structure and configuration file. See
`test-module`, which contains example module.

If you have a proper `saven` module, then you can use `sav` command. The
`sav` command takes multiple targets. The followings are the targets:

- `build`
- `test`
- `package`
- `clean`

Directory structure
-------------------

TBD

Saven file
----------

TBD

Misc
====

The name `saven` comes from Dutchised English word *saven* means, *save*. 
The tool itself is highly inspired by `maven` so it also means Scheme + maven.
