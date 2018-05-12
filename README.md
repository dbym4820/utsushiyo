# Utsushiyo - Common Lisp Local Configuration Management Framework -

[![Build Status](https://travis-ci.org/dbym4820/utsushiyo.svg?branch=master)](https://travis-ci.org/dbym4820/utsushiyo)

## Usage

Management Application Configuration through Local File about Configuration like below.

```
# When you want to create a project named "blah" and define configuration in Own home directory

$ utsushiyo -c blah example
$ ls ~/.blah/

$ cat ~/.blah/helps/example
Example-command: pupose of this command

Usage: how to use this command (e.g. $ project-name example-command)

explanation about this command

```

You have to load the project which you wanna create configs by quicklisp (or roswell) in your local environment before you use utsushiyo.

## Installation

* As roswell script

```
$ ros install dbym4820/utsushiyo
...
$ utsushiyo -g your-project-name example
Example-command: pupose of this command

Usage: how to use this command (e.g. $ project-name example-command)

explanation about this command

```

* With quicklisp

```
CL-USER> (ql:quickload :utsushiyo)
To load "utsushiyo":
  Load 1 ASDF system:
    utsushiyo
; Loading "utsushiyo"
[package utsushiyo].
(:UTSUSHIYO)
CL-USER> (utsushiyo:project-config-bootstrap "your-project")

; No value
(utsushiyo:get-help "example" "utsushiyo")
"Example-command: pupose of this command

Usage: how to use this command (e.g. $ project-name example-command)

explanation about this command
"
```

## Requirement

You need to make your project composed as same as below

-- yout-common-lisp-project-dir/
├ README.md
├ your-common-lisp.asd
├ src/
|	├ your-common-lisp-project.lisp
|	├ **helps/**
|		├ **help-sentence**
├ t/

## Author

* Tomoki ABURATANI (aburatanitomoki@gmail.com)

## Copyright

Copyright (c) 2018 Tomoki ABURATANI (aburatanitomoki@gmail.com)

## License

Licensed under the MIT License.
