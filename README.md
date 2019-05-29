# Utsushiyo - Common Lisp Local Environment Attributes Manager -

<img src="./img/icon.png" width="300px" />

[![Build Status](https://travis-ci.org/dbym4820/utsushiyo.svg?branch=master)](https://travis-ci.org/dbym4820/utsushiyo)
[![Quicklisp](http://quickdocs.org/badge/utsushiyo.svg)](http://quickdocs.org/utsushiyo/)


## Usage

Management Application Configuration via Local File, for instance, generating help messege, or setting server port number by local file.

This system intend interaction between system and user.


```
# When you want to create a project named "blah" and define configuration in Own home directory

# utsushiyo [set-attribute | --set-attribute | -s] 
$ utsushiyo -s sample-cl-project sample-attribute "it is sample attribute value"
$ cat ~/.utsushiyo/sample-cl-project/sample-attribute
it is sample attribute value
```

You have to load the project which you wanna create configs by quicklisp (or roswell) in your local environment before you use utsushiyo.

## Other Usage

Set/Get environment parameter via Common Lisp.

```
CL-USER> (utsushiyo:set-attribute "clkedit-server(project-name)" "config/port" "5000")
"5000"

;; => cat ~/.utsushiyo/clkedit-server/config/port ;=> 5000
CL-USER> (utsushiyo:get-attribute "clkedit-server" "config/port")
"5000"

=================================================================================

```

## Installation

* Via roswell

```
$ ros install dbym4820/utsushiyo
```

* Via quicklisp

```
# download source code from github
$ git clone https://github.com/dbym4820/utsushiyo.git

# make path to quicklisp local-project directory
$ ln -nfs /path/to/utsushiyo/directory /path/to/quicklisp/local-projects/
```

```
CL-USER> (ql:quickload :utsushiyo)
To load "utsushiyo":
  Load 1 ASDF system:
    utsushiyo
; Loading "utsushiyo"
[package utsushiyo].
(:UTSUSHIYO)
```

## Requirement

You need to make your project composed as same as below

```
-- yout-common-lisp-project-dir/
├ README.md
├ your-common-lisp.asd
├ src/
|  ├ your-common-lisp-project.lisp
|  ├...  
|  ├utsushiyo/
|	  |- attribute => attribute-value
├ t/
```

## Supported options (as roswell script)

developping now...

## Author

* Tomoki ABURATANI (aburatanitomoki@gmail.com)

## Copyright

Copyright (c) 2018 Tomoki ABURATANI (aburatanitomoki@gmail.com)

## License

Licensed under the MIT License.
