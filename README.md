# Utsushiyo - Common Lisp Local Environment Attributes Manager -

[![Build Status](https://travis-ci.org/dbym4820/utsushiyo.svg?branch=master)](https://travis-ci.org/dbym4820/utsushiyo)
[![Quicklisp](http://quickdocs.org/badge/utsushiyo.svg)](http://quickdocs.org/utsushiyo/)


## Usage

Management Application Configuration via Local File, for instance, generating help messege, or setting server port number by local file.

Set/Get environment parameter via Common Lisp.

```
CL-USER> (utsushiyo:init "any-project-name" :project-root-path "path/to/project-root/")
CL-USER> (utsushiyo:set-attribute "any-project-name" "port" "5000")
"5000"

;; => cat ~/.utsushiyo/any-project/port ;=> 5000
CL-USER> (utsushiyo:get-attribute "any-project" "port")
"5000"

=================================================================================

```

## Installation

* Via roswell

```
$ ros install dbym4820/utsushiyo
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
