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


## Utsushiyoが提供している主なAPI

### 関数
* **bootstrap**：.utsushiyoディレクトリの作成と，utsushiyo自体の設定ファイルの追加
```lisp
CL-USER> (utsushiyo:bootstrap)
```

* **init**：新規プロジェクトの設定ディレクトリの作成
  * 引数1（project-env-name / string）：設定・管理するシステムやアプリケーションの名称
  * キーワード引数1（project-root-path / pathstring）：もし，何かしらのCLプロジェクトディレクトリがすでにあるなら，そのプロジェクトのルートディレクトリの絶対パス
  * キーワード引数2（utsushiyo-file-directory : pathstring）：project-root-pathからのutsushiyo用設定ディレクトリがある場合，そのルートからの相対パス
  * キーワード引数3（project-config-dir / pathstring）：ホームディレクトリ以下の.utsushiyo以外の場所に設定ファイルを置きたい場合，そのディレクトリの絶対パス
```lisp
;; プロジェクトディレクトリがない場合
CL-USER> (utsushiyo:init "original-web-app")
``` 

```sh
~ $ ls -la ~/.utsushiyo/
total 0
drwxr-xr-x    4 tomabu  staff   136 12  4 16:37 .
drwxr-xr-x+ 114 tomabu  staff  3876 11 26 14:33 ..
drwxr-xr-x    2 tomabu  staff    68 12  4 16:37 original-web-app
drwxr-xr-x    6 tomabu  staff   204 12  4  2018 utsushiyo
```

```lisp
;; プロジェクトディレクトリがある（下記のようなUtsushiyo用ディレクトリがある）場合
;; /path/to/project/root/.../utsushiyo/というディレクトリがある場合
CL-USER> (utsushiyo:init "original-web-app" :project-root-path "/path/to/project/root/" 
                         :utsushiyo-file-directory ".../utsushiyo/")
``` 

* **get-attribute**：環境名に対応した設定ファイルの中身を文字列として取り出す
  * 引数1（project / string）：対象のソフトウェア・システム・環境の名称
  * 引数2（attribute-name）：設定名（対象ファイル名と同じにする）
```lisp
;; $HOME/.utsushiyo/original-web-app/config/portファイルから設定を取り出す場合
CL-USER> (utsushiyo:get-attribute "original-web-app" "config/port")
"5000"

;; $HOME/.utsushiyo/original-web-app/helpファイルから設定を取り出す場合
CL-USER> (utsushiyo:get-attribute "original-web-app" "help")
"......help text..."
```


## Supported options (as roswell script)

developping now...

## Author

* Tomoki ABURATANI (aburatanitomoki@gmail.com)

## Copyright

Copyright (c) 2018 Tomoki ABURATANI (aburatanitomoki@gmail.com)

## License

Licensed under the MIT License.
