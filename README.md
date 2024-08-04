<a id="x-28ULTRALISP-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ultralisp - A Quicklisp compatible Common Lisp software distribution.

<a id="ultralisp-asdf-system-details"></a>

## ULTRALISP ASDF System Details

* Version: 1.26.1
* Description: A fast-moving Common Lisp software distribution for those who want to publish his/her software today.
* Licence: `BSD`
* Author: Alexander Artemenko <svetlyak.40wt@gmail.com>
* Depends on: [3bmd][cc3e], [40ants-doc][2c00], [40ants-slynk][2e1d], [alexandria][8236], [anaphora][c9ae], [arrows][b590], [bordeaux-threads][3dbf], [chanl][24c3], [cl-base64][1d3a], [cl-cron][4ba4], [cl-dbi][6bc3], [cl-fad][1059], [cl-gearman][8936], [cl-info][4e81], [cl-interpol][f8a9], [cl-postgres][ce97], [cl-ppcre][49b9], [cl-store][c57f], [cl-yandex-metrika][acd1], [closer-mop][61a4], [dbd-postgres][0b29], [dbi][a5c3], [defmain][3266], [dexador][8347], [event-emitter][bb23], [f-underscore][a8bd], [fare-utils][c99c], [flexi-streams][5642], [function-cache][c2b5], [github][e6b5], [global-vars][07be], [group-by][774a], [ironclad][90b9], [jonathan][6dd8], [jsonrpc][a9bd], [jsown][0dcb], [kebab][5186], [lack-middleware-mount][7e24], [legit][6017], [link-header][851a], [local-time][46a1], [local-time-duration][6422], [log4cl][7f8b], [log4cl-extras][691c], [log4sly][d8da], [lparallel][e72d], [metatilities][4600], [mito][5b70], [named-readtables][d0a9], [openrpc-server][c8e7], [parenscript][7921], [prometheus][14fa], [prometheus.collectors.process][563a], [prometheus.collectors.sbcl][a01b], [prometheus.formats.text][b66b], [qlot][ae89], [quickdist][62b2], [quicklisp][9c78], [quri][2103], [reblocks][184b], [reblocks-auth][30ed], [reblocks-file-server][db1f], [reblocks-lass][28e0], [reblocks-navigation-widget][02e1], [reblocks-parenscript][c07c], [reblocks-ui][4376], [resend][b222], [routes][48e8], [rutils][9717], [secret-values][cd18], [serapeum][c41d], [slynk][b440], [spinneret][8175], [str][ef7f], [sxql][2efd], [trivial-timeout][afe6], [uuid][d6b3], [woo][c260], [yason][aba2], [zs3][8b5e]

[![](https://github-actions.40ants.com/ultralisp/ultralisp/matrix.svg?only=ci.run-tests)][1535]

![](http://quickdocs.org/badge/ultralisp.svg)

<a id="x-28ULTRALISP-DOCS-2FARCHITECTURE-3A-3A-40ARCHITECTURE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Architecture

<a id="components-of-the-system"></a>

### Components of the system

Ultralisp consists of many moving parts:

![](docs/images/architecture.png)

All components running in the Digital Ocean are running as Docker containers.

<a id="main-app"></a>

#### MainApp

MainApp implements most logic:

* It returns web pages of Ultralisp.org (the Reblocks framework is used).
* It runs a cron-like process for such regular tasks as:
* Checking of new code for all registered projects.
* Building new versions of the distribution.
* Updating a search index in ElasticSearch.

New code checks requires to load asd files and to check if each asd system is loadable. To make this process
more secure, MainApp sends request for a check to the Gearman server and Checker receives this request, then
performs a check and returns result back to the MainApp. During the check Checker has only a read-only access to the database.
After the each check, container with Checker dies to start the next check from a scratch.

Periods between project checks by cron are calculated with respect to the distance between recent commits in the project's repository.
Also a project owner may setup a web hook and the project will be updated as soon as possible after the restart.

<a id="checker"></a>

#### Checker

Checker app just listens to the Gearman server and waits for a command to check the project. In production this container dies after the each check. For convenience of development it is possible to make many checks in the same process.

For each project's source Ultralisp remebers the git commit used to build the most recent release. Checker fetches new commits and see if the last one is different from the commit hash stored in the database. If it is differ, then Checker searches for all asd files, loads them and returns to the MainApp a new commit hash and a list of `ASDF` systems.

After the successful check, MainApp creates a new revision of the project's check and a binds it to a new version of the distribution. Then MainApp builds a new distrubution by downloading a new source of the project, archiving it and uploading to the Amazon `S3` (for development a local storage is used instead).

<a id="gearman"></a>

#### Gearman

This component is an opensource `RPC` server, it works like a proxy between MainApp and Checker. This way we can scale checker and run a multiple instances to speed up the checking some day.

To run Gearman we are using [this][d92c] docker image from Artefactual Labs.

<a id="file-storage"></a>

#### FileStorage

Amazon `S3` is used as a storage for all project releases and dist metadata. CloudFlare proxies all requests to the https://dist.ultralisp.org/ into the Amazon `S3` bucket.

During development all releases and metadata are saved into the local filesystem and can MainApp can serve them as static files.


[d92c]: https://github.com/artefactual-labs/docker-gearmand
[1535]: https://github.com/ultralisp/ultralisp/actions
[cc3e]: https://quickdocs.org/3bmd
[2c00]: https://quickdocs.org/40ants-doc
[2e1d]: https://quickdocs.org/40ants-slynk
[8236]: https://quickdocs.org/alexandria
[c9ae]: https://quickdocs.org/anaphora
[b590]: https://quickdocs.org/arrows
[3dbf]: https://quickdocs.org/bordeaux-threads
[24c3]: https://quickdocs.org/chanl
[1d3a]: https://quickdocs.org/cl-base64
[4ba4]: https://quickdocs.org/cl-cron
[6bc3]: https://quickdocs.org/cl-dbi
[1059]: https://quickdocs.org/cl-fad
[8936]: https://quickdocs.org/cl-gearman
[4e81]: https://quickdocs.org/cl-info
[f8a9]: https://quickdocs.org/cl-interpol
[ce97]: https://quickdocs.org/cl-postgres
[49b9]: https://quickdocs.org/cl-ppcre
[c57f]: https://quickdocs.org/cl-store
[acd1]: https://quickdocs.org/cl-yandex-metrika
[61a4]: https://quickdocs.org/closer-mop
[0b29]: https://quickdocs.org/dbd-postgres
[a5c3]: https://quickdocs.org/dbi
[3266]: https://quickdocs.org/defmain
[8347]: https://quickdocs.org/dexador
[bb23]: https://quickdocs.org/event-emitter
[a8bd]: https://quickdocs.org/f-underscore
[c99c]: https://quickdocs.org/fare-utils
[5642]: https://quickdocs.org/flexi-streams
[c2b5]: https://quickdocs.org/function-cache
[e6b5]: https://quickdocs.org/github
[07be]: https://quickdocs.org/global-vars
[774a]: https://quickdocs.org/group-by
[90b9]: https://quickdocs.org/ironclad
[6dd8]: https://quickdocs.org/jonathan
[a9bd]: https://quickdocs.org/jsonrpc
[0dcb]: https://quickdocs.org/jsown
[5186]: https://quickdocs.org/kebab
[7e24]: https://quickdocs.org/lack-middleware-mount
[6017]: https://quickdocs.org/legit
[851a]: https://quickdocs.org/link-header
[46a1]: https://quickdocs.org/local-time
[6422]: https://quickdocs.org/local-time-duration
[7f8b]: https://quickdocs.org/log4cl
[691c]: https://quickdocs.org/log4cl-extras
[d8da]: https://quickdocs.org/log4sly
[e72d]: https://quickdocs.org/lparallel
[4600]: https://quickdocs.org/metatilities
[5b70]: https://quickdocs.org/mito
[d0a9]: https://quickdocs.org/named-readtables
[c8e7]: https://quickdocs.org/openrpc-server
[7921]: https://quickdocs.org/parenscript
[14fa]: https://quickdocs.org/prometheus
[563a]: https://quickdocs.org/prometheus.collectors.process
[a01b]: https://quickdocs.org/prometheus.collectors.sbcl
[b66b]: https://quickdocs.org/prometheus.formats.text
[ae89]: https://quickdocs.org/qlot
[62b2]: https://quickdocs.org/quickdist
[9c78]: https://quickdocs.org/quicklisp
[2103]: https://quickdocs.org/quri
[184b]: https://quickdocs.org/reblocks
[30ed]: https://quickdocs.org/reblocks-auth
[db1f]: https://quickdocs.org/reblocks-file-server
[28e0]: https://quickdocs.org/reblocks-lass
[02e1]: https://quickdocs.org/reblocks-navigation-widget
[c07c]: https://quickdocs.org/reblocks-parenscript
[4376]: https://quickdocs.org/reblocks-ui
[b222]: https://quickdocs.org/resend
[48e8]: https://quickdocs.org/routes
[9717]: https://quickdocs.org/rutils
[cd18]: https://quickdocs.org/secret-values
[c41d]: https://quickdocs.org/serapeum
[b440]: https://quickdocs.org/slynk
[8175]: https://quickdocs.org/spinneret
[ef7f]: https://quickdocs.org/str
[2efd]: https://quickdocs.org/sxql
[afe6]: https://quickdocs.org/trivial-timeout
[d6b3]: https://quickdocs.org/uuid
[c260]: https://quickdocs.org/woo
[aba2]: https://quickdocs.org/yason
[8b5e]: https://quickdocs.org/zs3

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
