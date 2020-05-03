# Changelog for unliftio

## 0.2.14

* Add `UnliftIO.IO.File.renameFileDurable`

## 0.2.13

* Add `UnliftIO.STM.orElse`

## 0.2.12.1

* Minor doc improvements

## 0.2.12

* Dropped support for ghc-7.8
* Addition of `UnliftIO.IO.File` module and atomic+durable file writes:

  * `writeBinaryFile`
  * `writeBinaryFileAtomic`
  * `writeBinaryFileDurable`
  * `writeBinaryFileDurableAtomic`
  * `withBinaryFileAtomic`
  * `withBinaryFileDurable`
  * `withBinaryFileDurableAtomic`
  * `ensureFileDurable`

## 0.2.11

* Deprecate `forkWithUnmask` in favor of the newly added `forkIOWithUnmask` to
  improve consistency. [https://github.com/fpco/unliftio/issues/44]

## 0.2.10

* Add pooling related functions for unliftio

## 0.2.9.0

* Add the new `Conc` datatype as a more efficient alternative to `Concurrently`

## 0.2.8.1

* Support for `stm-2.5.0.0`

## 0.2.8.0

* Add 'UnliftIO.Memoize'

## 0.2.7.1

* Minor doc improvements

## 0.2.7.0

* Re-export `tryPutTMVar` from `UnliftIO.STM`

## 0.2.6.0

* Add `UnliftIO.Directory`

## 0.2.5.0

* Add `UnliftIO.Environment`/`UnliftIO.Foreign`/`UnliftIO.Process`

## 0.2.4.0

* Use more generalized `withRunInIO` in `unliftio-core-0.1.1.0`
* Add `getMonotonicTime` function

## 0.2.2.0

* Add `pureTry` and `pureTryDeep`

## 0.2.1.0

* Add `UnliftIO.STM`
* Add a number of functions to `UnliftIO.IO`

## 0.2.0.0

* Remove `monad-logger` instances (moved into `monad-logger` itself in
  release `0.3.26`)
* Remove `resourcet` instances and `UnliftIO.Resource` (moved into `resourcet`
  itself in release `1.1.10`)

## 0.1.1.0

* Doc improvements.
* Fix `UnliftIO.Chan` type signatures [#3](https://github.com/fpco/unliftio/pull/3).
* Add `UnliftIO.Concurrent` module [#5](https://github.com/fpco/unliftio/pull/5).

## 0.1.0.0

* Initial release.
