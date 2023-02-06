# ChangeLog for unliftio-core

## 0.2.1.0

* Added `Control.Monad.IO.Unlift.liftIOOp`

## 0.2.0.2

* Widen `base` upperbound to `< 4.17` to support ghc-9.2.

## 0.2.0.1

* Remove faulty default implementation of `withRunInIO` [#56](https://github.com/fpco/unliftio/issues/56)

## 0.2.0.0

* Move `askUnliftIO` out of class [#55](https://github.com/fpco/unliftio/issues/55)

## 0.1.2.0

* Add `wrappedWithRunInIO`.

## 0.1.1.0

* Doc improvements.
* Inline functions in `Control.Monad.IO.Unlift` module [#4](https://github.com/fpco/unliftio/pull/4).
* Fully polymorphic `withRunInIO` [#12](https://github.com/fpco/unliftio/pull/12).
* Move `withRunInIO` into the `MonadUnliftIO` typeclass itself[#13](https://github.com/fpco/unliftio/issues/13)

## 0.1.0.0

* Initial release.
