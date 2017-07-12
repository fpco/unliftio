# unliftio

Provides the core `MonadUnliftIO` typeclass, a number of common
instances, and a collection of common functions working with it.  Not
sure what the `MonadUnliftIO` typeclass is all about? Read on!

## Quickstart

* Replace imports like `Control.Exception` with
  `UnliftIO.Exception`. Yay, your `catch` and `finally` are more
  powerful and safer!
* Similar with `Control.Concurrent.Async` with `UnliftIO.Async`
* Or go all in and import `UnliftIO`
* Naming conflicts: let `unliftio` win
* Drop the deps on `monad-control`, `lifted-base`, and `exceptions`
* Compilation failures? You may have just avoided subtle runtime bugs

Sound like magic? It's not. Keep reading!

## Unlifting in 2 minutes

Let's say I have a function:

```haskell
readFile :: FilePath -> IO ByteString
```

But I'm writing code inside a function that uses `ReaderT Env IO`, not
just plain `IO`. How can I call my `readFile` function in that
context? One way is to manually unwrap the `ReaderT` data constructor:

```haskell
myReadFile :: FilePath -> ReaderT Env IO ByteString
myReadFile fp = ReaderT $ \_env -> readFile fp
```

But having to do this regularly is tedious, and ties our code to a
specific monad transformer stack. Instead, many of us would use
`MonadIO`:

```haskell
myReadFile :: MonadIO m => FilePath -> m ByteString
myReadFile = liftIO . readFile
```

But now let's play with a different function:

```haskell
withBinaryFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
```

We want a function with signature:

```haskell
myWithBinaryFile
    :: FilePath
    -> IOMode
    -> (Handle -> ReaderT Env IO a)
    -> ReaderT Env IO a
```

If I squint hard enough, I can accomplish this directly with the
`ReaderT` constructor via:

```haskell
myWithBinaryFile fp mode inner =
  ReaderT $ \env -> withBinaryFile
    fp
    mode
    (\h -> runReaderT (inner h) env)
```

I dare you to try to and accomplish this with `MonadIO` and
`liftIO`. It simply can't be done. (If you're looking for the
technical reason, it's because `IO` appears in
[negative/argument position](https://www.fpcomplete.com/blog/2016/11/covariance-contravariance)
in `withBinaryFile`.)

However, with `MonadUnliftIO`, this is possible:

```haskell
import Control.Monad.IO.Unlift

myWithBinaryFile
    :: MonadUnliftIO m
    => FilePath
    -> IOMode
    -> (Handle -> m a)
    -> m a
myWithBinaryFile fp mode inner =
  withRunInIO $ \runInIO ->
  withBinaryFile
    fp
    mode
    (\h -> runInIO (inner h))
```

That's it, you now know the entire basis of this library.

## How common is this problem?

This pops up in a number of places. Some examples:

* Proper exception handling, with functions like `bracket`, `catch`,
  and `finally`
* Working with `MVar`s via `modifyMVar` and similar
* Using the `timeout` function
* Installing callback handlers (e.g., do you want to do
  [logging](https://www.stackage.org/package/monad-logger) in a signal
  handler?).

This also pops up when working with libraries which are monomorphic on
`IO`, even if they could be written more extensibly.

## Examples

Reading through the codebase here is likely the best example to see
how to use `MonadUnliftIO` in practice. And for many cases, you can
simply add the `MonadUnliftIO` constraint and then use the
pre-unlifted versions of functions (like
`UnliftIO.Exception.catch`). But ultimately, you'll probably want to
use the typeclass directly. Here are some simple examples. First: ome
typeclass instances:

```haskell
instance MonadUnliftIO IO where
  askUnliftIO = return (UnliftIO id)
instance MonadUnliftIO m => MonadUnliftIO (IdentityT m) where
  askUnliftIO = IdentityT $
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . runIdentityT))
instance MonadUnliftIO m => MonadUnliftIO (ReaderT r m) where
  askUnliftIO = ReaderT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runReaderT r))
```

Note that:

* The `IO` instance does not actually do any lifting or unlifting, and
  therefore it can use `id`
* `IdentityT` is essentially just wrapping/unwrapping its data
  constructor, and then recursively calling `withUnliftIO` on the
  underlying monad.
* `ReaderT` is just like `IdentityT`, but it captures the reader
  environment when starting.

Second, using `withRunInIO` to unlift a function:

```haskell
timeout :: MonadUnliftIO m => Int -> m a -> m (Maybe a)
timeout x y = withRunInIO $ \run -> System.Timeout.timeout x $ run y
```

This is a common pattern: use `withRunInIO` to capture a run function,
and then call the original function with the user-supplied arguments,
applying `run` as necessary.

Thirdly, using `askUnliftIO` directly when multiple types are needed:

```haskell
race :: MonadUnliftIO m => m a -> m b -> m (Either a b)
race a b = withUnliftIO $ \u -> A.race (unliftIO u a) (unliftIO u b)
```

This works just like `withRunIO`, except we use `unliftIO u` instead
of `run`, which is polymorphic. You _could_ get away with multiple
`withRunInIO` calls here instead, but this approach is idiomatic and
may be more performant (depending on optimizations).

And finally, a much more complex usage, when unlifting the `mask`
function. This function needs to unlift vaues to be passed into the
`restore` function, and then `liftIO` the result of the `restore`
function.

```haskell
mask :: MonadUnliftIO m => ((forall a. m a -> m a) -> m b) -> m b
mask f = withUnliftIO $ \u -> Control.Exception.mask $ \unmask ->
  unliftIO u $ f $ liftIO . unmask . unliftIO u
```

## Limitations

Not all monads which can be an instance of `MonadIO` can be instances
of `MonadUnliftIO`, due to the `MonadUnliftIO` laws (described in the
Haddocks for the typeclass). This prevents instances for a number of
classes of transformers:

* Transformers using continuations (e.g., `ContT`, `ConduitM`, `Pipe`)
* Transformers with some monadic state (e.g., `StateT`, `WriterT`)
* Transformers with multiple exit points (e.g., `ExceptT` and its ilk)

In fact, there are two specific classes of transformers that this
approach does work for:

* Transformers with no context at all (e.g., `IdentityT`, `NoLoggingT`)
* Transformers with a context but no state (e.g., `ReaderT`, `LoggingT`)

This may sound restrictive, but this restriction is fully
intentional. Trying to unlift actions in stateful monads leads to
unpredictable behavior. For a long and exhaustive example of this, see
[A Tale of Two Brackets](https://www.fpcomplete.com/blog/2017/06/tale-of-two-brackets),
which was a large motivation for writing this library.

## Comparison to other approaches

You may be thinking "Haven't I seen a way to do `catch` in `StateT`?"
You almost certainly have. Let's compare this approach with
alternatives. (For an older but more thorough rundown of the options,
see
[Exceptions and monad transformers](http://www.yesodweb.com/blog/2014/06/exceptions-transformers).)

There are really two approaches to this problem:

* Use a set of typeclasses for the specific functionality we care
  about. This is the approach taken by the `exceptions` package with
  `MonadThrow`, `MonadCatch`, and `MonadMask`. (Earlier approaches
  include `MonadCatchIO-mtl` and `MonadCatchIO-transformers`.)
* Define a generic typeclass that allows any control structure to be
  unlifted. This is the approach taken by the `monad-control`
  package. (Earlier approaches include `monad-peel` and `neither`.)

The first style gives extra functionality in allowing instances that
have nothing to do with runtime exceptions (e.g., a `MonadCatch`
instance for `Either`). This is arguably a good thing. The second
style gives extra functionality in allowing more operations to be
unlifted (like threading primitives, not supported by the `exceptions`
package).

Another distinction within the generic typeclass family is whether we
unlift to just `IO`, or to arbitrary base monads. For those familiar,
this is the distinction between the `MonadIO` and `MonadBase`
typeclasses.

This package's main objection to all of the above approaches is that
they work for too many monads, and provide difficult-to-predict
behavior for a number of them (arguably: plain wrong behavior). For
example, in `lifted-base` (built on top of `monad-control`), the
`finally` operation will discard mutated state coming from the cleanup
action, which is usually not what people expect. `exceptions` has
_different_ behavior here, which is arguably better. But we're arguing
here that we should disallow all such ambiguity at the type level.

So comparing to other approaches:

### monad-unlift

Throwing this one out there now: the `monad-unlift` library is built
on top of `monad-control`, and uses fairly sophisticated type level
features to restrict it to only the safe subset of monads. The same
approach is taken by `Control.Concurrent.Async.Lifted.Safe` in the
`lifted-async` package. Two problems with this:

* The complicated type level functionality can confuse GHC in some
  cases, making it difficult to get code to compile.
* We don't have an ecosystem of functions like `lifted-base` built on
  top of it, making it likely people will revert to the less safe
  cousin functions.

### monad-control

The main contention until now is that unlifting in a transformer like
`StateT` is unsafe. This is not universally true: if only one action
is being unlifted, no ambiguity exists. So, for example, `try :: IO a
-> IO (Either e a)` can safely be unlifted in `StateT`, while `finally
:: IO a -> IO b -> IO a` cannot.

`monad-control` allows us to unlift both styles. In theory, we could
write a variant of `lifted-base` that never does state discards, and
let `try` be more general than `finally`. In other words, this is an
advantage of `monad-control` over `MonadUnliftIO`. We've avoided
providing any such extra typeclass in this package though, for two
reasons:

* `MonadUnliftIO` is a simple typeclass, easy to explain. We don't
  want to complicated matters (`MonadBaseControl` is a notoriously
  difficult to understand typeclass)
* Having this kind of split would be confusing in user code, when
  suddenly `finally` is not available to us. We would rather encourage
  [good practices](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern)
  from the beginning.

Another distinction is that `monad-control` uses the `MonadBase`
style, allowing unlifting to arbitrary base monads. In this package,
we've elected to go with `MonadIO` style. This limits what we can do
(e.g., no unlifting to `STM`), but we went this way because:

* In practice, we've found that the vast majority of cases are dealing
  with `IO`
* The split in the ecosystem between constraints like `MonadBase IO`
  and `MonadIO` leads to significant confusion, and `MonadIO` is by
  far the more common constraints (with the typeclass existing in
  `base`)

### exceptions

One thing we lose by leaving the `exceptions` approach is the ability
to model both pure and side-effecting (via `IO`) monads with a single
paradigm. For example, it can be pretty convenient to have
`MonadThrow` constraints for parsing functions, which will either
return an `Either` value or throw a runtime exception. That said,
there are detractors of that approach:

* You lose type information about which exception was thrown
* There is ambiguity about _how_ the exception was returned in a
  constraint like `(MonadIO m, MonadThrow m`)

The latter could be addressed by defining a law such as `throwM =
liftIO . throwIO`. However, we've decided in this library to go the
route of encouraging `Either` return values for pure functions, and
using runtime exceptions in `IO` otherwise. (You're of course free to
also return `IO (Either e a)`.)

By losing `MonadCatch`, we lose the ability to define a generic way to
catch exceptions in continuation based monads (such as
`ConduitM`). Our argument here is that those monads can freely provide
their own catching functions. And in practice, long before the
`MonadCatch` typeclass existed, `conduit` provided a `catchC`
function.

In exchange for the `MonadThrow` typeclass, we provide helper
functions to convert `Either` values to runtime exceptions in this
package. And the `MonadMask` typeclass is now replaced fully by
`MonadUnliftIO`, which like the `monad-control` case limits which
monads we can be working with.

## Async exception safety

The `safe-exceptions` package builds on top of the `exceptions`
package and provides intelligent behavior for dealing with
asynchronous exceptions, a common pitfall. This library provides a set
of exception handling functions with the same async exception behavior
as that library. You can consider this library a drop-in replacement
for `safe-exceptions`. In the future, we may reimplement
`safe-exceptions` to use `MonadUnliftIO` instead of `MonadCatch` and
`MonadMask`.

## Package split

The `unliftio-core` package provides just the typeclass with minimal
dependencies (just `base` and `transformers`). If you're writing a
library, we recommend depending on that package to provide your
instances. The `unliftio` package is a "batteries loaded" library
providing a plethora of pre-unlifted helper functions. It's a good
choice for importing, or even for use in a custom prelude.
