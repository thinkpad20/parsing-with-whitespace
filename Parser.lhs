We want to make a grammar for a whitespace-sensitive language. Let's begin with an absolutely simple language; all it has are words and blocks. For example

```
this is a line
this is another
  this is contained inside
  this is also
now I'm back
  oh now I'm in a block again
    now I'm in another block
  now I'm a little back
    now I'm back again
I just jumped back 2
```
''''

this would be tokenized as

```l, l, +, l, l, -, l, +, l, +, l, -, l, +, l, -, -, l, EOI```
''''

Where we denote l = line, + = indent, - = dedent, EOI = end of input.

so we have three tokens, string (s), indent (+), dedent (-)

we can't just "see" an indent. The rule for an indent is:
  k spaces, where k > current indent

similarly a dedent is
  k spaces, where k < current indent

then a string is simply
  [a-zA-Z0-9 ,.'"] ++ "\n"

then we have to update the current indent; when does this happen? Well whenever we have an indent or dedent, of course. In particular, if we have an indent of k spaces, then the current indent increases by k, and vice versa for a dedent. Initially, the indent is 0.

Let's start just by introducing the type of the parser we're going to be dealing with.

```

import Text.ParserCombinators.Parsec
import Control.Monad.State

data Token = Line [String] 
           | Block [Token]
           deriving (Show)

data Newline = Newline
             | Indent
             | Dedent
             deriving (Show)

type IParser = StateT Int Parser

```

Alright, so a `Parser` is Parsec's monad to keep track of the state of your parsing. We need to add on some extra information on top of that, so that we can store our indent; this is most easily accomplished with just an `Int`. Therefore, we use a `StateT` monad transformer. A monad transformer is a monad which contains another monad inside of it (in this case, a `Parser`), and implements the `MonadTrans` type class which gives us some useful functions that we'll see going forward. So in summary, an `IParser` is a modified version of a `Parser` which additionally is able to keep track of an `Int`.

Next, let's introduce a really simple parser; all it will do is parse a string.

```

parseWord :: IParser String
parseWord = do
  w <- lift $ many1 $ digit <|> letter
  lift $ many $ char ' '
  return w

```

In the type signature we can see that we're returning an `IParser String` instead of a `Parser String`, as we'd normally see in Parsec. What this means, though, is that we now have to use the `lift` function whenever we want to call Parsec functions. Essentially, if you have a monad transformer `A` which contains another monad `B`, and `f` returns a `B`, then `lift f` will return an `A` and keep things simple.

Before we get too much further, let's make some convenience functions which will make our lives easier. Normally we'd use a function `parse` which takes some parser, a language name and some string, and returns an `Either ParseError a`. But since we're using an `IParser`, we can't use this as-is: we need a way to get the `Parser` out of the `IParser` and feed it into the `parse` function.

Fortunately, we have just such a function: `runStateT`. The type of `runStateT` is `runStateT :: StateT s m a -> s -> m (a, s)`. Effectively, it takes a State transformer and returns a function which, when given some initial state, returns a new state. Crucially, `runStateT` alone doesn't really do much: we need to give it some starting point for it to work. Well, our starting point is the indent of 0, so here we go:

```

start :: IParser a -> Int -> Parser (a, Int)
start p i = runStateT p i

```

Just to go over this one more time, `p` is an `IParser` which contains some `a`. Therefore `runStateT p` will return a function `f :: Int -> Parser (a, Int)`. We call this function on the integer `i`, and there we go.

Now that we can produce a `Parser` monad, we can use it wherever we would in regular Parsec; specifically, we can pass it into the `parse` function.

```

parse' :: IParser a -> String -> Either ParseError (a, Int)
parse' parser input = parse (start parser 0) "mylang" input

```

Now we can try running this code in ghci:

```
ghci> parse parseWord "heyheyhey"
Right ("heyheyhey", 0)
```"


Alright, now that we have some basics covered, let's move on to a bit more complexity. Here's the next hurdle we encounter: we didn't include a space in our word definition. That means that a line can contain any number of words. This means that we need to write a parser which can string words together. Now, in Parsec, we normally use `many1` for this. The type of `many1` is:

```
many1
  :: Text.Parsec.Prim.Stream s m t =>
     Text.Parsec.Prim.ParsecT s u m a
     -> Text.Parsec.Prim.ParsecT s u m [a]
```
''''


Or more simply,

```
many1 :: Parser a -> Parser [a]
```
''''

So, `many1` operates on `Parser`s, not `IParser`s! What to do? Well, before we converted between the two with `runStateT`. But we just passed in 0 then; now, we don't know ahead of time what indent we'll have. But hey! There's a function that does just that!

```
get :: MonadState s m -> m s
```
''''

What this is effectively telling us is that we can "pull out" the state. Remember `runStateT` will give us a function `f :: Int -> Parser (a, Int)`. Once we have the indent state, we can push it into `f` a `Parser (a, Int)`. But we only need a `Parser a`, so we can use the `fst` function to drop the Int. Here's the finished product:

```

type Line = [String]

parseLine :: IParser Token
parseLine = do
  ind <- get
  wordz <- lift $ many1 $ fmap fst $ runStateT parseWord ind
  lift $ char '\n'
  return $ Line wordz

```

We can try it out, and it seems to work:

```
ghci> parse' parseLine "hey hey hey\n"
Right (["hey","hey","hey"],0)
```"

OK, so that was kinda painful! Maybe we can smooth that out going forward, but for now let's press on, and see if we can work out indents and dedents, finally!

Remember our definition of an indent: k spaces, where k is greater than the current indent.

```

parseIndent :: IParser Newline
parseIndent = do
  lift $ char '\n'
  sps <- lift $ many $ char ' '
  ind <- get
  let k = length sps
  if k > ind
    then put k >> return Indent
    else if k < ind
      then put k >> return Dedent
      else return Newline

```

OK, so I think this is pretty clear, but just to review, `lift` lets us use the `many1` function inside our `IParser`. We get the number of spaces by the length of the list. Then we compare it to our current indent, which we got with `get`. Based on the comparison, we either increment/decrement, or not. Note that we don't need to store any "result" with this, so our internal "value" is just the `()` unit.

OK, so now let's try to piece these together.

```

parseBlock :: IParser Token
parseBlock = undefined

```

This is tough! :P