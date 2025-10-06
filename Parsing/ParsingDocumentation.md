# Parsing Module Documentation

This document describes the functionality of the `Parsing` module, which provides a custom parser implementation using the `Functor`, `Applicative`, `Alternative`, and `Monad` typeclasses.

---

## 📊 Basic Types

### `type ParserType a = String -> Maybe (a, String)`
A function that takes a string and returns, upon success, a value of type `a` and the rest of the unconsumed string.

### `data Parser a = Parser { runParser :: String -> Maybe (a, String) }`
The main type. Represents a parser for elements of type `a`.

---

## 🔧 Core Functions

### `fmapParser`
Applies a function to the result returned by a parser.

```haskell
runParser (fmap (+1) (pure 3)) "abc"  -- Just (4, "abc")

runParser ((+1) <$> (pure 3)) "abc"  -- Just (4, "abc")
```

### `pureParser`
Creates a parser that returns a value without consuming the string.

```haskell
runParser (pureParser 5) "hello" -- Just (5, "hello")```

### `appParser`
Applies a function parser to a value parser.

```haskell
runParser ((pure (*2)) <*> pure 4) "abc" -- Just (8, "abc")
```

### `emptyParser`
A parser that always fails.

```haskell
runParser emptyParser "test" -- Nothing```

### `altParser`
Tries the first parser, and if it fails, tries the second one.

```haskell
runParser (parseChar 'a' <|> parseChar 'b') "abc" -- Just ('a', "bc")
```

### `monadParser`
Allows for chaining two parsers.

```haskell
parseTuple p = do
    _ <- parseChar '('
    a <- p
    _ <- parseChar ','
    b <- p
    _ <- parseChar ')'
    return (a, b)
```

---

## 🔍 Simple Parsers

### `parseChar :: Char -> Parser Char`
Parses an exact character.

```haskell
runParser (parseChar 'a') "abc" -- Just ('a', "bc")
```

### `parseOneChar :: Parser Char`
Parses a single character, whatever it may be.

```haskell
runParser parseOneChar "abc" -- Just ('a', "bc")
```

### `parseString :: String -> Parser String`
Parses an exact string.

```haskell
runParser (parseString "hello") "hello world" -- Just ("hello", " world")
```

### `parseAnyChar :: String -> Parser Char`
Parses one character from a given set of characters.

```haskell
runParser (parseAnyChar "abc") "bcd" -- Just ('b', "cd")
```

### `tryMaybe :: Parser a -> Parser (Maybe a)`
Tries a parser, returns `Nothing` on failure, without causing an error.

```haskell
runParser (tryMaybe (parseChar 'x')) "xyz" -- Just (Just 'x', "yz")

runParser (tryMaybe (parseChar 'x')) "abc" -- Just (Nothing, "abc")
```

---

## ♻️ Repetition

### `parseMany`, `parseSome`
Repeats a parser until failure. `parseMany` can never fail (it can return an empty list), while `parseSome` fails if the first run of the parser fails.

```haskell
runParser (parseMany (parseChar 'a')) "aaabbb" -- Just ("aaa", "bbb")
```

---

## 📊 Integers

### `parseUInt :: Parser Int`
Parses an unsigned integer.

```haskell
runParser parseUInt "42abc" -- Just (42, "abc")
```

### `parseInt :: Parser Int`
Parses an integer that can be signed.

```haskell
runParser parseInt "-42abc" -- Just (-42, "abc")

runParser parseInt "15xyz" -- Just (15, "xyz")
```

---

## ⚖️ Structures

### `parseTuple :: Parser a -> Parser (a, a)`
Parses a pair enclosed in parentheses, separated by a comma, with no spaces.

```haskell
runParser (parseTuple parseInt) "(-1,42)abc" -- Just ((-1,42), "abc")
```

### `parseTruple :: Parser (Int, Int, Int)`
Parses a triplet of integers, separated by a comma, with no spaces.

```haskell
runParser parseTruple "(1,2,3)" -- Just ((1,2,3), "")
```

---

## ⚖️ Custom Functions

### `parseWhile :: Int -> String -> Parser String`
Parses all characters up to a given end string (without consuming it).
Takes an `Int` as a parameter that defines the number of delimiters to pass before stopping. Use `-1` to stop at the last delimiter.

```haskell
runParser (parseWhile 2 "]") "abc]x]yz" -- Just ("abc]x", "yz")

runParser (parseWhile 1 "}-") "abc}-xyz" -- Just ("abc", "}-xyz")
```

### `parseBetween :: Int -> String -> String -> Parser String`
Parses a string between two given delimiters.
Takes an `Int` that works the same way as in `parseWhile` for the end delimiter.

```haskell
runParser (parseBetween 1 "[" "]") "[hello]world" -- Just ("hello", "world")

runParser (parseBetween 1 "-{" "}-") "-{hello}-world" -- Just ("hello", "world")
```

### `parseAnyCharExcept :: String -> Parser String`
Parses all characters as long as they are not in a list of forbidden characters.

```haskell
runParser (parseAnyCharExcept "c") "abcde" -- Just ("ab", "cde")

runParser (parseAnyCharExcept "tdy") "abcde" -- Just ("abc", "de")
```

### `parseWithoutConsum :: Parser a -> Parser String`
Runs a parser but does not consume the parsed part on success.

```haskell
runParser (parseWithoutConsum (parseString "hello")) "hello world" -- Just ("", "hello world")

runParser (parseWithoutConsum (parseString "hello")) "not hello world" -- Nothing
```
