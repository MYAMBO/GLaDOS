# Documentation du module Parsing

Ce document décrit le fonctionnement du module `Parsing`, qui fournit une implémentation personnalisée de parseurs à l'aide des classes `Functor`, `Applicative`, `Alternative` et `Monad`.

---

## 📊 Types de base

### `type ParserType a = String -> Maybe (a, String)`
Une fonction qui prend une chaîne de caractères et retourne, si réussi, une valeur de type `a` et le reste de la chaîne non consommée.

### `data Parser a = Parser { runParser :: String -> Maybe (a, String) }`
Le type principal. Représente un parseur d'éléments de type `a`.

---

## 🔧 Fonctions de base

### `fmapParser`
Applique une fonction au résultat retourné par un parseur.

```haskell
runParser (fmap (+1) (pure 3)) "abc"  -- Just (4, "abc")

runParser ((+1) <$> (pure 3)) "abc"  -- Just (4, "abc")
```

### `pureParser`
Crée un parseur qui retourne une valeur sans consommer la chaîne.

```haskell
runParser (pureParser 5) "hello" -- Just (5, "hello")
```

### `appParser`
Applique un parseur de fonction à un parseur de valeur.

```haskell
runParser ((pure (*2)) <*> pure 4) "abc" -- Just (8, "abc")
```

### `emptyParser`
Un parseur qui échoue toujours.

```haskell
runParser emptyParser "test" -- Nothing
```

### `altParser`
Essaye le premier parseur, si échec, essaye le deuxième.

```haskell
runParser (parseChar 'a' <|> parseChar 'b') "abc" -- Just ('a', "bc")
```

### `monadParser`
Permet d'enchaîner deux parseurs.

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

## 🔍 Parseurs simples

### `parseChar :: Char -> Parser Char`
Parse un caractère exact.

```haskell
runParser (parseChar 'a') "abc" -- Just ('a', "bc")
```

### `parseOneChar :: Parser Char`
Parse un seul caractère, quel qu’il soit.

```haskell
runParser parseOneChar "abc" -- Just ('a', "bc")
```

### `parseString :: String -> Parser String`
Parse une chaîne de caractères exacte.

```haskell
runParser (parseString "hello") "hello world" -- Just ("hello", " world")
```

### `parseAnyChar :: String -> Parser Char`
Parse un caractère parmi ceux donnés.

```haskell
runParser (parseAnyChar "abc") "bcd" -- Just ('b', "cd")
```

### `tryMaybe :: Parser a -> Parser (Maybe a)`
Tente un parseur, retourne `Nothing` si échec, sans erreur.

```haskell
runParser (tryMaybe (parseChar 'x')) "xyz" -- Just (Just 'x', "yz")

runParser (tryMaybe (parseChar 'x')) "abc" -- Just (Nothing, "abc")
```

---

## ♻️ Répétitions

### `parseMany`, `parseSome`
Répète un parseur jusqu'à échec, parseMany ne peut jamais échouer, parseSome échoue si le premier passage du parseur échoue

```haskell
runParser (parseMany (parseChar 'a')) "aaabbb" -- Just ("aaa", "bbb")
```

---

## 📊 Entiers

### `parseUInt :: Parser Int`
Parse un entier non signé.

```haskell
runParser parseUInt "42abc" -- Just (42, "abc")
```

### `parseInt :: Parser Int`
Parse un entier qui peut être signé.

```haskell
runParser parseInt "-42abc" -- Just (-42, "abc")

runParser parseInt "15xyz" -- Just (15, "xyz")
```

---

## ⚖️ Structures

### `parseTuple :: Parser a -> Parser (a, a)`
Parse une paire entre parenthèses, séparé par une virgule, sans espace.

```haskell
runParser (parseTuple parseInt) "(-1,42)abc" -- Just ((-1,42), "abc")
```

### `parseTruple :: Parser (Int, Int, Int)`
Parse un triplet d'entiers, séparé par une virgule, sans espace.

```haskell
runParser parseTruple "(1,2,3)" -- Just ((1,2,3), "")
```

---

## ⚖️ Fonctions personnalisées

### `parseWhile :: Int -> String -> Parser String`
Parse tous les caractères jusqu'à une chaîne de fin donnée (sans consommer celle-ci).
Prends un Int en paramètre qui définit le nombre de délimiteurs à passer avant d'arrêter le parseWhile. -1 pour s'arrêter au dernier.

```haskell
runParser (parseWhile 2 "]") "abc]x]yz" -- Just ("abc]x", "yz")

runParser (parseWhile 1 "}-") "abc}-xyz" -- Just ("abc", "}-xyz")
```

### `parseBetween :: Int -> String -> String -> Parser String`
Parse une chaîne entre deux délimiteurs donnés.
Prends un Int, même fonctionnement que parseWhile pour le délimiteur de fin.

```haskell
runParser (parseBetween "[" "]") "[hello]world" -- Just ("hello", "world")

runParser (parseBetween "-{" "}-") "-{hello}-world" -- Just ("hello", "world")
```

### `parseAnyCharExcept :: String -> Parser String`
Parse tous les caractères tant qu’ils ne sont pas dans une liste d’interdits.

```haskell
runParser (parseAnyCharExcept "c") "abcde" -- Just ("ab", "cde")

runParser (parseAnyCharExcept "tdy") "abcde" -- Just ("abc", "de")
```

### `parseWithoutConsum :: Parser a -> Parser String`
Lance un parser mais ne consume pas la partie parsé en cas de réussite.

```haskell
runParser (parseWithoutConsum (parseString "hello")) "hello world" -- Just ("", "hello world")

runParser (parseWithoutConsum (parseString "hello")) "not hello world" -- Nothing
```

---
