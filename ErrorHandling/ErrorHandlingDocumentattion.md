# Documentation du module ErrorMonad

Ce document décrit le fonctionnement du module `ErrorMonad`, qui fournit une gestion simple et expressive des erreurs en Haskell sans dépendances externes, en s’appuyant sur `Either`.

---

## 📊 Types de base

### `type Err a = Either String a`

Un alias pratique pour `Either String a`.

* `Right x` représente une valeur correcte.
* `Left msg` représente une erreur avec un message.

Ce type est monadique, ce qui permet la propagation automatique des erreurs dans la `do`-notation.

---

## 🔧 Fonctions de base

### `throw`

Permet de lever une erreur immédiatement. Tout calcul suivant dans la monade est ignoré, et l’erreur remonte jusqu’au point où le résultat est inspecté.

```haskell
throw "Division by Zero"
```

---

### `runErr`

Exécute un calcul encapsulé dans `Err` et renvoie le résultat sous forme d’un `Either String a`.
C’est essentiellement l’identité, mais il fournit une abstraction claire pour signaler la sortie du contexte d’erreur.

```haskell
runErr (Right 42)   -- renvoie Right 42
runErr (Left "err") -- renvoie Left "err"
```

---

## ⚙️ Exemple d’utilisation

Voici un petit exemple d’utilisation pour gérer des erreurs simples :

```haskell
divide :: Int -> Int -> Err Int
divide _ 0 = throw "Division by zero"
divide x y = return (x `div` y)

example :: Err Int
example = do
    a <- divide 10 2
    b <- divide a 0
    return b

-- runErr example = Left "Division by zero"
```
