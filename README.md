# alpharewrite

## Dependencies

We use `stack` version 3.1.1.
We recommend using `ghc` version 8.6.6.

NOTE: on Ubuntu 24.04, if you are new to Haskell, you may need to install `libgmp3-dev`.

## Usage

```bash
stack build # compile the project
stack exec alpharewrite-exe [1|2|3|4|5] <tfb.json> > tfbp.pure.json
```
The options are:
1. do all rewrites
2. rewrite all types
3. rewrite "atomic" types in the type signature (removing *NL* elements)
4. rewrite type variables
5. rewrite function names (removing *NL* elements)

If no option is provided, the default is 1.

## Description

This tool rewrites Haskell tasks in [TF-Bench](https://github.com/SecurityLab-UCD/TF-Bench) to remove natural language elements from the code,
getting an NL-free alpha-equivalent version of the task.

### Rewriting "Atomic" Types

This step removes natural language elements from the type signature,
while ensuring that the type signature remains valid.
Note that we do not change built-in special constructors like `()` or `[]`.

For example, we rewrite the task

```haskell
-- dependencies
hPutChar :: Handle -> Char -> IO ()
stdout :: Handle
-- code
putChar c = hPutChar stdout c
-- ground truth type signature 
putChar :: Char -> IO ()
```

into

```haskell
-- dependencies
hPutChar :: Handle -> T1 -> T2 ()
stdout :: Handle
-- code
putChar c = hPutChar stdout c
-- ground truth type signature 
putChar :: T1 -> T2 ()
```

### Rewriting Type Variables

This step rewrites all type variables in the code (likely `a`, `b`, ...) to `t1`, `t2`, etc.
Since polymorphic type variables do not contain natural language from the beginning,
only doing this step *should* have little to no impact on LLM's performance.
One example of this step is

```haskell
-- dependencies
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
(++) :: [a] -> [a] -> [a]
(.) :: (b -> c) -> (a -> b) -> a -> c
-- code
concatMap f = foldr ((++) . f) []
-- ground truth type signature
concatMap ::  (a -> [b]) -> [a] -> [b]
```

```haskell
-- dependencies
foldr :: Foldable t1 => (t2 -> t3 -> t3) -> t3 -> t1 t2 -> t3
(++) :: [t1] -> [t1] -> [t1]
(.) :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
-- code
concatMap f = foldr ((++) . f) []
-- ground truth type signature
concatMap :: (t1 -> [t2]) -> [t1] -> [t2]
```

### Rewriting Function Names

This step removes natural language elements from function names.
For example, we rewrite task

```haskell
-- dependencies
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]
-- code
splitAt n xs =  (take n xs, drop n xs)
-- ground truth type signature
splitAt :: Int -> [a] -> ([a],[a])

```

```haskell
-- dependencies
f2 :: Int -> [a] -> [a]
f3 :: Int -> [a] -> [a]
-- code
f1 n xs = (f2 n xs, f3 n xs)
-- ground truth type signature
f1 :: Int -> [a] -> ([a],[a])
```
