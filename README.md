# Haskell assignment base

<img alt="points bar" align="right" height="36" src="../../blob/badges/.github/badges/points-bar.svg" />

## Guidelines

When solving the homework, strive to create not just code that works, but code that is readable and concise.
Try to write small functions which perform just a single task, and then combine those smaller
pieces to create more complex functions.

Don’t repeat yourself: write one function for each logical task, and reuse functions as necessary.

Don't be afraid to introduce new functions where you see fit.

### Sources

Each task has corresponding source file in [src](src) directory where you should implement the solution.

### Building

All solutions should compile without warnings with following command:

```bash
stack build
```

### Testing

You can and should run automated tests before pushing solution to GitHub via

```bash
stack test --test-arguments "-p TaskX"
```

where `X` in `TaskX` should be number of corresponding Task to be tested.

So to run all test for the first task you should use following command:

```bash
stack test --test-arguments "-p Task1"
```

You can also run tests for all tasks with just

```bash
stack test
```

### Debugging

For debugging you should use GHCi via stack:

```bash
stack ghci
```

You can then load your solution for particular task using `:load TaskX` command.

Here is how to load Task1 in GHCi:

```bash
$ stack ghci
ghci> :load Task1
[1 of 1] Compiling Task1 ( .../src/Task1.hs, interpreted )
Ok, one module loaded.
```

> **Note:** if you updated solution, it can be quickly reloaded in the same GHCi session with `:reload` command
> ```bash
> ghci> :reload
> ```

## Task 1 (6 points)

Implement factorial of n.

$$
n! = 1 \cdot 2 \cdot ... \cdot n
$$

```haskell
factorial :: Integer
```
**Example:**
```haskell
>>> factorial 0
1
>>> factorial 5
120
```

## Task 2 (4 points)

Implement "sumtorial" of n.

$$
f(n) =\begin{cases}
1& \text{if } n = 0\\
\sum_{i=1}^n{i}& \text{otherwise}
\end{cases}
$$

```haskell
sumtorial :: Integer
```
**Example:**
```haskell
>>> sumtorial 0
1
>>> sumtorial 5
16
```
