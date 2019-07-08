# Sortee

A library for creating sort key, inspired by Jira's Lexorank.

## Motivation

To represent an ordered list in database, the simplest way is to assign an integer to each row. The problem though is, reordering requires updating multiple rows. Sorting by string can overcome this problem, Sortee provides a function that generates strings for sorting.

## Usage

Let's say you have 3 rows, with "a" "c" "t" as sort key. To move "t" to the middle of the list, you can get the new sort key by:

```haskell
between "a" "c" // "b"
```

If you do not have a lower / upper reference, pass `Nothing`:

```haskell
between Nothing "9" // "4"
between "q" Nothing // "v"
```

When there is no "space" between keys, Sortee increases the length of the string.

```haskell
between "h" "i" == Sortee "hU"
between Nothing "1" == Sortee "0U"
```

### Limitations

Not every possible input is valid.

```haskell
between Nothing "0" // Nothing
between "b" "b" // Nothing
between "b" "b0" // Nothing
```
