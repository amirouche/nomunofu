
## `(arew pfds fingertrees)`

A Simple General-Purpose Data Structure

### Abstract

Fingertrees are a generalised form of deque, that you can parameterise
to compute a value, called the "measure" of a fingertree. This measure
will be updated incrementally as you add and remove elements from the
fingertree. Among other things, this allows fingertrees to be used
where you otherwise might have written a custom data structure.

To compute the measure, fingertrees require pieces of information: a
converter, a combiner, and an identity.

The converter is a procedure of one argument, that maps values in the
fingertree to other values which are used for computing the measure.

The combiner is a procedure of two arguments, and combines these into
one value representing them both. A combiner must be associative
i.e. (combine A (combine B C)) must be equivalent to (combine (combine
A B) C) for all values A, B and C.

An identity is a value that represents the measure of an empty
fingertree. It must obey the rule that (combine X identity), (combine
identity X) and X are always the same.

To make things more concrete, a simple use of a fingertree is as a
deque that keeps a running total. In this case, the converter can
simply be the procedure `(lambda (x) x)` if it is a deque of integers,
the combiner would be `+`, and the identity `0`.

```scheme
(define l '(3 1 4 1 5 9))

(define ft (list->fingertree l 0 + (lambda (x) x)))

(fingertree-measure ft)
;; => 23
(fingertree-measure (fingertree-snoc ft 2))
;; => 25
(let-values (((head tail) (fingertree-uncons ft)))
  (fingertree-measure tail))
;; => 20
```

### Reference

#### `(fingertree? obj)`

Returns `#t` if argument is a fingertree, `#f` otherwise.

#### `(fingertree-empty? fingertree)`

Returns `#t` if there are no items in the fingertree, `#f` otherwise.

#### `(make-fingertree identifier combine measure)`

Returns a new fingertree, parameterised by the given monoid.

#### `(fingertree-cons obj fingertree)`

Returns the new fingertree created by adding the element `OBJ` to the front
of the argument fingertree.

#### `(fingertree-snoc fingertree obj)`

Returns the new fingertree created by adding the element to the end of
the fingertree.

#### `(fingertree-uncons fingertree)`

Returns two values: the element at the front of the fingertree, and a
new fingertree containing all but the front element. If the fingertree
is empty, a `&fingertree-empty` condition is raised.

#### `(fingertree-unsnoc fingertree)`

Returns two values: a new fingertree containing all but the rear
element of the argument fingertree, and the rear element itself. If
the fingertree is empty, a `&fingertree-empty-condition` is raised.

#### `(fingertree-append fingertree fingertree)`

Returns a new fingertree which contains all of the elements of the
first fingertree argument, followed by all the elements of the
second. The argument fingertrees are assumed to be parameterised by
the same monoid.

#### `(list->fingertree lst identity append convert)`

Returns a fingertree containing all of the elements of the argument
list, in the same order.

#### `(fingertree->list fingertree)`

Returns a list of all the elements in the `FINGERTREE`, in the order
they would be unconsed.

#### `fingertree-measure fingertree)`

Returns the measure of the fingertree, as defined by the fingertree's
monoid.

#### `(fingertree-split predicate fingertree)`

Returns two values: the first is the largest prefix of the fingertree
for which applying the predicate to it's accumulated measure returns
`#f`q. The second values is a fingertree containing all those elements
not in the first fingertree.

#### `(fingertree-split3 proc fingertree)`

Similar to `fingertree-split`, however, instead of returning the
remainder as the second argument, it returns the head of the remainder
as the second argument, and tail of the remainder as the third
argument.

#### `(fingertree-fold proc init fingertree)`

Returns the value obtained by iterating the combiner procedure over
the `FINGERTREE` in left-to-right order. This procedure takes two
arguments, the current value from the fingertree, and an accumulator,
and it's return value is used as the accumulator for the next
iteration. The initial value for the accumulator is given by the base
argument `INIT`.

#### `(fingertree-fold-right proc init fingertree)`

Similar to `fingertree-fold`, but iterates in right-to-left order.

#### `(fingertree-reverse fingertree)`

Returns a new fingertree in which the elements are in the opposite
order from `FINGERTREE`.

#### `(fingertree-empty-condition? condition)`

Returns `#t` if the argument is a `&fingertree-empty` condition, `#f`
otherwise.
