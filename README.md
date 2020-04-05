# `purescript-iterator`

Iterators are lazy, generic structures that are computed on-demand rather than read from memory. They allow for truly infinite sequences without memory overhead; instead the values are calculated as they're accessed, increasing the computation overhead instead.
The iterators in this package are implemented in JavaScript with generators, which , unlike lazy lists, do not use recursion and do not suffer from call stack exhaustion.
