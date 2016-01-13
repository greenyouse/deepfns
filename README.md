# deepfns

A Clojure library for deeply nested data transformations!

### fmap

Basically it updates the values inside of data structures while
preserving the data-types of the structures. When passing in multiple
collections, make sure that every collection has the same type (all the
way down too)!

``` clj
(deepfmap inc [1 2 3])
;; => [2 3 4]

(deepfmap inc [1 2 3] [4 5 6])
;; => ([2 3 4] [5 6 7])

(deepfmap inc {:a 1 :b 2} {:c 3 :d 4})
;; => ({:a 2 :b 3} {:c 4 :d 5})

(deepfmap inc {:a [1 2] :b #{3}})
;; => {:a [2 3] :b #{4}}
```

### fapply + pure

The pure function will wrap a value in some type like this:

```clj
(pure [] 1) 
;; => [1]

(pure ['vals] 1) ; any contents are replaced by 1
;; => [1]
```

The deeppure function is similar but it will replace all contents with
the given value and insert the value into any empty collections:

```clj
(deeppure [1 []] 2)
;; => [2 [2]]
(deeppure #{{:a 1}} 2)
;; => #{{:a 2}}
```

The deepfapply function will act similar to fapply by applying some
function wrapped in a type to an argument wrapped an identical type and
return the result wrapped in the same type. The difference in deepfapply
is that it can operated on nested types and take any number of arguments:

```clj
(deepfapply [inc] [1 2 3])
;; => [2 3 4]
(deepfapply [inc dec] [1 2 3])
;; => [2 3 4 0 1 2]
(deepfapply #{[+]} #{[1]} #{[2 3]})
;; => #{[6]}
(deepfapply [] [1 2 3])
;; => []
(deepfapply {:a *} {:a 1} {:a 2 :b 3} {:a 3 :b 4 :c 4})
;; => {:a 6 :b 3 :c 4}
```

As you can see deepfapply also has a few other attributes that are worth
talking about:
- When multiple functions are used, deepfapply will return all the
  results (see example 2).
- If a map is used for the type, the function will only be applied to
  matching keys and all extra keys will be returned in the result. If
  there multiple values for non-matching keys, the value from the
  earlier argument takes precedence (see `:b` in the last example).
- Using an empty collection as the function will always return an empty
  collection.

### filterapply

This is essentially the same as deepfapply but when maps are used the
non-matching keys will not be returned in the result:

```clj
(filterapply {:a +} {:a 1} {:b 2})
;; => 1
(filterapply {:a *} {:a 1} {:a 2 :b 3} {:a 3 :b 4 :c 4})
;; => {:a 6}
```

### traverse

The traverse function is like an inside-out deepfapply. It will match
keys in the function map, apply any functions, and build up the results
in the function map. Here are a few examples:

```clj
(traverse [:foo [:bar]] {:foo 1 :bar 2})
;; => [1 [2]]

(traverse {:a :foo} {:foo 1})
;; => {:a 1}

(traverse {:a :foo} {:foo 1} {:foo 2})
;; => ({:a 1} {:a 2)

(traverse {:a {:b {:c :foo}}
            :d :foo}
 {:foo 1})
;; => {:a {:b {:c 1}} :d 1}

(traverse {:a inc} 1)
;; => {:a 2}

(traverse {:a "some"} {:a 1})
;; => {:a "some"}
```

See how it applies the functions to the arguments and inserts the
results?

Here are a few things to note for traverse:
- Using the variadic form will map traverse over the arguments and
  return a list of the results (see example 2).
- Using normal Clojure functions instead of keywords will apply the
  function to each argument too (see second to last example).
- If you use a value that's not a function, it will be wrapped in a
  constantly so it is always returned (see last example).

## License

Copyright © 2016 Ed Babcock

Distributed under the Eclipse Public License version 1.0
