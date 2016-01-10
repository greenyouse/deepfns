# deepfns

A Clojure library for deeply nested data transformations!

There will probably be a proper documentation site once development
wraps up. For now I added some quick descriptions for each of the
transformations.

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

## License

Copyright © 2016 Ed Babcock

Distributed under the Eclipse Public License version 1.0
