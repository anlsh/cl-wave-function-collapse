# WaveFunctionCollapse in Common Lisp
### Anish Moorthy <anlsh@protonmail.com>

This is a Common Lisp "Implementation" of [Maxim Gumin's WaveFunction
Collapse](https://github.com/mxgmn/WaveFunctionCollapse).

As far as I know, this code correctly implements the "overlap" mode of the WFC
algorithm. However it's just an "implementation" because it's missing a lot of
features as compared to the original repo, and is too slow to use in practice :|

Also its not structured well at all: for example, there are still some hardcoded
paths to files that I used for testing on my system, etc. Despite that, I hope
that the code is at least understandable. It is my first nontrivial project in
Common Lisp, so although I strove to keep the code of decent quality I'm sure
that there are some issues.

## Regarding performance

As I said, the code is too slow to use in practice: but I don't think it needs
to be. Significant speedups could be achieved by tackling the following areas.

1. I `:use` the `generic-cl` package, when the only things I really need from it
   ire `elt`, `equalp`, and its `hash-maps`. I'm not sure what the performance
   penaly is here (hopefully small, but I have no idea), but selective imports
   would probably eliminate a ton of unnecessary dynamic dispatch.

2. There's a lot of unnecessary `cons`ing going on. The `range` and `product`
   functions are particularly egrigious offenders due to how often they're
   called, but the `set` operations could use some redoing too.

   The performance issues of `range` and `product` could be easily solved with a
   proper `range` function and `itertools` package. I'll [probably try my
   hand](https://xkcd.com/927/) at polishing and/or implementing these soon.

## License

MIT
