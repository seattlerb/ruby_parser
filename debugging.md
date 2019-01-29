# Quick Notes to Help with Debugging

## Comparing against ruby / ripper:

```
% rake cmp3 F=file.rb
```

This compiles the parser & lexer and then parses file.rb using both
ruby, ripper, and ruby_parser in debug modes. The output is munged to
be as uniform as possible and diffable. I'm using emacs'
`ediff-files3` to compare these files (via `rake cmp3`) all at once,
but regular `diff -u tmp/{ruby,rp}` will suffice for most tasks.

From there? Good luck. I'm currently trying to backtrack from rule
reductions to state change differences. I'd like to figure out a way
to go from this sort of diff to a reasonable test that checks state
changes but I don't have that set up at this point.
