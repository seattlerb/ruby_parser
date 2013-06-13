# ruby_parser

* [HOME](https://github.com/seattlerb/ruby_parser)
* [BUGS](https://github.com/seattlerb/ruby_parser/issues)
* [RDOC](http://docs.seattlerb.org/ruby_parser)

## DESCRIPTION

ruby_parser (RP) is a ruby parser written in pure ruby (utilizing
racc--which does by default use a C extension). RP's output is
the same as ParseTree's output: s-expressions using ruby's arrays and
base types.

As an example:

```ruby
def conditional1 arg1
  return 1 if arg1 == 0
  return 0
end
```

becomes:

```ruby
s(:defn, :conditional1, s(:args, :arg1),
  s(:if,
    s(:call, s(:lvar, :arg1), :==, s(:lit, 0)),
    s(:return, s(:lit, 1)),
    nil),
  s(:return, s(:lit, 0)))
```

## FEATURES/PROBLEMS:

* Pure ruby, no compiles.
* Includes preceding comment data for defn/defs/class/module nodes!
* Incredibly simple interface.
* Output is 100% equivalent to ParseTree.
  * Can utilize PT's SexpProcessor and UnifiedRuby for language processing.
* Known Issue: Speed is now pretty good, but can always improve:
  * RP parses a corpus of 3702 files in 125s (avg 108 Kb/s)
  * MRI+PT parsed the same in 67.38s (avg 200.89 Kb/s)
* Known Issue: Code is much better, but still has a long way to go.
* Known Issue: Totally awesome.
* Known Issue: line number values can be slightly off. Parsing LR sucks.

## SYNOPSIS:

```ruby
  RubyParser.new.parse "1+1"
  # => s(:call, s(:lit, 1), :+, s(:lit, 1))
```

You can also use Ruby19Parser, Ruby18Parser, or RubyParser.for_current_ruby:

```ruby
  RubyParser.for_current_ruby.parse "1+1"
  # => s(:call, s(:lit, 1), :+, s(:lit, 1))
```

## REQUIREMENTS:

* ruby. woot.
* sexp_processor for Sexp and SexpProcessor classes, and testing.
* racc full package for parser development (compiling .y to .rb).

## INSTALL:

`sudo gem install ruby_parser`

## LICENSE:

(The MIT License)

Copyright (c) Ryan Davis, seattle.rb

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
'Software'), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
