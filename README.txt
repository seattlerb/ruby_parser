ruby_parser
    by Ryan Davis
    http://parsetree.rubyforge.org/

== DESCRIPTION:

ruby_parser (RP) is a ruby parser written in pure ruby (utilizing
racc--which does by default use a C extension). RP's output is
the same as ParseTree's output: s-expressions using ruby's arrays and
base types.

== FEATURES/PROBLEMS:

* Pure ruby, no compiles.
* Incredibly simple interface.
* Output is 100% equivalent to ParseTree.
  * Can utilize PT's SexpProcessor and UnifiedRuby for language processing.
* Known Issue: Speed sucks currently. 5500 tests currently run in 21 min.
* Known Issue: Code is waaay ugly. Port of a port. Not my fault. Will fix RSN.
* Known Issue: I don't currently support newline nodes.
* Known Issue: Totally awesome.
* Known Issue: dasgn_curr decls can be out of order from ParseTree's.
* TODO: Add comment nodes.

== SYNOPSIS:

  RubyParser.new.parse "1+1"
  # => s(:call, s(:lit, 1), :+, s(:array, s(:lit, 1)))

== REQUIREMENTS:

* ruby. woot.
* ParseTree is needed for Sexp class... crap. I might break that out.
* ParseTree for testing.
* racc full package for parser development.

== INSTALL:

* sudo gem install ruby_parser

== LICENSE:

(The MIT License)

Copyright (c) 2007 Ryan Davis

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
