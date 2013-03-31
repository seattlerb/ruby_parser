#!/usr/local/bin/ruby
# encoding: utf-8

# ENV['VERBOSE'] = "1"

require 'rubygems'
gem "minitest"
require 'minitest/autorun'
require 'ruby_parser'

$: << File.expand_path('~/Work/p4/zss/src/sexp_processor/dev/lib')

require 'pt_testcase'

class Sexp
  alias oldeq2 ==
  def ==(obj) # :nodoc:
    if obj.class == self.class then
      super and
        (self.line.nil? or obj.line.nil? or self.line == obj.line)
    else
      false
    end
  end
end

class RubyParserTestCase < ParseTreeTestCase
  attr_accessor :result, :processor

  make_my_diffs_pretty!

  def self.previous key
    "Ruby"
  end

  def self.generate_test klass, node, data, input_name, output_name
    return if node.to_s =~ /bmethod|dmethod/
    return if Array === data['Ruby']

    output_name = "ParseTree"

    super
  end

  def assert_parse rb, pt
    self.result = processor.parse rb
    assert_equal pt, result
  end

  def assert_syntax_error rb, emsg
    e = nil
    assert_silent do
      e = assert_raises RubyParser::SyntaxError do
        processor.parse rb
      end
    end

    assert_equal emsg, e.message
  end

  def assert_parse_error rb, emsg
    e = nil
    assert_silent do
      e = assert_raises Racc::ParseError do
        processor.parse rb
      end
    end

    assert_equal emsg, e.message
  end

  def assert_parse_line rb, pt, line
    assert_parse rb, pt
    assert_equal line, result.line,   "call should have line number"
  end
end

module TestRubyParserShared
  def setup
    super
    # p :test => [self.class, __name__]
  end

  BLOCK_DUP_MSG = "Both block arg and actual block given."

  def test_double_block_error_01
    assert_syntax_error "a(1, &b) { }", BLOCK_DUP_MSG
  end

  def test_double_block_error_02
    assert_syntax_error "a(1, &b) do end", BLOCK_DUP_MSG
  end

  def test_double_block_error_03
    assert_syntax_error "a 1, &b do end", BLOCK_DUP_MSG
  end

  def test_double_block_error_04
    assert_syntax_error "m.a(1, &b) { }", BLOCK_DUP_MSG
  end

  def test_double_block_error_05
    assert_syntax_error "m.a(1, &b) do end", BLOCK_DUP_MSG
  end

  def test_double_block_error_06
    assert_syntax_error "m.a 1, &b do end", BLOCK_DUP_MSG
  end

  def test_double_block_error_07
    assert_syntax_error "m::a(1, &b) { }", BLOCK_DUP_MSG
  end

  def test_double_block_error_08
    assert_syntax_error "m::a(1, &b) do end", BLOCK_DUP_MSG
  end

  def test_double_block_error_09
    assert_syntax_error "m::a 1, &b do end", BLOCK_DUP_MSG
  end

  def test_wtf_7
    rb = "a.b (1) {c}"
    pt = s(:iter,
           s(:call, s(:call, nil, :a), :b, s(:lit, 1)),
           s(:args),
           s(:call, nil, :c))

    assert_parse rb, pt
  end

  def test_wtf_8
    rb = "a::b (1) {c}"
    pt =  s(:iter,
            s(:call, s(:call, nil, :a), :b, s(:lit, 1)),
            s(:args),
            s(:call, nil, :c))

    assert_parse rb, pt
  end

  def test_attrasgn_array_lhs
    rb = '[1, 2, 3, 4][from .. to] = ["a", "b", "c"]'
    pt = s(:attrasgn,
           s(:array, s(:lit, 1), s(:lit, 2), s(:lit, 3), s(:lit, 4)),
           :[]=,
           s(:dot2,
             s(:call, nil, :from),
             s(:call, nil, :to)),
           s(:array, s(:str, "a"), s(:str, "b"), s(:str, "c")))

    assert_parse rb, pt
  end

  def test_block_append
    head = s(:args)
    tail = s(:zsuper)
    expected = s(:block, s(:args), s(:zsuper))
    assert_equal expected, processor.block_append(head, tail)
  end

  def test_block_append_begin_begin
    head = s(:begin, s(:args))
    tail = s(:begin, s(:args))
    expected = s(:block, s(:args), s(:begin, s(:args)))
    assert_equal expected, processor.block_append(head, tail)
  end

  def test_block_append_block
    head = s(:block, s(:args))
    tail = s(:zsuper)
    expected = s(:block, s(:args), s(:zsuper))
    assert_equal expected, processor.block_append(head, tail)
  end

  def test_block_append_nil_head
    head = nil
    tail = s(:zsuper)
    expected = s(:zsuper)
    assert_equal expected, processor.block_append(head, tail)
  end

  def test_block_append_nil_tail
    head = s(:args)
    tail = nil
    expected = s(:args)
    assert_equal expected, processor.block_append(head, tail)
  end

  def test_block_append_tail_block
    head = s(:call, nil, :f1)
    tail = s(:block, s(:undef, s(:lit, :x)), s(:undef, s(:lit, :y)))
    expected = s(:block,
                 s(:call, nil, :f1),
                 s(:block, s(:undef, s(:lit, :x)), s(:undef, s(:lit, :y))))
    assert_equal expected, processor.block_append(head, tail)
  end

  def test_call_array_arg
    rb = "1 == [:b, :c]"
    pt = s(:call, s(:lit, 1), :==, s(:array, s(:lit, :b), s(:lit, :c)))

    assert_parse rb, pt
  end

  def test_call_env
    processor.env[:a] = :lvar
    rb = "a.happy"
    pt = s(:call, s(:lvar, :a), :happy)

    assert_parse rb, pt
  end

  def test_dasgn_icky2
    rb = "a do\n  v = nil\n  begin\n    yield\n  rescue Exception => v\n    break\n  end\nend"
    pt = s(:iter,
           s(:call, nil, :a),
           s(:args),
           s(:block,
             s(:lasgn, :v, s(:nil)),
             s(:rescue,
               s(:yield),
               s(:resbody,
                 s(:array, s(:const, :Exception), s(:lasgn, :v, s(:gvar, :$!))),
                 s(:break)))))

    assert_parse rb, pt
  end

  def test_class_comments
    rb = "# blah 1\n# blah 2\n\nclass X\n  # blah 3\n  def blah\n    # blah 4\n  end\nend"
    pt = s(:class, :X, nil,
           s(:defn, :blah, s(:args), s(:nil)))

    assert_parse rb, pt

    assert_equal "# blah 1\n# blah 2\n\n", result.comments
    assert_equal "# blah 3\n", result.defn.comments
  end

  def test_module_comments
    rb = "# blah 1\n  \n  # blah 2\n\nmodule X\n  # blah 3\n  def blah\n    # blah 4\n  end\nend"
    pt = s(:module, :X,
           s(:defn, :blah, s(:args), s(:nil)))

    assert_parse rb, pt
    assert_equal "# blah 1\n\n# blah 2\n\n", result.comments
    assert_equal "# blah 3\n", result.defn.comments
  end

  def test_defn_comments
    rb = "# blah 1\n# blah 2\n\ndef blah\nend"
    pt = s(:defn, :blah, s(:args), s(:nil))

    assert_parse rb, pt
    assert_equal "# blah 1\n# blah 2\n\n", result.comments
  end

  def test_defs_comments
    rb = "# blah 1\n# blah 2\n\ndef self.blah\nend"
    pt = s(:defs, s(:self), :blah, s(:args))

    assert_parse rb, pt
    assert_equal "# blah 1\n# blah 2\n\n", result.comments
  end

  def test_do_bug # TODO: rename
    rb = "a 1\na.b do |c|\n  # do nothing\nend"
    pt = s(:block,
           s(:call, nil, :a, s(:lit, 1)),
           s(:iter,
             s(:call, s(:call, nil, :a), :b),
             s(:args, :c)))

    assert_parse rb, pt
  end

  def test_bug_begin_else
    rb = "begin 1; else; 2 end"
    pt = s(:block, s(:lit, 1), s(:lit, 2))

    assert_parse rb, pt
  end

  def test_bug_comment_eq_begin
    rb = "\n\n#\n=begin\nblah\n=end\n\n"
    pt = nil
    exp = rb.strip + "\n"

    assert_parse rb, pt
    assert_equal exp, processor.lexer.comments
  end

  def test_bug_call_arglist_parens
    rb = 'g ( 1), 2'
    pt = s(:call, nil, :g, s(:lit, 1), s(:lit, 2))

    assert_parse rb, pt

    rb = <<-CODE
      def f
        g ( 1), 2
      end
    CODE

    pt = s(:defn, :f, s(:args),
           s(:call, nil, :g, s(:lit, 1), s(:lit, 2)))

    assert_parse rb, pt

    rb = <<-CODE
      def f()
        g (1), 2
      end
    CODE

    assert_parse rb, pt
  end

  def test_dstr_evstr
    rb = "\"#\{'a'}#\{b}\""
    pt = s(:dstr, "a", s(:evstr, s(:call, nil, :b)))

    assert_parse rb, pt
  end

  def test_dstr_str
    rb = "\"#\{'a'} b\""
    pt = s(:str, "a b")

    assert_parse rb, pt
  end

  def test_empty
    rb = ""
    pt = nil

    assert_parse rb, pt
  end

  def test_evstr_evstr
    rb = "\"#\{a}#\{b}\""
    pt = s(:dstr, "", s(:evstr, s(:call, nil, :a)), s(:evstr, s(:call, nil, :b)))

    assert_parse rb, pt
  end

  def test_evstr_str
    rb = "\"#\{a} b\""
    pt = s(:dstr, "", s(:evstr, s(:call, nil, :a)), s(:str, " b"))

    assert_parse rb, pt
  end

  def test_lasgn_env
    rb = 'a = 42'
    pt = s(:lasgn, :a, s(:lit, 42))
    expected_env = { :a => :lvar }

    assert_parse rb, pt
    assert_equal expected_env, processor.env.all
  end

  def test_list_append
    a = s(:lit, 1)
    b = s(:lit, 2)
    c = s(:lit, 3)

    result = processor.list_append(s(:array, b.dup), c.dup)

    assert_equal s(:array, b, c), result

    result = processor.list_append(b.dup, c.dup)

    assert_equal s(:array, b, c), result

    result = processor.list_append(result, a.dup)

    assert_equal s(:array, b, c, a), result

    lhs, rhs = s(:array, s(:lit, :iter)), s(:when, s(:const, :BRANCHING), nil)
    expected = s(:array, s(:lit, :iter), s(:when, s(:const, :BRANCHING), nil))

    assert_equal expected, processor.list_append(lhs, rhs)
  end

  def test_list_prepend
    a = s(:lit, 1)
    b = s(:lit, 2)
    c = s(:lit, 3)

    result = processor.list_prepend(b.dup, s(:array, c.dup))

    assert_equal s(:array, b, c), result

    result = processor.list_prepend(b.dup, c.dup)

    assert_equal s(:array, b, c), result

    result = processor.list_prepend(a.dup, result)

    assert_equal s(:array, a, b, c), result
  end

  def test_literal_concat_dstr_dstr
    lhs      = s(:dstr, "Failed to download spec ",
                 s(:evstr, s(:call, nil, :spec_name)),
                 s(:str, " from "),
                 s(:evstr, s(:call, nil, :source_uri)),
                 s(:str, ":\n"))
    rhs      = s(:dstr, "\t",
                 s(:evstr, s(:call, s(:ivar, :@fetch_error), :message)))
    expected = s(:dstr, "Failed to download spec ",
                 s(:evstr, s(:call, nil, :spec_name)),
                 s(:str, " from "),
                 s(:evstr, s(:call, nil, :source_uri)),
                 s(:str, ":\n"),
                 s(:str, "\t"),
                 s(:evstr, s(:call, s(:ivar, :@fetch_error), :message)))

    assert_equal expected, processor.literal_concat(lhs, rhs)
  end

  def test_literal_concat_dstr_evstr
    lhs, rhs = s(:dstr, "a"), s(:evstr, s(:call, nil, :b))
    expected = s(:dstr, "a", s(:evstr, s(:call, nil, :b)))

    assert_equal expected, processor.literal_concat(lhs, rhs)
  end

  def test_literal_concat_evstr_evstr
    lhs, rhs = s(:evstr, s(:lit, 1)), s(:evstr, s(:lit, 2))
    expected = s(:dstr, "", s(:evstr, s(:lit, 1)), s(:evstr, s(:lit, 2)))

    assert_equal expected, processor.literal_concat(lhs, rhs)
  end

  def test_literal_concat_str_evstr
    lhs, rhs = s(:str, ""), s(:evstr, s(:str, "blah"))

    assert_equal s(:str, "blah"), processor.literal_concat(lhs, rhs)
  end

  def test_logop_12
    lhs = s(:lit, 1)
    rhs = s(:lit, 2)
    exp = s(:and, s(:lit, 1), s(:lit, 2))

    assert_equal exp, processor.logop(:and, lhs, rhs)
  end

  def test_logop_1234_5
    lhs = s(:and, s(:lit, 1), s(:and, s(:lit, 2), s(:and, s(:lit, 3), s(:lit, 4))))
    rhs = s(:lit, 5)
    exp = s(:and,
            s(:lit, 1),
            s(:and,
              s(:lit, 2),
              s(:and,
                s(:lit, 3),
                s(:and,
                  s(:lit, 4),
                  s(:lit, 5)))))

    assert_equal exp, processor.logop(:and, lhs, rhs)
  end

  def test_logop_123_4
    lhs = s(:and, s(:lit, 1), s(:and, s(:lit, 2), s(:lit, 3)))
    rhs = s(:lit, 4)
    exp = s(:and,
            s(:lit, 1),
            s(:and,
              s(:lit, 2),
              s(:and,
                s(:lit, 3),
                s(:lit, 4))))

    assert_equal exp, processor.logop(:and, lhs, rhs)
  end

  def test_logop_12_3
    lhs = s(:and, s(:lit, 1), s(:lit, 2))
    rhs = s(:lit, 3)
    exp = s(:and, s(:lit, 1), s(:and, s(:lit, 2), s(:lit, 3)))

    assert_equal exp, processor.logop(:and, lhs, rhs)
  end

  def test_logop_nested_mix
    lhs = s(:or, s(:call, nil, :a), s(:call, nil, :b))
    rhs = s(:and, s(:call, nil, :c), s(:call, nil, :d))
    exp = s(:or,
            s(:or, s(:call, nil, :a), s(:call, nil, :b)),
            s(:and, s(:call, nil, :c), s(:call, nil, :d)))

    lhs.paren = true
    rhs.paren = true

    assert_equal exp, processor.logop(:or, lhs, rhs)
  end

  def test_str_evstr
    rb = "\"a #\{b}\""
    pt = s(:dstr, "a ", s(:evstr, s(:call, nil, :b)))

    assert_parse rb, pt
  end

  def test_dsym_to_sym
    pt = s(:alias, s(:lit, :<<), s(:lit, :>>))

    rb = 'alias :<< :>>'
    assert_parse rb, pt

    rb = 'alias :"<<" :">>"'
    assert_parse rb, pt
  end

  def test_regexp
    regexps = {
      "/wtf/" => /wtf/,
      "/wtf/n" => /wtf/n,
      "/wtf/m" => /wtf/m,
      "/wtf/nm" => /wtf/nm,
      "/wtf/nmnmnmnm" => /wtf/nm,
    }

    regexps.each do |rb, lit|
      assert_parse rb, s(:lit, lit)
    end

    # TODO: add more including interpolation etc
  end

  def test_str_pct_Q_nested
    rb = "%Q[before [#\{nest}] after]"
    pt = s(:dstr, "before [", s(:evstr, s(:call, nil, :nest)), s(:str, "] after"))

    assert_parse rb, pt
  end

  # def test_str_pct_nested_nested
  #   rb = "%{ { #\{ \"#\{1}\" } } }"
  #   assert_equal " { 1 } ", eval(rb)
  #   pt = s(:dstr, " { ", s(:evstr, s(:lit, 1)), s(:str, " } "))
  #
  #   assert_parse rb, pt
  # end

  def test_str_str
    rb = "\"a #\{'b'}\""
    pt = s(:str, "a b")

    assert_parse rb, pt
  end

  def test_str_str_str
    rb = "\"a #\{'b'} c\""
    pt = s(:str, "a b c")

    assert_parse rb, pt
  end

  STARTING_LINE = {
    "case_no_expr"                       => 2, # TODO this should be 1
    "structure_unused_literal_wwtt"      => 3, # yes, 3... odd test
  }

  def after_process_hook klass, node, data, input_name, output_name
    expected = STARTING_LINE[node] || 1
    assert_equal expected, @result.line, "should have proper line number"
  end

  def test_parse_line_block
    rb = "a = 42\np a"
    pt = s(:block,
           s(:lasgn, :a, s(:lit, 42)),
           s(:call, nil, :p, s(:lvar, :a)))

    assert_parse_line rb, pt, 1
    assert_equal 1, result.lasgn.line, "lasgn should have line number"
    assert_equal 2, result.call.line,  "call should have line number"

    expected = "(string)"
    assert_equal expected, result.file
    assert_equal expected, result.lasgn.file
    assert_equal expected, result.call.file

    assert_same result.file, result.lasgn.file
    assert_same result.file, result.call.file
  end

  def test_parse_line_call_no_args
    rb = "f do |x, y|\n  x + y\nend"

    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, :x, :y),
           s(:call, s(:lvar, :x), :+, s(:lvar, :y)))

    assert_parse_line rb, pt, 1
    assert_equal 1, result[1].line,   "call should have line number"
    assert_equal 1, result[2].line,   "masgn should have line number"
    assert_equal 2, result[3].line,   "call should have line number"
  end

  def test_parse_line_defn_no_parens
    pt = s(:defn, :f, s(:args), s(:nil))

    rb = "def f\nend"
    assert_parse_line rb, pt, 1

    rb = "def f\n\nend"
    assert_parse_line rb, pt, 1
  end

  def test_parse_line_defn_complex
    rb = "def x(y)\n  p(y)\n  y *= 2\n  return y;\nend" # TODO: remove () & ;
    pt = s(:defn, :x, s(:args, :y),
           s(:call, nil, :p, s(:lvar, :y)),
           s(:lasgn, :y, s(:call, s(:lvar, :y), :*, s(:lit, 2))),
           s(:return, s(:lvar, :y)))

    assert_parse_line rb, pt, 1

    body = result
    assert_equal 2, body.call.line,   "call should have line number"
    assert_equal 3, body.lasgn.line,  "lasgn should have line number"
    assert_equal 4, body.return.line, "return should have line number"
  end

  def test_parse_line_iter_call_parens
    rb = "f(a) do |x, y|\n  x + y\nend"

    pt = s(:iter,
           s(:call, nil, :f, s(:call, nil, :a)),
           s(:args, :x, :y),
           s(:call, s(:lvar, :x), :+, s(:lvar, :y)))

    assert_parse_line rb, pt, 1

    assert_equal 1, result[1].line,   "call should have line number"
    assert_equal 1, result[2].line,   "masgn should have line number"
    assert_equal 2, result[3].line,   "call should have line number"
  end

  def test_parse_line_iter_call_no_parens
    rb = "f a do |x, y|\n  x + y\nend"

    pt = s(:iter,
           s(:call, nil, :f, s(:call, nil, :a)),
           s(:args, :x, :y),
           s(:call, s(:lvar, :x), :+, s(:lvar, :y)))

    assert_parse_line rb, pt, 1

    assert_equal 1, result[1].line,   "call should have line number"
    assert_equal 1, result[2].line,   "masgn should have line number"
    assert_equal 2, result[3].line,   "call should have line number"
  end

  def test_parse_line_heredoc
    rb = <<-CODE
      string = <<-HEREDOC
        very long string
      HEREDOC
      puts string
    CODE

    pt = s(:block,
           s(:lasgn, :string,
             s(:str, "        very long string\n").line(1)).line(1),
           s(:call, nil, :puts, s(:lvar, :string).line(4)).line(4)).line(1)

    assert_parse rb, pt
  end

  def test_parse_line_newlines
    rb = "true\n\n"
    pt = s(:true)

    assert_parse_line rb, pt, 1
  end

  def test_parse_line_return
    rb = <<-RUBY
      def blah
        if true then
          return 42
        end
      end
    RUBY

    pt = s(:defn, :blah, s(:args),
           s(:if, s(:true),
             s(:return, s(:lit, 42)),
             nil))

    assert_parse_line rb, pt, 1

    assert_equal 3, result.if.return.line
    assert_equal 3, result.if.return.lit.line
  end

  def test_bug_and
    rb = "true and []"
    pt = s(:and, s(:true), s(:array))

    assert_parse rb, pt

    rb = "true and\ntrue"
    pt = s(:and, s(:true), s(:true))

    assert_parse rb, pt
  end

  def test_bug_cond_pct
    rb = "case; when %r%blahblah%; end"
    pt = s(:case, nil, s(:when, s(:array, s(:lit, /blahblah/)), nil), nil)

    assert_parse rb, pt
  end

  # according to 2.3.1 parser -- added: ON 1.8 only:
  # rp.process("f { |(a,b),c| }") == rp.process("f { |((a,b),c)| }")

  # ruby18 -e "p lambda { |(a,b)|     }.arity" # =>  2
  # ruby19 -e "p lambda { |(a,b)|     }.arity" # =>  1
  # ruby18 -e "p lambda { |(a,b),c|   }.arity" # =>  2
  # ruby19 -e "p lambda { |(a,b),c|   }.arity" # =>  2
  # ruby18 -e "p lambda { |((a,b),c)| }.arity" # =>  2
  # ruby19 -e "p lambda { |((a,b),c)| }.arity" # =>  1

  def test_bug_args_masgn
    rb = "f { |(a, b), c| }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, s(:masgn, :a, :b), :c))

    assert_parse rb, pt.dup
  end

  def test_bug_args_masgn2
    rb = "f { |((a, b), c), d| }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, s(:masgn, s(:masgn, :a, :b), :c), :d))

    assert_parse rb, pt
  end

  def ruby18
    Ruby18Parser === self.processor
  end

  def ruby19
    Ruby19Parser === self.processor
  end

  def test_bug_comma
    val = if ruby18 then
            s(:lit, 100)
          elsif ruby19 then
            s(:str, "d")
          else
            raise "wtf"
          end

    rb = "if test ?d, dir then end"
    pt = s(:if,
           s(:call, nil, :test, val, s(:call, nil, :dir)),
           nil,
           nil)

    assert_parse rb, pt
  end

  def test_bug_case_when_regexp
    rb = "case :x; when /x/ then end"
    pt = s(:case, s(:lit, :x),
           s(:when, s(:array, s(:lit, /x/)), nil),
           nil)

    assert_parse rb, pt
  end

  def test_bug_masgn_right
    rb = "f { |a, (b, c)| }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, :a, s(:masgn, :b, :c)))

    assert_parse rb, pt
  end

  def test_when_splat
    rb = "case a; when *b then; end"
    pt = s(:case, s(:call, nil, :a),
           s(:when, s(:array, s(:splat, s(:call, nil, :b))), nil),
           nil)

    assert_parse rb, pt
  end

  def test_if_symbol
    rb = "if f :x; end"
    pt = s(:if, s(:call, nil, :f, s(:lit, :x)), nil, nil)

    assert_parse rb, pt
  end


  def test_bug_not_parens
    rb = "not(a)"
    pt = if ruby18 then
           s(:not, s(:call, nil, :a))
         elsif ruby19 then
           s(:call, s(:call, nil, :a), :"!")
         else
           raise "wtf"
         end

    assert_parse rb, pt
  end

  def test_pipe_space
    rb = "a.b do | | end"
    pt = s(:iter, s(:call, s(:call, nil, :a), :b), 0)

    assert_parse rb, pt
  end

  def test_cond_unary_minus
    rb = "if -1; end"
    pt = s(:if, s(:lit, -1), nil, nil)

    assert_parse rb, pt
  end

  def test_bug_op_asgn_rescue
    rb = "a ||= b rescue nil"
    pt = s(:rescue,
           s(:op_asgn_or, s(:lvar, :a), s(:lasgn, :a, s(:call, nil, :b))),
           s(:resbody, s(:array), s(:nil)))

    assert_parse rb, pt
  end

  def test_magic_encoding_comment
    rb = "# encoding: utf-8\nclass ExampleUTF8ClassNameVarietà; def self.è; così = :però; end\nend\n"

    rb.force_encoding "ASCII-8BIT" if rb.respond_to? :force_encoding

    # TODO: class vars
    # TODO: odd-ternary: a ?bb : c
    # TODO: globals

    pt = s(:class, :"ExampleUTF8ClassNameVariet\303\240", nil,
           s(:defs, s(:self), :"\303\250", s(:args),
             s(:lasgn, :"cos\303\254", s(:lit, :"per\303\262"))))

    err = RUBY_VERSION =~ /^1\.8/ ? "Skipping magic encoding comment\n" : ""

    assert_output "", err do
      assert_parse rb, pt
    end
  end

  def test_iter_args_1
    rb = "f { |a,b| }"
    pt = s(:iter, s(:call, nil, :f), s(:args, :a, :b))

    assert_parse rb, pt
  end

  def test_iter_args_3
    rb = "f { |a, (b, c), d| }"
    pt = s(:iter, s(:call, nil, :f), s(:args, :a, s(:masgn, :b, :c), :d))

    assert_parse rb, pt
  end

  def test_str_heredoc_interp
    rb = "<<\"\"\n\#{x}\nblah2\n\n"
    pt = s(:dstr, "", s(:evstr, s(:call, nil, :x)), s(:str, "\nblah2\n"))

    assert_parse rb, pt
  end

  def test_i_fucking_hate_line_numbers
    rb = <<-END.gsub(/^ {6}/, '')
      if true
        p 1
        a.b 2
        c.d 3, 4
        e.f 5
        g.h 6, 7
        p(1)
        a.b(2)
        c.d(3, 4)
        e.f(5)
        g.h(6, 7)
      end
    END

    pt = s(:if, s(:true).line(1),
           s(:block,
             s(:call, nil, :p, s(:lit, 1).line(2)).line(2),
             s(:call, s(:call, nil, :a).line(3), :b,
               s(:lit, 2).line(3)).line(3),
             s(:call, s(:call, nil, :c).line(4), :d,
               s(:lit, 3).line(4), s(:lit, 4).line(4)).line(4),
             s(:call, s(:call, nil, :e).line(5), :f,
               s(:lit, 5).line(5)).line(5),
             s(:call, s(:call, nil, :g).line(6), :h,
               s(:lit, 6).line(6), s(:lit, 7).line(6)).line(6),
             s(:call, nil, :p, s(:lit, 1).line(7)).line(7),
             s(:call, s(:call, nil, :a).line(8), :b,
               s(:lit, 2).line(8)).line(8),
             s(:call, s(:call, nil, :c).line(9), :d,
               s(:lit, 3).line(9), s(:lit, 4).line(9)).line(9),
             s(:call, s(:call, nil, :e).line(10), :f,
               s(:lit, 5).line(10)).line(10),
             s(:call, s(:call, nil, :g).line(11), :h,
               s(:lit, 6).line(11), s(:lit, 7).line(11)).line(11)).line(2),
           nil).line(1)

    assert_parse rb, pt
  end

  def test_i_fucking_hate_line_numbers2
    rb = <<-EOM.gsub(/^ {6}/, '')
      if true then
        p('a')
        b = 1
        p b
        c =1
      end
      a
    EOM

    pt = s(:block,
           s(:if, s(:true).line(1),
             s(:block,
               s(:call, nil, :p, s(:str, "a").line(2)).line(2),
               s(:lasgn, :b, s(:lit, 1).line(3)).line(3),
               s(:call, nil, :p, s(:lvar, :b).line(4)).line(4),
               s(:lasgn, :c, s(:lit, 1).line(5)).line(5)).line(2), # TODO line 2?
             nil).line(1),
           s(:call, nil, :a).line(7)).line(1)

    assert_parse rb, pt
  end

  def test_parse_comments
    p = RubyParser.new
    sexp = p.parse <<-CODE
      # class comment
      class Inline
        def show
          # woot
        end

        # Returns a list of things
        def list
          # woot
        end
      end
      CODE

    assert_equal "# class comment\n", sexp.comments
    act = sexp.find_nodes(:defn).map(&:comments)
    exp = ["", "# Returns a list of things\n"]

    assert_equal exp, act
    assert_equal [], processor.comments
    assert_equal "", processor.lexer.comments
  end
end

class TestRubyParser < MiniTest::Unit::TestCase
  def test_parse
    processor = RubyParser.new

    # 1.8 only syntax
    rb = "while false : 42 end"
    pt = s(:while, s(:false), s(:lit, 42), true)

    assert_silent do
      assert_equal pt, processor.parse(rb)
    end

    # 1.9 only syntax
    rb = "a.()"
    pt = s(:call, s(:call, nil, :a), :call)

    assert_equal pt, processor.parse(rb)

    # bad syntax
    e = assert_raises Racc::ParseError do
      capture_io do
        processor.parse "a.("
      end
    end

    msg = "(string):1 :: parse error on value \"(\" (tLPAREN2)"
    assert_equal msg, e.message.strip
  end
end

class TestRuby18Parser < RubyParserTestCase
  include TestRubyParserShared

  def setup
    super

    self.processor = Ruby18Parser.new
  end

  def test_flip2_env_lvar
    rb = "if a..b then end"
    pt = s(:if, s(:flip2, s(:call, nil, :a), s(:call, nil, :b)), nil, nil)

    assert_parse rb, pt

    top_env = processor.env.env.first

    assert_kind_of Hash, top_env

    flip = top_env.find { |k,v| k =~ /^flip/ }

    assert flip
    assert_equal :lvar, flip.last
  end

  def test_assoc_list_18
    rb = "{1, 2, 3, 4}"
    pt = s(:hash, s(:lit, 1), s(:lit, 2), s(:lit, 3), s(:lit, 4))

    assert_parse rb, pt
  end

  def test_case_then_colon_18
    rb = "case x; when Fixnum: 42; end"
    pt = s(:case,
           s(:call, nil, :x),
           s(:when, s(:array, s(:const, :Fixnum)), s(:lit, 42)),
           nil)

    assert_parse rb, pt
  end

  def test_do_colon_18
    rb = "while false : 42 end"
    pt = s(:while, s(:false), s(:lit, 42), true)

    assert_parse rb, pt
  end

  def test_parse_until_not_canonical
    rb = "until not var.nil?\n  'foo'\nend"

    pt = s(:while,
           s(:call, s(:call, nil, :var), :nil?),
           s(:str, "foo"), true)

    assert_parse rb, pt
  end

  def test_parse_until_not_noncanonical
    rb = "until not var.nil?\n  'foo'\nend"
    pt = s(:until,
           s(:not, s(:call, s(:call, nil, :var), :nil?)),
           s(:str, "foo"), true)

    processor.canonicalize_conditions = false

    assert_parse rb, pt
  end

  def test_parse_if_not_canonical
    rb = "if not var.nil? then 'foo' else 'bar'\nend"
    pt = s(:if,
           s(:call, s(:call, nil, :var), :nil?),
           s(:str, "bar"),
           s(:str, "foo"))

    assert_parse rb, pt
  end

  def test_parse_if_not_noncanonical
    rb = "if not var.nil? then 'foo' else 'bar'\nend"
    pt = s(:if,
           s(:not, s(:call, s(:call, nil, :var), :nil?)),
           s(:str, "foo"),
           s(:str, "bar"))

    processor.canonicalize_conditions = false

    assert_parse rb, pt
  end

  def test_parse_while_not_canonical
    rb = "while not var.nil?\n  'foo'\nend"
    pt = s(:until,
           s(:call, s(:call, nil, :var), :nil?),
           s(:str, "foo"), true)

    assert_parse rb, pt
  end

  def test_parse_while_not_noncanonical
    rb = "while not var.nil?\n  'foo'\nend"
    pt = s(:while,
           s(:not, s(:call, s(:call, nil, :var), :nil?)),
           s(:str, "foo"), true)

    processor.canonicalize_conditions = false

    assert_parse rb, pt
  end

  def test_double_block_error_10
    assert_syntax_error "a.b (&b) {}", BLOCK_DUP_MSG
  end

  def test_double_block_error_11
    assert_syntax_error "a (1, &b) { }", BLOCK_DUP_MSG
  end

  def test_double_block_error_12
    assert_syntax_error "a (1, &b) do end", BLOCK_DUP_MSG
  end

  def test_double_block_error_13
    assert_syntax_error "m.a (1, &b) { }", BLOCK_DUP_MSG
  end

  def test_double_block_error_14
    assert_syntax_error "m.a (1, &b) do end", BLOCK_DUP_MSG
  end

  def test_double_block_error_15
    assert_syntax_error "m::a (1, &b) { }", BLOCK_DUP_MSG
  end

  def test_double_block_error_16
    assert_syntax_error "m::a (1, &b) do end", BLOCK_DUP_MSG
  end

  # In 1.8, block args with an outer set of parens are superfluous.
  # In 1.9, outer set of parens are NOT... they are an explicit extra masgn.

  def test_iter_args_2_18
    rb = "f { |(a, b)| }"
    pt = s(:iter, s(:call, nil, :f), s(:args, :a, :b))

    assert_parse rb, pt
  end

  def test_bug_args__18
    rb = "f { |(a, b)| }"
    pt = s(:iter, s(:call, nil, :f),
           s(:args, :a, :b))

    assert_parse rb, pt
  end

  def test_bug_args_masgn_outer_parens__18
    rb = "f { |((a, b), c)| }"
    pt = s(:iter,               # NOTE: same sexp as test_bug_args_masgn
           s(:call, nil, :f),
           s(:args, s(:masgn, :a, :b), :c))

    assert_parse rb, pt.dup
  end
end

class TestRuby19Parser < RubyParserTestCase
  include TestRubyParserShared

  def setup
    super

    self.processor = Ruby19Parser.new
  end

  def test_mlhs_back_splat
    rb = "a, b, c, *s = f"
    pt = s(:masgn,
           s(:array,
             s(:lasgn, :a), s(:lasgn, :b), s(:lasgn, :c),
             s(:splat, s(:lasgn, :s))),
           s(:to_ary, s(:call, nil, :f)))

    assert_parse rb, pt
  end

  def test_mlhs_back_anonsplat
    rb = "a, b, c, * = f"
    pt = s(:masgn,
           s(:array,
             s(:lasgn, :a), s(:lasgn, :b), s(:lasgn, :c),
             s(:splat)),
           s(:to_ary, s(:call, nil, :f)))

    assert_parse rb, pt
  end

  def test_mlhs_mid_splat
    rb = "a, b, c, *s, x, y, z = f"
    pt = s(:masgn,
           s(:array,
             s(:lasgn, :a), s(:lasgn, :b), s(:lasgn, :c),
             s(:splat, s(:lasgn, :s)),
             s(:lasgn, :x), s(:lasgn, :y), s(:lasgn, :z)),
           s(:to_ary, s(:call, nil, :f)))

    assert_parse rb, pt
  end

  def test_mlhs_mid_anonsplat
    rb = "a, b, c, *, x, y, z = f"
    pt = s(:masgn,
           s(:array, s(:lasgn, :a), s(:splat), s(:lasgn, :z)),
           s(:to_ary, s(:call, nil, :f)))
    pt = s(:masgn,
           s(:array,
             s(:lasgn, :a), s(:lasgn, :b), s(:lasgn, :c),
             s(:splat),
             s(:lasgn, :x), s(:lasgn, :y), s(:lasgn, :z)),
           s(:to_ary, s(:call, nil, :f)))

    assert_parse rb, pt
  end

  def test_mlhs_front_splat
    rb = "*s, x, y, z = f"
    pt = s(:masgn,
           s(:array, s(:splat, s(:lasgn, :s)), s(:lasgn, :z)),
           s(:to_ary, s(:call, nil, :f)))
    pt = s(:masgn,
           s(:array,
             s(:splat, s(:lasgn, :s)),
             s(:lasgn, :x), s(:lasgn, :y), s(:lasgn, :z)),
           s(:to_ary, s(:call, nil, :f)))

    assert_parse rb, pt
  end

  def test_mlhs_front_anonsplat
    rb = "*, x, y, z = f"
    pt = s(:masgn,
           s(:array,
             s(:splat),
             s(:lasgn, :x), s(:lasgn, :y), s(:lasgn, :z)),
           s(:to_ary, s(:call, nil, :f)))

    assert_parse rb, pt
  end

  def test_expr_not_bang
    rb = "! a b"
    pt = s(:call, s(:call, nil, :a, s(:call, nil, :b)), :"!")

    assert_parse rb, pt
  end

  def test_encoding
    rb = '__ENCODING__'
    pt = if defined? Encoding then
           s(:const, Encoding::UTF_8)
         else
           s(:str, "Unsupported!")
         end

    assert_parse rb, pt
  end

  def test_do_colon_19
    rb = "while false : 42 end"

    assert_parse_error rb, "(string):1 :: parse error on value \":\" (tCOLON)"
  end

  def test_assoc_list_19
    rb = "{1, 2, 3, 4}"

    assert_parse_error rb, "(string):1 :: parse error on value \",\" (tCOMMA)"
  end

  def test_case_then_colon_19
    rb = <<-EOM
      case x
      when Fixnum : # need the space to not hit new hash arg syntax
        42
      end
    EOM

    assert_parse_error rb, "(string):2 :: parse error on value \":\" (tCOLON)"
  end

  def test_parse_def_xxx1
    rb = 'def f(a, *b, c = nil) end'

    assert_parse_error rb, '(string):1 :: parse error on value "=" (tEQL)'
  end

  def test_parse_def_xxx2
    rb = 'def f(a = nil, *b, c = nil) end'

    assert_parse_error rb, '(string):1 :: parse error on value "=" (tEQL)'
  end

  def test_parse_until_not_canonical
    rb = "until not var.nil?\n  'foo'\nend"
    pt = s(:until,
           s(:call, s(:call, s(:call, nil, :var), :nil?), :"!"),
           s(:str, "foo"), true)

    assert_parse rb, pt
  end

  def test_parse_until_not_noncanonical
    rb = "until not var.nil?\n  'foo'\nend"
    pt = s(:until,
           s(:call, s(:call, s(:call, nil, :var), :nil?), :"!"),
           s(:str, "foo"), true)

    processor.canonicalize_conditions = false

    assert_parse rb, pt
  end

  def test_parse_if_not_canonical
    rb = "if not var.nil? then 'foo' else 'bar'\nend"
    pt = s(:if,
           s(:call, s(:call, s(:call, nil, :var), :nil?), :"!"),
           s(:str, "foo"),
           s(:str, "bar"))

    assert_parse rb, pt
  end

  def test_parse_if_not_noncanonical
    rb = "if not var.nil? then 'foo' else 'bar'\nend"
    pt = s(:if,
           s(:call, s(:call, s(:call, nil, :var), :nil?), :"!"),
           s(:str, "foo"),
           s(:str, "bar"))

    processor.canonicalize_conditions = false

    assert_parse rb, pt
  end

  def test_parse_while_not_canonical
    rb = "while not var.nil?\n  'foo'\nend"
    pt = s(:while,
           s(:call, s(:call, s(:call, nil, :var), :nil?), :"!"),
           s(:str, "foo"), true)

    assert_parse rb, pt
  end

  def test_parse_while_not_noncanonical
    rb = "while not var.nil?\n  'foo'\nend"
    pt = s(:while,
           s(:call, s(:call, s(:call, nil, :var), :nil?), :"!"),
           s(:str, "foo"), true)

    processor.canonicalize_conditions = false

    assert_parse rb, pt
  end

  def test_parse_opt_call_args_assocs_comma
    rb = "1[2=>3,]"
    pt = s(:call, s(:lit, 1), :[], s(:lit, 2), s(:lit, 3))

    assert_parse rb, pt
  end

  def test_bug_hash_args
    rb = "foo(:bar, baz: nil)"
    pt = s(:call, nil, :foo,
           s(:lit, :bar),
           s(:hash, s(:lit, :baz), s(:nil)))

    assert_parse rb, pt
  end

  def test_bug_hash_args_trailing_comma
    rb = "foo(:bar, baz: nil,)"
    pt = s(:call, nil, :foo,    # NOTE: same sexp as test_bug_hash_args
           s(:lit, :bar),
           s(:hash, s(:lit, :baz), s(:nil)))

    assert_parse rb, pt
  end

  def test_block_arg_optional
    rb = "a { |b = 1| }"
    pt = s(:iter,
           s(:call, nil, :a),
           s(:args, s(:lasgn, :b, s(:lit, 1))))

    assert_parse rb, pt
  end

  def test_zomg_sometimes_i_hate_this_project
    rb = <<-RUBY
      {
        a: lambda { b ? c() : d },
        e: nil,
      }
    RUBY

    pt = s(:hash,
           s(:lit, :a),
           s(:iter,
             s(:call, nil, :lambda),
             s(:args),
             s(:if, s(:call, nil, :b), s(:call, nil, :c), s(:call, nil, :d))),

           s(:lit, :e),
           s(:nil))

    assert_parse rb, pt
  end

  # def test_pipe_semicolon # HACK
  #   rb = "a.b do | ; c | end"
  #   pt = s(:iter, s(:call, s(:call, nil, :a), :b), 0)
  #
  #   assert_parse rb, pt
  # end

  def test_wtf
    # lambda -> f_larglist lambda_body
    # f_larglist -> f_args opt_bv_decl
    # opt_bv_decl
    # bv_decls
    # bvar

    rb = "->(a, b=nil) { p [a, b] }"
    pt = s(:iter,
           s(:call, nil, :lambda),
           s(:args, :a, s(:lasgn, :b, s(:nil))),
           s(:call, nil, :p, s(:array, s(:lvar, :a), s(:lvar, :b))))

    assert_parse rb, pt

    # rb = "->(a; b) { p [a, b] }"
    #
    # assert_parse rb, pt
  end

  def test_block_args_opt1
    rb = "f { |a, b = 42| [a, b] }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, :a, s(:lasgn, :b, s(:lit, 42))),
           s(:array, s(:lvar, :a), s(:lvar, :b)))

    assert_parse rb, pt
  end

  def test_block_args_opt2
    rb = "f { |a, b = 42, c = 24| [a, b, c] }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, :a, s(:lasgn, :b, s(:lit, 42)), s(:lasgn, :c, s(:lit, 24))),
           s(:array, s(:lvar, :a), s(:lvar, :b), s(:lvar, :c)))

    assert_parse rb, pt
  end

  def test_block_args_opt3
    rb = "f { |a, b = 42, c = 24, &d| [a, b, c, d] }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, :a, s(:lasgn, :b, s(:lit, 42)), s(:lasgn, :c, s(:lit, 24)), :"&d"),
           s(:array, s(:lvar, :a), s(:lvar, :b), s(:lvar, :c), s(:lvar, :d)))

    assert_parse rb, pt
  end

  def test_i_have_no_freakin_clue
    rb = "1 ? b('') : 2\na d: 3"
    pt = s(:block,
           s(:if, s(:lit, 1), s(:call, nil, :b, s(:str, "")), s(:lit, 2)),
           s(:call, nil, :a, s(:hash, s(:lit, :d), s(:lit, 3))))

    assert_parse rb, pt
  end

  def test_motherfuckin_leading_dots
    rb = "a\n.b"
    pt = s(:call, s(:call, nil, :a), :b)

    assert_parse rb, pt
  end

  def test_motherfuckin_leading_dots2
    rb = "a\n..b"

    assert_parse_error rb, '(string):2 :: parse error on value ".." (tDOT2)'
  end

  def test_kill_me
    rb = "f { |a, (b, *c)| }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, :a, s(:masgn, :b, :"*c")))

    assert_parse rb, pt
  end

  def test_kill_me2
    rb = "f { |*a, b| }"
    pt = s(:iter, s(:call, nil, :f), s(:args, :"*a", :b))

    assert_parse rb, pt
  end

  def test_kill_me3
    rb = "f { |*a, b, &c| }"
    pt = s(:iter, s(:call, nil, :f), s(:args, :"*a", :b, :"&c"))

    assert_parse rb, pt
  end

  def test_kill_me4
    rb = "a=b ? true: false"
    pt = s(:lasgn, :a, s(:if, s(:call, nil, :b), s(:true), s(:false)))

    assert_parse rb, pt
  end

  # def test_kill_me5
  #   rb = "f ->() { g do end }"
  #   pt = 42
  #
  #   assert_parse rb, pt
  # end

  def test_iter_args_4
    rb = "f { |a, *b, c| }"
    pt = s(:iter, s(:call, nil, :f), s(:args, :a, :"*b", :c))

    assert_parse rb, pt
  end

  def test_iter_args_5
    rb = "f { |a, &b| }"
    pt = s(:iter, s(:call, nil, :f), s(:args, :a, :"&b"))

    assert_parse rb, pt
  end

  def test_iter_args_6
    rb = "f { |a, b=42, c| }"
    pt = s(:iter, s(:call, nil, :f), s(:args, :a, s(:lasgn, :b, s(:lit, 42)), :c))

    assert_parse rb, pt
  end

  # In 1.8, block args with an outer set of parens are superfluous.
  # In 1.9, outer set of parens are NOT... they are an explicit extra masgn.

  def test_iter_args_2__19
    rb = "f { |(a, b)| }"
    pt = s(:iter, s(:call, nil, :f), s(:args, s(:masgn, :a, :b)))

    assert_parse rb, pt
  end

  def test_bug_args__19
    rb = "f { |(a, b)| d }"
    pt = s(:iter, s(:call, nil, :f),
           s(:args, s(:masgn, :a, :b)),
           s(:call, nil, :d))

    assert_parse rb, pt
  end

  def test_bug_args_masgn_outer_parens__19
    rb = "f { |((k, v), i)| }"
    pt = s(:iter,               # NOTE: same sexp as test_bug_args_masgn
           s(:call, nil, :f),
           s(:args, s(:masgn, s(:masgn, :k, :v), :i)))

    assert_parse rb, pt.dup
  end

  def test_iter_args_7_1
    rb = "f { |a = 42, *b| }"
    pt = s(:iter, s(:call, nil, :f),
           s(:args, s(:lasgn, :a, s(:lit, 42)), :"*b"))

    assert_parse rb, pt
  end

  def test_iter_args_7_2
    rb = "f { |a = 42, *b, &c| }"
    pt = s(:iter, s(:call, nil, :f),
           s(:args, s(:lasgn, :a, s(:lit, 42)), :"*b", :"&c"))

    assert_parse rb, pt
  end

  def test_iter_args_8_1
    rb = "f { |a = 42, *b, c| }"
    pt = s(:iter, s(:call, nil, :f),
           s(:args, s(:lasgn, :a, s(:lit, 42)), :"*b", :c))

    assert_parse rb, pt
  end

  def test_iter_args_8_2
    rb = "f { |a = 42, *b, c, &d| }"
    pt = s(:iter, s(:call, nil, :f),
           s(:args, s(:lasgn, :a, s(:lit, 42)), :"*b", :c, :"&d"))

    assert_parse rb, pt
  end

  def test_iter_args_9_1
    rb = "f { |a = 42, b| }"
    pt = s(:iter, s(:call, nil, :f),
           s(:args, s(:lasgn, :a, s(:lit, 42)), :b))

    assert_parse rb, pt
  end

  def test_iter_args_9_2
    rb = "f { |a = 42, b, &c| }"
    pt = s(:iter, s(:call, nil, :f),
           s(:args, s(:lasgn, :a, s(:lit, 42)), :b, :"&c"))

    assert_parse rb, pt
  end

  def test_iter_args_10_1
    rb = "f { |a, b = 42, *c| }"
    pt = s(:iter, s(:call, nil, :f),
           s(:args, :a, s(:lasgn, :b, s(:lit, 42)), :"*c"))

    assert_parse rb, pt
  end

  def test_iter_args_10_2
    rb = "f { |a, b = 42, *c, &d| }"
    pt = s(:iter, s(:call, nil, :f),
           s(:args, :a, s(:lasgn, :b, s(:lit, 42)), :"*c", :"&d"))

    assert_parse rb, pt
  end

  def test_iter_args_11_1
    rb = "f { |a, b = 42, *c, d| }"
    pt = s(:iter, s(:call, nil, :f),
           s(:args, :a, s(:lasgn, :b, s(:lit, 42)), :"*c", :d))

    assert_parse rb, pt
  end

  def test_iter_args_11_2
    rb = "f { |a, b = 42, *c, d, &e| }"
    pt = s(:iter, s(:call, nil, :f),
           s(:args, :a, s(:lasgn, :b, s(:lit, 42)), :"*c", :d, :"&e"))

    assert_parse rb, pt
  end

  def test_kill_me_6
    # | f_marg_list tCOMMA tSTAR f_norm_arg tCOMMA f_marg_list
    rb = "f { |a, (b, *c, d)| }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, :a, s(:masgn, :b, :"*c", :d)))

    assert_parse rb, pt
  end

  def test_kill_me_7
    # | f_marg_list tCOMMA tSTAR
    rb = "f { |a, (b, *)| }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, :a, s(:masgn, :b, :*)))

    assert_parse rb, pt
  end

  def test_kill_me_8
    # | f_marg_list tCOMMA tSTAR tCOMMA f_marg_list
    rb = "f { |a, (b, *, c)| }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, :a, s(:masgn, :b, :*, :c)))

    assert_parse rb, pt
  end

  def test_kill_me_9
    # | tSTAR f_norm_arg
    rb = "f { |a, (*b)| }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, :a, s(:masgn, :"*b")))

    assert_parse rb, pt
  end

  def test_kill_me_10
    # | tSTAR f_norm_arg tCOMMA f_marg_list
    rb = "f { |a, (*b, c)| }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, :a, s(:masgn, :"*b", :c)))

    assert_parse rb, pt
  end

  def test_kill_me_11
    # | tSTAR
    rb = "f { |a, (*)| }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, :a, s(:masgn, :*)))

    assert_parse rb, pt
  end

  def test_kill_me_12
    # | tSTAR tCOMMA f_marg_list
    rb = "f { |a, (*, b)| }"
    pt = s(:iter,
           s(:call, nil, :f),
           s(:args, :a, s(:masgn, :*, :b)))

    assert_parse rb, pt
  end

  def test_index_0
    rb = "a[] = b"
    pt = s(:attrasgn, s(:call, nil, :a), :[]=, s(:call, nil, :b))

    assert_parse rb, pt
  end

  def test_lambda_do_vs_brace
    pt = s(:call, nil, :f, s(:iter, s(:call, nil, :lambda), 0))

    rb = "f ->() {}"
    assert_parse rb, pt

    rb = "f ->() do end"
    assert_parse rb, pt
  end

  def test_thingy
    pt = s(:call, s(:call, nil, :f), :call, s(:lit, 42))

    rb = "f.(42)"
    assert_parse rb, pt

    rb = "f::(42)"
    assert_parse rb, pt
  end

  def test_unary_plus_on_literal
    rb = "+:a"
    pt = s(:call, s(:lit, :a), :+@)

    assert_parse rb, pt
  end
end
