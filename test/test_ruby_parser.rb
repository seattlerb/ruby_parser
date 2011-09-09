#!/usr/local/bin/ruby

ENV['VERBOSE'] = "1"

require 'rubygems'
require 'minitest/autorun'
require 'ruby_parser'

$: << File.expand_path('~/Work/p4/zss/src/ParseTree/dev/test')

require 'pt_testcase'

class RubyParser
  def process input
    parse input
  end
end

class RubyParserTestCase < ParseTreeTestCase
  def self.previous key
    "Ruby"
  end

  def self.generate_test klass, node, data, input_name, output_name
    return if node.to_s =~ /bmethod|dmethod/
    return if Array === data['Ruby']

    output_name = "ParseTree"

    super
  end
end

class TestRubyParser < RubyParserTestCase
  attr_accessor :result, :processor

  def setup
    super

    self.processor = RubyParser.new
  end

  def assert_parse rb, pt
    self.result = processor.parse rb
    assert_equal pt, result
  end

  def assert_parse_line rb, pt, line
    assert_parse rb, pt
    assert_equal line, result.line,   "call should have line number"
  end

  def test_attrasgn_array_lhs
    rb = '[1, 2, 3, 4][from .. to] = ["a", "b", "c"]'
    pt = s(:attrasgn,
           s(:array, s(:lit, 1), s(:lit, 2), s(:lit, 3), s(:lit, 4)),
           :[]=,
           s(:arglist,
             s(:dot2,
               s(:call, nil, :from, s(:arglist)),
               s(:call, nil, :to, s(:arglist))),
             s(:array, s(:str, "a"), s(:str, "b"), s(:str, "c"))))

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
    head = s(:call, nil, :f1, s(:arglist))
    tail = s(:block, s(:undef, s(:lit, :x)), s(:undef, s(:lit, :y)))
    expected = s(:block,
                 s(:call, nil, :f1, s(:arglist)),
                 s(:block, s(:undef, s(:lit, :x)), s(:undef, s(:lit, :y))))
    assert_equal expected, processor.block_append(head, tail)
  end

  def test_call_env
    processor.env[:a] = :lvar
    rb = "a.happy"
    pt = s(:call, s(:lvar, :a), :happy, s(:arglist))

    assert_parse rb, pt
  end

  def test_dasgn_icky2
    rb = "a do\n  v = nil\n  begin\n    yield\n  rescue Exception => v\n    break\n  end\nend"
    pt = s(:iter,
           s(:call, nil, :a, s(:arglist)),
           nil,
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
           s(:scope,
             s(:defn, :blah, s(:args), s(:scope, s(:block, s(:nil))))))

    assert_parse rb, pt

    assert_equal "# blah 1\n# blah 2\n\n", result.comments
    assert_equal "# blah 3\n", result.scope.defn.comments
  end

  def test_module_comments
    rb = "# blah 1\n  \n  # blah 2\n\nmodule X\n  # blah 3\n  def blah\n    # blah 4\n  end\nend"
    pt = s(:module, :X,
           s(:scope,
             s(:defn, :blah, s(:args), s(:scope, s(:block, s(:nil))))))

    assert_parse rb, pt
    assert_equal "# blah 1\n\n# blah 2\n\n", result.comments
    assert_equal "# blah 3\n", result.scope.defn.comments
  end

  def test_defn_comments
    rb = "# blah 1\n# blah 2\n\ndef blah\nend"
    pt = s(:defn, :blah, s(:args), s(:scope, s(:block, s(:nil))))

    assert_parse rb, pt
    assert_equal "# blah 1\n# blah 2\n\n", result.comments
  end

  def test_defs_comments
    rb = "# blah 1\n# blah 2\n\ndef self.blah\nend"
    pt = s(:defs, s(:self), :blah, s(:args), s(:scope, s(:block)))

    assert_parse rb, pt
    assert_equal "# blah 1\n# blah 2\n\n", result.comments
  end

  def test_do_bug # TODO: rename
    rb = "a 1\na.b do |c|\n  # do nothing\nend"
    pt = s(:block,
           s(:call, nil, :a, s(:arglist, s(:lit, 1))),
           s(:iter,
             s(:call, s(:call, nil, :a, s(:arglist)), :b, s(:arglist)),
             s(:lasgn, :c)))

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
    pt = s(:call, nil, :g, s(:arglist, s(:lit, 1), s(:lit, 2)))

    assert_parse rb, pt

    rb = <<-CODE
      def f
        g ( 1), 2
      end
    CODE

    pt = s(:defn, :f, s(:args),
           s(:scope,
             s(:block,
               s(:call, nil, :g,
                 s(:arglist,
                   s(:lit, 1), s(:lit, 2))))))

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
    pt = s(:dstr, "a", s(:evstr, s(:call, nil, :b, s(:arglist))))

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
    pt = s(:dstr, "", s(:evstr, s(:call, nil, :a, s(:arglist))), s(:evstr, s(:call, nil, :b, s(:arglist))))

    assert_parse rb, pt
  end

  def test_evstr_str
    rb = "\"#\{a} b\""
    pt = s(:dstr, "", s(:evstr, s(:call, nil, :a, s(:arglist))), s(:str, " b"))

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
                 s(:evstr, s(:call, nil, :spec_name, s(:arglist))),
                 s(:str, " from "),
                 s(:evstr, s(:call, nil, :source_uri, s(:arglist))),
                 s(:str, ":\n"))
    rhs      = s(:dstr, "\t",
                 s(:evstr, s(:call, s(:ivar, :@fetch_error), :message)))
    expected = s(:dstr, "Failed to download spec ",
                 s(:evstr, s(:call, nil, :spec_name, s(:arglist))),
                 s(:str, " from "),
                 s(:evstr, s(:call, nil, :source_uri, s(:arglist))),
                 s(:str, ":\n"),
                 s(:str, "\t"),
                 s(:evstr, s(:call, s(:ivar, :@fetch_error), :message)))

    assert_equal expected, processor.literal_concat(lhs, rhs)
  end

  def test_literal_concat_dstr_evstr
    lhs, rhs = s(:dstr, "a"), s(:evstr, s(:call, nil, :b, s(:arglist)))
    expected = s(:dstr, "a", s(:evstr, s(:call, nil, :b, s(:arglist))))

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
    lhs = s(:or, s(:call, nil, :a, s(:arglist)), s(:call, nil, :b, s(:arglist)))
    rhs = s(:and, s(:call, nil, :c, s(:arglist)), s(:call, nil, :d, s(:arglist)))
    exp = s(:or,
            s(:or, s(:call, nil, :a, s(:arglist)), s(:call, nil, :b, s(:arglist))),
            s(:and, s(:call, nil, :c, s(:arglist)), s(:call, nil, :d, s(:arglist))))

    lhs.paren = true
    rhs.paren = true

    assert_equal exp, processor.logop(:or, lhs, rhs)
  end

  def test_str_evstr
    rb = "\"a #\{b}\""
    pt = s(:dstr, "a ", s(:evstr, s(:call, nil, :b, s(:arglist))))

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
    pt = s(:dstr, "before [", s(:evstr, s(:call, nil, :nest, s(:arglist))), s(:str, "] after"))

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
    "case_nested_inner_no_expr"          => 2,
    "case_no_expr"                       => 2,
    "case_splat"                         => 2,
    "dstr_heredoc_expand"                => 1,
    "dstr_heredoc_windoze_sucks"         => 1,
    "dstr_heredoc_yet_again"             => 1,
    "str_heredoc"                        => 1,
    "str_heredoc_call"                   => 1,
    "str_heredoc_empty"                  => 1,
    "str_heredoc_indent"                 => 1,
    "structure_unused_literal_wwtt"      => 3, # yes, 3... odd test
    "undef_block_1"                      => 2,
    "undef_block_2"                      => 2,
    "undef_block_3"                      => 2,
    "undef_block_wtf"                    => 2,
  }

  def after_process_hook klass, node, data, input_name, output_name
    expected = STARTING_LINE[node] || 1
    assert_equal expected, @result.line, "should have proper line number"
  end

  def test_parse_line_block
    rb = "a = 42\np a"
    pt = s(:block,
           s(:lasgn, :a, s(:lit, 42)),
           s(:call, nil, :p, s(:arglist, s(:lvar, :a))))

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
           s(:call, nil, :f, s(:arglist)),
           s(:masgn, s(:array, s(:lasgn, :x), s(:lasgn, :y))),
           s(:call, s(:lvar, :x), :+, s(:arglist, s(:lvar, :y))))

    assert_parse_line rb, pt, 1
    assert_equal 1, result[1].line,   "call should have line number"
    assert_equal 1, result[2].line,   "masgn should have line number"
    assert_equal 2, result[3].line,   "call should have line number"
  end

  def test_parse_line_defn_no_parens
    pt = s(:defn, :f, s(:args), s(:scope, s(:block, s(:nil))))

    rb = "def f\nend"
    assert_parse_line rb, pt, 1

    rb = "def f\n\nend"
    assert_parse_line rb, pt, 1
  end

  def test_parse_line_defn_complex
    rb = "def x(y)\n  p(y)\n  y *= 2\n  return y;\nend" # TODO: remove () & ;
    pt = s(:defn, :x, s(:args, :y),
           s(:scope,
             s(:block,
               s(:call, nil, :p, s(:arglist, s(:lvar, :y))),
               s(:lasgn, :y,
                 s(:call, s(:lvar, :y), :*, s(:arglist, s(:lit, 2)))),
               s(:return, s(:lvar, :y)))))

    assert_parse_line rb, pt, 1

    body = result.scope.block
    assert_equal 2, body.call.line,   "call should have line number"
    assert_equal 3, body.lasgn.line,  "lasgn should have line number"
    assert_equal 4, body.return.line, "return should have line number"
  end

  def test_parse_line_iter_call_parens
    rb = "f(a) do |x, y|\n  x + y\nend"

    pt = s(:iter,
           s(:call, nil, :f, s(:arglist, s(:call, nil, :a, s(:arglist)))),
           s(:masgn, s(:array, s(:lasgn, :x), s(:lasgn, :y))),
           s(:call, s(:lvar, :x), :+, s(:arglist, s(:lvar, :y))))

    assert_parse_line rb, pt, 1

    assert_equal 1, result[1].line,   "call should have line number"
    assert_equal 1, result[2].line,   "masgn should have line number"
    assert_equal 2, result[3].line,   "call should have line number"
  end

  def test_parse_line_iter_call_no_parens
    rb = "f a do |x, y|\n  x + y\nend"

    pt = s(:iter,
           s(:call, nil, :f, s(:arglist, s(:call, nil, :a, s(:arglist)))),
           s(:masgn, s(:array, s(:lasgn, :x), s(:lasgn, :y))),
           s(:call, s(:lvar, :x), :+, s(:arglist, s(:lvar, :y))))

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

    result = processor.parse rb
    assert_equal 1, result.lasgn.line
    assert_equal 4, result.call.line
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
           s(:scope,
             s(:block,
               s(:if,
                 s(:true),
                 s(:return, s(:lit, 42)),
                 nil))))

    assert_parse_line rb, pt, 1

    assert_equal 3, result.scope.block.if.return.line
    assert_equal 3, result.scope.block.if.return.lit.line
  end

  def test_parse_if_not_canonical
    rb = "if not var.nil? then 'foo' else 'bar'\nend"
    pt = s(:if,
           s(:call, s(:call, nil, :var, s(:arglist)), :nil?, s(:arglist)),
           s(:str, "bar"),
           s(:str, "foo"))

    assert_parse rb, pt
  end

  def test_parse_if_not_noncanonical
    rb = "if not var.nil? then 'foo' else 'bar'\nend"
    pt = s(:if,
           s(:not,
             s(:call, s(:call, nil, :var, s(:arglist)), :nil?, s(:arglist))),
           s(:str, "foo"),
           s(:str, "bar"))

    processor.canonicalize_conditions = false

    assert_parse rb, pt
  end

  def test_parse_while_not_canonical
    rb = "while not var.nil?\n  'foo'\nend"
    pt = s(:until,
           s(:call, s(:call, nil, :var, s(:arglist)), :nil?, s(:arglist)),
           s(:str, "foo"), true)

    assert_parse rb, pt
  end

  def test_parse_while_not_noncanonical
    rb = "while not var.nil?\n  'foo'\nend"
    pt = s(:while,
           s(:not,
             s(:call, s(:call, nil, :var, s(:arglist)), :nil?, s(:arglist))),
           s(:str, "foo"), true)

    processor.canonicalize_conditions = false

    assert_parse rb, pt
  end

  def test_parse_until_not_canonical
    rb = "until not var.nil?\n  'foo'\nend"

    pt = s(:while,
           s(:call, s(:call, nil, :var, s(:arglist)), :nil?, s(:arglist)),
           s(:str, "foo"), true)

    assert_parse rb, pt
  end

  def test_parse_until_not_noncanonical
    rb = "until not var.nil?\n  'foo'\nend"
    pt = s(:until,
           s(:not,
             s(:call, s(:call, nil, :var, s(:arglist)), :nil?, s(:arglist))),
           s(:str, "foo"), true)

    processor.canonicalize_conditions = false

    assert_parse rb, pt
  end
end
