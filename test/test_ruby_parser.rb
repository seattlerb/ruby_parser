#!/usr/local/bin/ruby

require 'test/unit'
require 'ruby_parser'

$: << File.expand_path('~/Work/p4/zss/src/ParseTree/dev/lib')
$: << File.expand_path('~/Work/p4/zss/src/ParseTree/dev/test')

require 'pt_testcase'

class TestRubyParser < Test::Unit::TestCase # ParseTreeTestCase

  alias :refute_nil :assert_not_nil unless defined? Mini

  # Regular ParseTreeTestCase tests
  eval ParseTreeTestCase.testcases.map { |node, data|
    next if node.to_s =~ /bmethod|dmethod/
    next if Array === data['Ruby'] # runtime only crap
    "def test_#{node}
       rb = #{data['Ruby'].inspect}
       pt = #{data['ParseTree'].inspect}

       refute_nil rb, \"Ruby for #{node} undefined\"
       refute_nil pt, \"ParseTree for #{node} undefined\"

       assert_equal Sexp.from_array(pt), @processor.parse(rb)
     end"
  }.compact.join("\n")

if false then
  require 'parse_tree'

  # Regular ParseTreeTestCase tests
  eval ParseTreeTestCase.testcases.map { |node, data|
    next if node.to_s =~ /bmethod|dmethod/
    next if Array === data['Ruby'] # runtime only crap
    "def test_nl_#{node}
       rb = #{data['Ruby'].inspect}
       pt = ParseTree.new(true).parse_tree_for_string(rb).first

       refute_nil rb, \"Ruby for #{node} undefined\"
       refute_nil pt, \"ParseTree for #{node} undefined\"

       assert_equal Sexp.from_array(pt), @processor.parse(rb)
     end"
  }.compact.join("\n")
end

  # Scour the world and compare against ParseTree
  if ENV['ZOMGPONIES'] or File.exist? 'zomgponies' then
    require 'parse_tree'

    base = "/usr/lib/ruby"
    base = "unit"

    files = Dir[File.join(base, "**/*.rb")]

    # these files/patterns cause parse_tree_show to bus error (or just suck):
    files.reject! { |f| f =~ /environments.environment|rss.maker.base|rails_generator|ferret.browser|rubinius.spec.core.module.(constants|name|remove_const)_spec|tkextlib.tcllib.tablelist/ }

    # these are rejected for dasgn_curr ordering failures... I'll fix them later.
    # (or mri parse errors--I should have separated them out)
    files.reject! { |f| f =~ /lib.flog|lib.autotest.notify|lib.analyzer.tools.rails.stat|flog.lib.flog|rakelib.struct.generator|rubinius.kernel.core.array|lib.rbosa.rb|src.rbosa.rb|spec.spec.mocks.mock.spec.rb|dsl.shared.behaviour.spec.rb|spec.spec.dsl.behaviour.spec|lib.hpricot.parse.rb|resolve.rb|parsers.parse.f95|rubinius.shotgun.lib.primitives.ltm|rubinius.lib.bin.compile|rubinius.kernel.core.object|rubinius.kernel.core.file|rubinius.compiler2.garnet.bindingagent|ruby_to_c_test_r2ctestcase|lib.more.like.this|resolv\.rb|test.r2ctestcase/ }

    warn "Generating #{files.size} tests from #{base}"

    eval files.map { |file|
      name = file[base.size..-1].gsub(/\W+/, '_')

      loc = `wc -l #{file}`.strip.to_i

      "def test#{name}_#{loc}
         file = #{file.inspect}
         rb = File.read(file)

         pt = ParseTree.new.parse_tree_for_string rb
         refute_nil pt, \"ParseTree for #{name} undefined\"

         rp = @processor.parse rb
         assert_equal Sexp.from_array(pt).first, rp, \"RP different from PT\"
         File.unlink #{file.inspect}
       end"
    }.compact.join("\n")
  end

  def setup
    super

    # puts self.name

    @processor = RubyParser.new
  end

  def test_block_append
    head = s(:args)
    tail = s(:zsuper)
    expected = s(:block, s(:args), s(:zsuper))
    assert_equal expected, @processor.block_append(head, tail)
  end

  def test_block_append_begin_begin
    head = s(:begin, s(:args))
    tail = s(:begin, s(:args))
    expected = s(:block, s(:args), s(:begin, s(:args)))
    assert_equal expected, @processor.block_append(head, tail)
  end

  def test_block_append_block
    head = s(:block, s(:args))
    tail = s(:zsuper)
    expected = s(:block, s(:args), s(:zsuper))
    assert_equal expected, @processor.block_append(head, tail)
  end

  def test_block_append_nil_head
    head = nil
    tail = s(:zsuper)
    expected = s(:zsuper)
    assert_equal expected, @processor.block_append(head, tail)
  end

  def test_block_append_nil_tail
    head = s(:args)
    tail = nil
    expected = s(:args)
    assert_equal expected, @processor.block_append(head, tail)
  end

  def test_block_append_tail_block
    head = s(:vcall, :f1)
    tail = s(:block, s(:undef, s(:lit, :x)), s(:undef, s(:lit, :y)))
    expected = s(:block,
                 s(:vcall, :f1),
                 s(:block, s(:undef, s(:lit, :x)), s(:undef, s(:lit, :y))))
    assert_equal expected, @processor.block_append(head, tail)
  end

  def test_call_env
    @processor.env[:a] = :lvar
    expected = s(:call, s(:lvar, :a), :happy)

    assert_equal expected, @processor.parse('a.happy')
  end

  def test_dasgn_icky2
    rb = "a do\n  v = nil\n  begin\n    yield\n  rescue Exception => v\n    break\n  end\nend"
    pt = s(:iter,
           s(:fcall, :a),
           nil,
           s(:block,
             s(:dasgn_curr, :v, s(:nil)),
             s(:begin,
               s(:rescue,
                 s(:yield),
                 s(:resbody,
                   s(:array, s(:const, :Exception)),
                   s(:block, s(:dasgn_curr, :v, s(:gvar, :$!)), s(:break)))))))

    assert_equal pt, @processor.parse(rb)
  end

  def test_class_comments
    rb = "# blah 1\n# blah 2\n\nclass X\n  # blah 3\n  def blah\n    # blah 4\n  end\nend"
    pt = s(:class, :X, nil,
           s(:scope,
             s(:defn, :blah, s(:scope, s(:block, s(:args), s(:nil))))))

    actual = @processor.parse(rb)
    assert_equal pt, actual

    assert_equal "# blah 1\n# blah 2\n\n", actual.comments
    assert_equal "# blah 3\n", actual.scope.defn.comments
  end

  def test_module_comments
    rb = "# blah 1\n  \n  # blah 2\n\nmodule X\n  # blah 3\n  def blah\n    # blah 4\n  end\nend"
    pt = s(:module, :X,
           s(:scope,
             s(:defn, :blah, s(:scope, s(:block, s(:args), s(:nil))))))

    actual = @processor.parse(rb)
    assert_equal pt, actual
    assert_equal "# blah 1\n\n# blah 2\n\n", actual.comments
    assert_equal "# blah 3\n", actual.scope.defn.comments
  end

  def test_defn_comments
    rb = "# blah 1\n# blah 2\n\ndef blah\nend"
    pt = s(:defn, :blah, s(:scope, s(:block, s(:args), s(:nil))))

    actual = @processor.parse(rb)
    assert_equal pt, actual
    assert_equal "# blah 1\n# blah 2\n\n", actual.comments
  end

  def test_defs_comments
    rb = "# blah 1\n# blah 2\n\ndef self.blah\nend"
    pt = s(:defs, s(:self), :blah, s(:scope, s(:args)))

    actual = @processor.parse(rb)
    assert_equal pt, actual
    assert_equal "# blah 1\n# blah 2\n\n", actual.comments
  end

  def test_do_bug # TODO: rename
    rb = "a 1\na.b do |c|\n  # do nothing\nend"
    pt = s(:block,
           s(:fcall, :a, s(:array, s(:lit, 1))),
           s(:iter, s(:call, s(:vcall, :a), :b), s(:dasgn_curr, :c)))

    assert_equal pt, @processor.parse(rb)
  end

  def test_dstr_evstr
    rb = "\"#\{'a'}#\{b}\""
    pt = s(:dstr, "a", s(:evstr, s(:vcall, :b)))

    assert_equal pt, @processor.parse(rb)
  end

  def test_dstr_str
    rb = "\"#\{'a'} b\""
    pt = s(:str, "a b")

    assert_equal pt, @processor.parse(rb)
  end

  def test_empty
    rb = ""
    pt = nil

    assert_equal pt, @processor.parse(rb)
  end

  def test_evstr_evstr
    rb = "\"#\{a}#\{b}\""
    pt = s(:dstr, "", s(:evstr, s(:vcall, :a)), s(:evstr, s(:vcall, :b)))

    assert_equal pt, @processor.parse(rb)
  end

  def test_evstr_str
    rb = "\"#\{a} b\""
    pt = s(:dstr, "", s(:evstr, s(:vcall, :a)), s(:str, " b"))

    assert_equal pt, @processor.parse(rb)
  end

  def test_lasgn_env
    rb = 'a = 42'
    pt = s(:lasgn, :a, s(:lit, 42))
    expected_env = { :a => :lvar }
    
    assert_equal pt, @processor.parse(rb)
    assert_equal expected_env, @processor.env.all
  end

  def test_list_append
    lhs, rhs = s(:array, s(:lit, :iter)), s(:when, s(:const, :BRANCHING), nil)
    expected = s(:array, s(:lit, :iter), s(:when, s(:const, :BRANCHING), nil))

    assert_equal expected, @processor.list_append(lhs, rhs)
  end

  def test_literal_concat_dstr_dstr
    lhs      = s(:dstr, "Failed to download spec ",
                 s(:evstr, s(:vcall, :spec_name)),
                 s(:str, " from "),
                 s(:evstr, s(:vcall, :source_uri)),
                 s(:str, ":\n"))
    rhs      = s(:dstr, "\t",
                 s(:evstr, s(:call, s(:ivar, :@fetch_error), :message)))
    expected = s(:dstr, "Failed to download spec ",
                 s(:evstr, s(:vcall, :spec_name)),
                 s(:str, " from "),
                 s(:evstr, s(:vcall, :source_uri)),
                 s(:str, ":\n"),
                 s(:str, "\t"),
                 s(:evstr, s(:call, s(:ivar, :@fetch_error), :message)))

    assert_equal expected, @processor.literal_concat(lhs, rhs)
  end

  def test_literal_concat_dstr_evstr
    lhs, rhs = s(:dstr, "a"), s(:evstr, s(:vcall, :b))
    expected = s(:dstr, "a", s(:evstr, s(:vcall, :b)))

    assert_equal expected, @processor.literal_concat(lhs, rhs)
  end

  def test_literal_concat_evstr_evstr
    lhs, rhs = s(:evstr, s(:lit, 1)), s(:evstr, s(:lit, 2))
    expected = s(:dstr, "", s(:evstr, s(:lit, 1)), s(:evstr, s(:lit, 2)))

    assert_equal expected, @processor.literal_concat(lhs, rhs)
  end

  def test_literal_concat_str_evstr
    lhs, rhs = s(:str, ""), s(:evstr, s(:str, "blah"))

    assert_equal s(:str, "blah"), @processor.literal_concat(lhs, rhs)
  end

  def test_logop_12
    lhs = s(:lit, 1)
    rhs = s(:lit, 2)
    exp = s(:and, s(:lit, 1), s(:lit, 2))

    assert_equal exp, @processor.logop(:and, lhs, rhs)
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

    assert_equal exp, @processor.logop(:and, lhs, rhs)
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

    assert_equal exp, @processor.logop(:and, lhs, rhs)
  end

  def test_logop_12_3
    lhs = s(:and, s(:lit, 1), s(:lit, 2))
    rhs = s(:lit, 3)
    exp = s(:and, s(:lit, 1), s(:and, s(:lit, 2), s(:lit, 3)))

    assert_equal exp, @processor.logop(:and, lhs, rhs)
  end

  def test_logop_nested_mix
    lhs = s(:or, s(:vcall, :a), s(:vcall, :b))
    rhs = s(:and, s(:vcall, :c), s(:vcall, :d))
    exp = s(:or,
            s(:or, s(:vcall, :a), s(:vcall, :b)),
            s(:and, s(:vcall, :c), s(:vcall, :d)))

    lhs.paren = true
    rhs.paren = true

    assert_equal exp, @processor.logop(:or, lhs, rhs)
  end

  def test_str_evstr
    rb = "\"a #\{b}\""
    pt = s(:dstr, "a ", s(:evstr, s(:vcall, :b)))

    assert_equal pt, @processor.parse(rb)
  end

  def test_str_pct_Q_nested
    rb = "%Q[before [#\{nest}] after]"
    pt = s(:dstr, "before [", s(:evstr, s(:vcall, :nest)), s(:str, "] after"))

    assert_equal pt, @processor.parse(rb)
  end

  def test_str_str
    rb = "\"a #\{'b'}\""
    pt = s(:str, "a b")

    assert_equal pt, @processor.parse(rb)
  end

  def test_str_str_str
    rb = "\"a #\{'b'} c\""
    pt = s(:str, "a b c")

    assert_equal pt, @processor.parse(rb)
  end
end

__END__

# blah18.rb

assert_equal("sub", $_)
