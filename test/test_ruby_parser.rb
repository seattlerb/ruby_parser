#!/usr/local/bin/ruby

require 'test/unit'
require 'ruby_parser'

$: << File.expand_path('~/Work/p4/zss/src/ParseTree/dev/lib')
$: << File.expand_path('~/Work/p4/zss/src/ParseTree/dev/test')

require 'pt_testcase'

class TestRubyParser < Test::Unit::TestCase # ParseTreeTestCase
  def setup
    super

    # puts self.name

    @processor = RubyParser.new
  end

  def test_lasgn_env
    rb = 'a = 42'
    pt = s(:lasgn, :a, s(:lit, 42))
    expected_env = { :a => :lvar }
    
    assert_equal pt, @processor.parse(rb)
    assert_equal expected_env, @processor.env.all
  end

  def test_do_bug
    rb = "a 1\na.b do |c|\n  # do nothing\nend"
    pt = s(:block,
           s(:fcall, :a, s(:array, s(:lit, 1))),
           s(:iter, s(:call, s(:vcall, :a), :b), s(:dasgn_curr, :c)))

    assert_equal pt, @processor.parse(rb)
  end

  def test_call_env
    @processor.env[:a] = :lvar
    expected = s(:call, s(:lvar, :a), :happy)

    assert_equal expected, @processor.parse('a.happy')
  end

  eval ParseTreeTestCase.testcases.map { |node, data|
    next if node.to_s =~ /bmethod|dmethod/
    next if Array === data['Ruby'] # runtime only crap
    "def test_#{node}
       rb = #{data['Ruby'].inspect}
       pt = #{data['ParseTree'].inspect}

       assert_not_nil rb, \"Ruby for #{node} undefined\"
       assert_not_nil pt, \"ParseTree for #{node} undefined\"

       assert_equal Sexp.from_array(pt), @processor.parse(rb)
     end"
  }.compact.join("\n")

  if ENV['ZOMGPONIES'] or File.exist? 'zomgponies' then
    require 'parse_tree'

    files = Dir["/usr/lib/ruby/1.8/test/**/*.rb"] # TODO: drop 1.8 so I get gems

    eval files.map { |file|
      name = file.split(/\//)[4..-1].join('_').gsub(/\W+/, '_')
      eval "def test_#{name}
       file = #{file.inspect}
       rb = File.read(file)

       pt = ParseTree.new.parse_tree_for_string rb
       assert_not_nil pt, \"ParseTree for #{name} undefined\"

       rp = @processor.parse rb
       assert_equal Sexp.from_array(pt).first, rp, \"RP different from PT\"
     end"
    }.compact.join("\n")
  end
end
