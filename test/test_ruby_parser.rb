#!/usr/local/bin/ruby

require 'test/unit'
require 'ruby_parser'

$: << File.expand_path('~/Work/p4/zss/src/ParseTree/dev/lib')
$: << File.expand_path('~/Work/p4/zss/src/ParseTree/dev/test')

require 'pt_testcase'

class TestRubyParser < Test::Unit::TestCase # ParseTreeTestCase
  def setup
    super
    @processor = RubyParser.new
  end

  def test_lasgn_env
    rb = 'a = 42'
    pt = s(:lasgn, :a, s(:lit, 42))
    expected_env = { :a => :lvar }
    
    assert_equal pt, @processor.parse(rb)
    assert_equal expected_env, @processor.env.all
  end

  def test_call_env
    @processor.env[:a] = :lvar
    expected = s(:call, s(:lvar, :a), :happy)

    assert_equal expected, @processor.parse('a.happy')
  end

  eval ParseTreeTestCase.testcases.map { |node, data|
    next if node.to_s =~ /bmethod|dmethod|dx?str|dx?sym|dx?regx/
    next if Array === data['Ruby'] # runtime only crap
    "def test_#{node}
       rb = #{data['Ruby'].inspect}
       pt = #{data['ParseTree'].inspect}

       assert_not_nil rb, \"Ruby for #{node} undefined\"
       assert_not_nil pt, \"ParseTree for #{node} undefined\"

       assert_equal Sexp.from_array(pt), @processor.parse(rb)
     end"
  }.compact.join("\n")
end

# p Regexp.union(*ParseTreeTestCase.testcases.sort_by { |k,v| v.inspect.size }[20...40].map { |k,v| k }.sort)
