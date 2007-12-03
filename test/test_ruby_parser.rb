#!/usr/local/bin/ruby

require 'test/unit'
require 'ruby_parser'

$: << File.expand_path('~/Work/p4/zss/src/ParseTree/dev/lib')
$: << File.expand_path('~/Work/p4/zss/src/ParseTree/dev/test')

if ENV['PROFILE_INDEX'] then
  require 'rubygems'
  require 'spy_on'
  Array.spy_on :[]
end

if ENV['PROFILE_EACH'] then
  $each = []

  class Array
    alias :safe_each :each
    def each(&b)
      $each << caller[0..2]
      safe_each(&b)
    end
  end

  at_exit {
    at_exit {
      $each.safe_each do |pair|
        p pair
      end
    }
  }
end

require 'pt_testcase'

class TestRubyParser < Test::Unit::TestCase # ParseTreeTestCase

  # Regular ParseTreeTestCase tests
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

  # Scour the world and compare against ParseTree
  if ENV['ZOMGPONIES'] or File.exist? 'zomgponies' then
    require 'parse_tree'

    files = Dir["/usr/lib/ruby/**/*.rb"] 
    files = Dir["/usr/lib/ruby/1.8/**/*.rb"]      # TODO: drop 1.8 so I get gems
    files = Dir["/usr/lib/ruby/1.8/test/**/*.rb"] # TODO: drop test so I get 1.8
    files = Dir["unit/*.rb"]

    files.reject! { |f| f =~ /tk|tcl/ } # causes a bus error ?!?

    eval files.map { |file|
      name = File.basename(file).gsub(/\W+/, '_')
      loc = `wc -l #{file}`.strip.to_i
      # name = file.split(/\//)[4..-1].join('_').gsub(/\W+/, '_')
      eval "def test_#{name}_#{loc}
       file = #{file.inspect}
       rb = File.read(file)

       pt = ParseTree.new.parse_tree_for_string rb
       assert_not_nil pt, \"ParseTree for #{name} undefined\"

       rp = @processor.parse rb
       assert_equal Sexp.from_array(pt).first, rp, \"RP different from PT\"
     end"
    }.compact.join("\n")
  end

  def setup
    unless defined? @@suck_it then
      @@suck_it = true
    end

    super

    # puts self.name

    @processor = RubyParser.new
  end

#   def test_append_to_block
#     raise NotImplementedError, 'Need to write test_append_to_block'
#   end

#   def test_arg_add
#     raise NotImplementedError, 'Need to write test_arg_add'
#   end

#   def test_arg_blk_pass
#     raise NotImplementedError, 'Need to write test_arg_blk_pass'
#   end

#   def test_arg_concat
#     raise NotImplementedError, 'Need to write test_arg_concat'
#   end

#   def test_aryset
#     raise NotImplementedError, 'Need to write test_aryset'
#   end

#   def test_assignable
#     raise NotImplementedError, 'Need to write test_assignable'
#   end

  def test_block_append
    head = s(:args)
    tail = s(:zsuper)
    expected = s(:block, s(:args), s(:zsuper))
    assert_equal expected, @processor.block_append(head, tail)
  end

  def test_block_append_block
    head = s(:block, s(:args))
    tail = s(:zsuper)
    expected = s(:block, s(:args), s(:zsuper))
    assert_equal expected, @processor.block_append(head, tail)
  end

  def test_block_append_begin_begin
    head = s(:begin, s(:args))
    tail = s(:begin, s(:args))
    expected = if $VERBOSE then # HACK - "bug" in ruby is forcing this
                 s(:block, s(:args), s(:begin, s(:args)))
               else
                 s(:block, s(:begin, s(:args)), s(:begin, s(:args)))
               end
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

  def test_call_env
    @processor.env[:a] = :lvar
    expected = s(:call, s(:lvar, :a), :happy)

    assert_equal expected, @processor.parse('a.happy')
  end

#   def test_cond
#     raise NotImplementedError, 'Need to write test_cond'
#   end

  def test_do_bug # TODO: rename
    rb = "a 1\na.b do |c|\n  # do nothing\nend"
    pt = s(:block,
           s(:fcall, :a, s(:array, s(:lit, 1))),
           s(:iter, s(:call, s(:vcall, :a), :b), s(:dasgn_curr, :c)))

    assert_equal pt, @processor.parse(rb)
  end

#   def test_do_parse
#     raise NotImplementedError, 'Need to write test_do_parse'
#   end

#   def test_env
#     raise NotImplementedError, 'Need to write test_env'
#   end

#   def test_get_match_node
#     raise NotImplementedError, 'Need to write test_get_match_node'
#   end

#   def test_gettable
#     raise NotImplementedError, 'Need to write test_gettable'
#   end

  def test_lasgn_env
    rb = 'a = 42'
    pt = s(:lasgn, :a, s(:lit, 42))
    expected_env = { :a => :lvar }
    
    assert_equal pt, @processor.parse(rb)
    assert_equal expected_env, @processor.env.all
  end

  def test_logop_12
    lhs = s(:lit, 1)
    rhs = s(:lit, 2)
    exp = s(:and, s(:lit, 1), s(:lit, 2))

    assert_equal exp, @processor.logop(:and, lhs, rhs)
  end

  def test_logop_12_3
    lhs = s(:and, s(:lit, 1), s(:lit, 2))
    rhs = s(:lit, 3)
    exp = s(:and, s(:lit, 1), s(:and, s(:lit, 2), s(:lit, 3)))

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

  def test_dasgn_icky1
    rb = '
a do
  v = nil                       # dasgn_curr
  assert_block(full_message) do
    begin
      yield
    rescue Exception => v       # dasgn
      break
    end
  end
end
'
    pt = s(:iter,
           s(:fcall, :a),
           nil,
           s(:block,
             s(:dasgn_curr, :v, s(:nil)),
             s(:iter,
               s(:fcall, :assert_block, s(:array, s(:vcall, :full_message))),
               nil,
               s(:begin,
                 s(:rescue,
                   s(:yield),
                   s(:resbody,
                     s(:array, s(:const, :Exception)),
                     s(:block, s(:dasgn, :v, s(:gvar, :$!)), s(:break))))))))

    assert_equal pt, @processor.parse(rb)
  end

  def test_dasgn_icky2
    rb = '
a do
  v = nil                       # dasgn_curr
  begin
    yield
  rescue Exception => v         # dasgn_curr
    break
  end
end
'
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


#   def test_list_append
#     raise NotImplementedError, 'Need to write test_list_append'
#   end

#   def test_literal_concat
#     raise NotImplementedError, 'Need to write test_literal_concat'
#   end

#   def test_new_fcall
#     raise NotImplementedError, 'Need to write test_new_fcall'
#   end

#   def test_new_super
#     raise NotImplementedError, 'Need to write test_new_super'
#   end

#   def test_new_yield
#     raise NotImplementedError, 'Need to write test_new_yield'
#   end

#   def test_next_token
#     raise NotImplementedError, 'Need to write test_next_token'
#   end

#   def test_node_assign
#     raise NotImplementedError, 'Need to write test_node_assign'
#   end

#   def test_old_yyerror
#     raise NotImplementedError, 'Need to write test_old_yyerror'
#   end

#   def test_parse
#     raise NotImplementedError, 'Need to write test_parse'
#   end

#   def test_pop_local_scope
#     raise NotImplementedError, 'Need to write test_pop_local_scope'
#   end

#   def test_push_local_scope
#     raise NotImplementedError, 'Need to write test_push_local_scope'
#   end

#   def test_void_stmts
#     raise NotImplementedError, 'Need to write test_void_stmts'
#   end
end
