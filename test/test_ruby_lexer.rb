#!/usr/local/bin/ruby

require 'test/unit'
require 'ruby_lexer'

class TestRubyLexer < Test::Unit::TestCase
  def setup
    @lex = RubyLexer.new
    @lex.src = StringIO.new("blah blah")
  end

  def test_reset
    @lex.reset
  end

  def test_advance
    @lex.advance
  end

  def test_stack_state
    s = StackState.new :test
    s.push true
    s.push false
    s.lexpop
    assert_equal [false, true], s.stack
  end

  def test_is_next_identchar
    assert @lex.is_next_identchar
  end

  def test_is_next_no_case
    assert @lex.is_next_no_case("blah") # TODO No clue
  end

  def deny cond
    assert ! cond
  end

  def test_is_hex_char
    assert @lex.is_hex_char("a")
    assert @lex.is_hex_char("1")
    deny @lex.is_hex_char("g")
    deny @lex.is_hex_char(" ")
  end

  def test_is_oct_char
    assert @lex.is_oct_char("0")
    assert @lex.is_oct_char("7")
    deny @lex.is_oct_char("8")
    deny @lex.is_oct_char("a")
    deny @lex.is_oct_char(" ")
  end

  def test_parse_quote
    @lex.src = StringIO.new 'blah)'
    node = @lex.parse_quote('(')
    assert_equal Tokens.tSTRING_BEG, node
    assert_equal ")", @lex.lex_strterm.term
    assert_equal "(", @lex.lex_strterm.open
    assert_equal ["%)"], @lex.yacc_value.args # FIX double check this
  end

  def test_is_identifier_char
    assert @lex.is_identifier_char("0")
    assert @lex.is_identifier_char("9")
    assert @lex.is_identifier_char("a")
    assert @lex.is_identifier_char("_")
    deny @lex.is_identifier_char(" ")
    deny @lex.is_identifier_char(" ")
  end

  def test_number_token
    node = @lex.number_token("42", false, "\0")
    assert_equal Tokens.tINTEGER, node
    assert_equal 42, @lex.yacc_value
  end

#   def test_read_comment
#     node = @lex.read_comment "blah blah blah"
#     assert_equal 42, node
#   end

#   def test_here_document_identifier
#     node = @lex.here_document_identifier "blah blah blah"
#     assert node
#   end

#   def test_arg_ambiguous
#     node = @lex.arg_ambiguous "blah blah blah"
#     assert node
#   end

  def test_parse_number
    @lex.src = StringIO.new '42'
    node = @lex.parse_number('1')
    assert_equal Tokens.tINTEGER, node
    assert_equal 142, @lex.yacc_value
  end
end
