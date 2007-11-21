#!/usr/local/bin/ruby

require 'test/unit'
require 'ruby_lexer'

class TestRubyLexer < Test::Unit::TestCase
  def deny cond, msg = nil
    assert ! cond, msg
  end

  def setup
    @lex = RubyLexer.new
    @lex.src = StringIO.new("blah blah")
    @lex.lex_state = :expr_beg # HACK ? I have no idea actually
  end

  def test_advance
    assert @lex.advance # blah
    assert @lex.advance # blah
    deny   @lex.advance # nada
  end

#   def test_arg_ambiguous
#     raise NotImplementedError, 'Need to write test_arg_ambiguous'
#   end

#   def test_here_document
#     raise NotImplementedError, 'Need to write test_here_document'
#   end

#   def test_here_document_identifier
#     raise NotImplementedError, 'Need to write test_here_document_identifier'
#   end

  def test_is_next_identchar
    assert @lex.is_next_identchar
    @lex.src = StringIO.new(" ")
    deny @lex.is_next_identchar
    @lex.src = StringIO.new("-")
    deny @lex.is_next_identchar
  end

  def test_is_next_no_case # TODO: worst name evah
    @lex.src = StringIO.new("123 456")
    assert @lex.is_next_no_case("123")
    pos = @lex.src.pos
    deny @lex.is_next_no_case("begin")
    assert_equal " 456", @lex.src.read_all, "must put back contents"
  end

#   def test_nextc
#     raise NotImplementedError, 'Need to write test_nextc'
#   end

  def test_number_token
    node = @lex.number_token("42", false, "\0")
    assert_equal :tINTEGER, node
    assert_equal 42, @lex.yacc_value
  end

  def test_parse_number
    @lex.src = StringIO.new '42'
    node = @lex.parse_number('1')
    assert_equal :tINTEGER, node
    assert_equal 142, @lex.yacc_value
  end

  def test_parse_quote
    @lex.src = StringIO.new 'blah)'
    node = @lex.parse_quote('(')
    assert_equal :tSTRING_BEG, node
    assert_equal s(:strterm, RubyLexer::STR_DQUOTE, ")", "("), @lex.lex_strterm
    assert_equal ["%)"], @lex.yacc_value.args # FIX double check this
  end

#   def test_parse_string
#     raise NotImplementedError, 'Need to write test_parse_string'
#   end

#   def test_pushback
#     raise NotImplementedError, 'Need to write test_pushback'
#   end

#   def test_rb_compile_error
#     raise NotImplementedError, 'Need to write test_rb_compile_error'
#   end

#   def test_read_comment
#     raise NotImplementedError, 'Need to write test_read_comment'
#   end

#   def test_read_escape
#     raise NotImplementedError, 'Need to write test_read_escape'
#   end

#   def test_regx_options
#     raise NotImplementedError, 'Need to write test_regx_options'
#   end

#   def test_tokadd_escape
#     raise NotImplementedError, 'Need to write test_tokadd_escape'
#   end

#   def test_tokadd_string
#     raise NotImplementedError, 'Need to write test_tokadd_string'
#   end

  def test_yylex_integer
    util_lex_token "42", :tINTEGER, 42
  end

  def test_yylex_float
    util_lex_token "1.0", :tFLOAT, 1.0
  end

  def test_yylex_constant
    util_lex_token("ArgumentError",
                   :tCONSTANT, t("ArgumentError"))
  end

  def test_yylex_constant_semi
    util_lex_token("ArgumentError;",
                   :tCONSTANT, t("ArgumentError"),
                   ";", t(";"))
  end

  def test_yylex_identifier
    util_lex_token("identifier",
                   :tIDENTIFIER, t("identifier"))
  end

  def test_yylex_regexp
    util_lex_token("/regexp/",
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regexp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_i
    util_lex_token("/regexp/i",
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regexp"),
                   :tREGEXP_END,     "i")
  end

  def test_yylex_regexp_escapes
    util_lex_token('/re\tge\nxp/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "re\\tge\\nxp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_string_double
    util_lex_token('"string"',
                   :tSTRING_BEG,     t('"'),
                   :tSTRING_CONTENT, s(:str, "string"),
                   :tSTRING_END,     t('"'))
  end

  def test_yylex_string_double_escapes
    util_lex_token('"s\tri\ng"',
                   :tSTRING_BEG,     t('"'),
                   :tSTRING_CONTENT, s(:str, "s\tri\ng"),
                   :tSTRING_END,     t('"'))
  end

  def test_yylex_string_single
    util_lex_token("'string'",
                   :tSTRING_BEG,     t("'"),
                   :tSTRING_CONTENT, s(:str, "string"),
                   :tSTRING_END,     t("'"))
  end

  def test_yylex_string_single_escapes
    util_lex_token("'s\\tri\\ng'",
                   :tSTRING_BEG,     t("'"),
                   :tSTRING_CONTENT, s(:str, "s\\tri\\ng"),
                   :tSTRING_END,     t("'"))
  end

  def test_yylex_symbol
    util_lex_token(":symbol",
                   :tSYMBEG, t(":"),
                   :tIDENTIFIER, t("symbol"))
  end

  def util_lex_token input, *args
    @lex.src = StringIO.new input

    until args.empty? do
      token = args.shift
      value = args.shift
      assert @lex.advance
      assert_equal [token, value], [@lex.token, @lex.yacc_value]
    end

    deny @lex.advance, "must be empty, but had #{[@lex.token, @lex.yacc_value].inspect}"
  end
end

class TestStackState < Test::Unit::TestCase
  def test_stack_state
    s = StackState.new :test
    s.push true
    s.push false
    s.lexpop
    assert_equal [false, true], s.stack
  end

  def test_is_in_state
    s = StackState.new :test
    assert_equal false, s.is_in_state
    s.push false
    assert_equal false, s.is_in_state
    s.push true
    assert_equal true, s.is_in_state
    s.push false
    assert_equal false, s.is_in_state
  end

  def test_lexpop
    s = StackState.new :test
    assert_equal [false], s.stack
    s.push true
    s.push false
    assert_equal [false, true, false], s.stack
    s.lexpop
    assert_equal [false, true], s.stack
  end

  def test_pop
    s = StackState.new :test
    assert_equal [false], s.stack
    s.push true
    assert_equal [false, true], s.stack
    assert_equal true, s.pop
    assert_equal [false], s.stack
  end

  def test_pop_top
    s = StackState.new :test
    assert_raise RuntimeError do
      s.pop
    end
  end

  def test_push
    s = StackState.new :test
    assert_equal [false], s.stack
    s.push true
    s.push false
    assert_equal [false, true, false], s.stack
  end
end
