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

  def test_yylex_integer
    util_lex_token "42", :tINTEGER, 42
  end

  def test_yylex_integer_eh_a
    util_lex_token('?a',
                   :tINTEGER,     97)
  end

  def test_yylex_integer_eh_escape_M_escape_C
    util_lex_token('?\M-\C-a',
                   :tINTEGER,     129)
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

  def test_yylex_regexp_nm
    util_lex_token("/.*/nm",
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, ".*"),
                   :tREGEXP_END,     "nm")
  end

  def test_yylex_regexp_escapes
    util_lex_token('/re\tge\nxp/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "re\\tge\\nxp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_oct
    util_lex_token('/re\tge\101xp/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "re\\tge\\101xp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_hex
    util_lex_token('/re\tge\x61xp/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "re\\tge\\x61xp"),
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

  def test_yylex_string_double_escape_M
    util_lex_token('"\M-g"',
                   :tSTRING_BEG,     t('"'),
                   :tSTRING_CONTENT, s(:str, "\347"),
                   :tSTRING_END,     t('"'))
  end

  def test_yylex_string_double_escape_octal
    util_lex_token('"n = \101\102\103"',
                   :tSTRING_BEG,     t('"'),
                   :tSTRING_CONTENT, s(:str, "n = ABC"),
                   :tSTRING_END,     t('"'))
  end

  def test_yylex_string_double_escape_hex
    util_lex_token('"n = \x61\x62\x63"',
                   :tSTRING_BEG,     t('"'),
                   :tSTRING_CONTENT, s(:str, "n = abc"),
                   :tSTRING_END,     t('"'))
  end

  def test_yylex_string_single
    util_lex_token("'string'",
                   :tSTRING_BEG,     t("'"),
                   :tSTRING_CONTENT, s(:str, "string"),
                   :tSTRING_END,     t("'"))
  end

  def test_yylex_string_pct_Q
    util_lex_token("%Q[string]",
                   :tSTRING_BEG,     t("%Q["),
                   :tSTRING_CONTENT, s(:str, "string"),
                   :tSTRING_END,     t("]"))
  end

  def test_yylex_string_single_escapes
    util_lex_token("'s\\tri\\ng'",
                   :tSTRING_BEG,     t("'"),
                   :tSTRING_CONTENT, s(:str, "s\\tri\\ng"),
                   :tSTRING_END,     t("'"))
  end

  def test_yylex_global
    util_lex_token("$blah",
                   :tGVAR,     t("$blah"))
  end

  def test_yylex_global_wierd
    util_lex_token("$__blah",
                   :tGVAR,     t("$__blah"))
  end

  def test_yylex_global_dollar_underscore
    util_lex_token("$_",
                   :tGVAR,     t("$_"))
  end

  def test_yylex_symbol
    util_lex_token(":symbol",
                   :tSYMBEG, t(":"),
                   :tIDENTIFIER, t("symbol"))
  end

  def test_yylex_comment_begin
    util_lex_token("=begin\nblah\nblah\n=end\n42",
                   :tINTEGER, 42)
  end

  def util_lex_token input, *args
    @lex.src = StringIO.new input

    until args.empty? do
      token = args.shift
      value = args.shift
      assert @lex.advance, "no more tokens"
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

  def test_push
    s = StackState.new :test
    assert_equal [false], s.stack
    s.push true
    s.push false
    assert_equal [false, true, false], s.stack
  end
end

class TestEnvironment < Test::Unit::TestCase
  def deny t
    assert ! t
  end

  def setup
    @env = Environment.new
    @env[:blah] = 42
    assert_equal 42, @env[:blah]
  end

  def test_use
    @env.use :blah
    expected = [{ :blah => true }]
    assert_equal expected, @env.instance_variable_get(:"@use")
  end

  def test_use_scoped
    @env.use :blah
    @env.extend
    expected = [{}, { :blah => true }]
    assert_equal expected, @env.instance_variable_get(:"@use")
  end

  def test_used_eh
    @env.extend :dynamic
    @env[:x] = :dvar
    @env.use :x
    assert_equal true, @env.used?(:x)
  end

  def test_used_eh_none
    assert_equal nil, @env.used?(:x)
  end

  def test_used_eh_scoped
    self.test_used_eh
    @env.extend :dynamic
    assert_equal true, @env.used?(:x)
  end

  def test_var_scope_dynamic
    @env.extend :dynamic
    assert_equal 42, @env[:blah]
    @env.unextend
    assert_equal 42, @env[:blah]
  end

  def test_var_scope_static
    @env.extend
    assert_equal nil, @env[:blah]
    @env.unextend
    assert_equal 42, @env[:blah]
  end

  def test_dynamic
    expected1 = {}
    expected2 = { :x => 42 }

    assert_equal expected1, @env.dynamic
    begin
      @env.extend :dynamic
      assert_equal expected1, @env.dynamic

      @env[:x] = 42
      assert_equal expected2, @env.dynamic

      begin
        @env.extend :dynamic
        assert_equal expected2, @env.dynamic
        @env.unextend
      end

      assert_equal expected2, @env.dynamic
      @env.unextend
    end
    assert_equal expected1, @env.dynamic
  end

  def test_all_dynamic
    expected = { :blah => 42 }

    @env.extend :dynamic
    assert_equal expected, @env.all
    @env.unextend
    assert_equal expected, @env.all
  end

  def test_all_static
    @env.extend
    expected = { }
    assert_equal expected, @env.all

    @env.unextend
    expected = { :blah => 42 }
    assert_equal expected, @env.all
  end

  def test_dynamic_eh
    assert_equal false, @env.dynamic?
    @env.extend :dynamic
    assert_equal true, @env.dynamic?
    @env.extend
    assert_equal false, @env.dynamic?
  end

  def test_all_static_deeper
    expected0 = { :blah => 42 }
    expected1 = { :blah => 42, :blah2 => 24 }
    expected2 = { :blah => 27 }

    @env.extend :dynamic
    @env[:blah2] = 24
    assert_equal expected1, @env.all

    @env.extend 
    @env[:blah] = 27
    assert_equal expected2, @env.all

    @env.unextend
    assert_equal expected1, @env.all

    @env.unextend
    assert_equal expected0, @env.all
  end
end
