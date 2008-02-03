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

  ############################################################

  def test_yylex_and
    util_lex_token "&", :tAMPER, t("&")
  end

  def test_yylex_and2
    util_lex_token "&&", :tANDOP, t("&&")
  end

  def test_yylex_and2_equals
    util_lex_token "&&=", :tOP_ASGN, t("&&")
  end

  def test_yylex_and_equals
    util_lex_token "&=", :tOP_ASGN, t("&")
  end

  def test_yylex_assoc
    util_lex_token "=>", :tASSOC, t("=>")
  end

  def test_yylex_backtick
    util_lex_token("A::B",
                   :tCONSTANT, t("A"),
                   :tCOLON2, t(":"), # FIX?
                   :tCONSTANT, t("B"))
  end

  def test_yylex_bang
    util_lex_token "!", :tBANG, t("!")
  end

  def test_yylex_bang_equals
    util_lex_token "!=", :tNEQ, t("!=")
  end

  def test_yylex_bang_tilde
    util_lex_token "!~", :tNMATCH, t("!~")
  end

  def test_yylex_carat
    util_lex_token "^", :tCARET, t("^")
  end

  def test_yylex_carat_equals
    util_lex_token "^=", :tOP_ASGN, t("^")
  end

  def test_yylex_colon2
    util_lex_token("A::B",
                   :tCONSTANT, t("A"),
                   :tCOLON2, t(":"), # FIX?
                   :tCONSTANT, t("B"))
  end

  def test_yylex_colon3
    util_lex_token "::", :tCOLON3, t("::")
  end

  def test_yylex_comma
    util_lex_token ",", ",", t(",") # FIX
  end

  def test_yylex_comment
    util_lex_token("1 # one\n2",
                   :tINTEGER, 1,
                   "\n", 1,
                   :tINTEGER, 2)
  end

  def test_yylex_comment_begin
    util_lex_token("=begin\nblah\nblah\n=end\n42",
                   :tINTEGER, 42)
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

  def test_yylex_div
    util_lex_token("a / 2",
                   :tIDENTIFIER, t("a"),
                   :tDIVIDE, t("/"),
                   :tINTEGER, 2)
  end

  def test_yylex_div_equals
    util_lex_token("a /= 2",
                   :tIDENTIFIER, t("a"),
                   :tOP_ASGN, t("/"),
                   :tINTEGER, 2)
  end

  def test_yylex_dot # HINT message sends
    util_lex_token ".", :tDOT, t(".")
  end

  def test_yylex_dot2
    util_lex_token "..", :tDOT2, t("..")
  end

  def test_yylex_dot3
    util_lex_token "...", :tDOT3, t("...")
  end

  def test_yylex_equals
    util_lex_token "=", '=', t("=") # FIX: this sucks
  end

  def test_yylex_equals2
    util_lex_token "==", :tEQ, t("==")
  end

  def test_yylex_equals3
    util_lex_token "===", :tEQQ, t("===")
  end

  def test_yylex_equals_tilde
    util_lex_token "=~", :tMATCH, t("=~")
  end

  def test_yylex_float
    util_lex_token "1.0", :tFLOAT, 1.0
  end

  def test_yylex_float_e
    util_lex_token "1e10", :tFLOAT, 1e10
  end

  def test_yylex_ge
    util_lex_token("a >= 2",
                   :tIDENTIFIER, t("a"),
                   :tGEQ, t(">="),
                   :tINTEGER, 2)
  end

  def test_yylex_global
    util_lex_token("$blah",
                   :tGVAR,     t("$blah"))
  end

  def test_yylex_global_dollar_underscore
    util_lex_token("$_",
                   :tGVAR,     t("$_"))
  end

  def test_yylex_global_wierd
    util_lex_token("$__blah",
                   :tGVAR,     t("$__blah"))
  end

  def test_yylex_gt
    util_lex_token("a > 2",
                   :tIDENTIFIER, t("a"),
                   :tGT, t(">"),
                   :tINTEGER, 2)
  end

  def test_yylex_identifier
    util_lex_token("identifier",
                   :tIDENTIFIER, t("identifier"))
  end

  def test_yylex_index
    @lex.lex_state = :expr_fname
    util_lex_token("def []",
                   :kDEF, t("def"),
                   :tAREF, t("[]"))
  end

  def test_yylex_index_equals
    @lex.lex_state = :expr_fname
    util_lex_token("def []=",
                   :kDEF, t("def"),
                   :tASET, t("[]="))
  end

  def test_yylex_integer
    util_lex_token "42", :tINTEGER, 42
  end

  def test_yylex_integer_bin
    util_lex_token "0b101010", :tINTEGER, 42
  end

  def test_yylex_integer_dec
    util_lex_token "0d42", :tINTEGER, 42
  end

  def test_yylex_integer_eh_a
    util_lex_token '?a', :tINTEGER, 97
  end

  def test_yylex_integer_eh_escape_M_escape_C
    util_lex_token '?\M-\C-a', :tINTEGER, 129
  end

  def test_yylex_integer_hex
    util_lex_token "0x2a", :tINTEGER, 42
  end

  def test_yylex_integer_oct
    util_lex_token "052", :tINTEGER, 42
  end

  def test_yylex_integer_underscore
    util_lex_token "4_2", :tINTEGER, 42
  end

  def test_yylex_lt
    util_lex_token "<", :tLT, t("<")
  end

  def test_yylex_lt2
    util_lex_token "<\<", :tLSHFT, t("<\<")
  end

  def test_yylex_lt2_equals
    util_lex_token "<\<=", :tOP_ASGN, t("<\<")
  end

  def test_yylex_lt_equals
    util_lex_token "<=", :tLEQ, t("<=")
  end

  def test_yylex_minus
    util_lex_token("1 - 2",
                   :tINTEGER, 1,
                   :tMINUS, t("-"),
                   :tINTEGER, 2)
  end

  def test_yylex_minus_equals
    util_lex_token "-=", :tOP_ASGN, t("-")
  end

  def test_yylex_minus_unary
    @lex.lex_state = :expr_fname
    util_lex_token "-@", :tUMINUS, t("-@")
  end

  def test_yylex_open_bracket
    util_lex_token("(",
                   :tLPAREN, t("("))
  end

  def test_yylex_open_curly_bracket
    util_lex_token("{",
                   :tLBRACE, t("{"))
  end

  def test_yylex_open_square_bracket
    util_lex_token("[1, 2, 3]",
                   :tLBRACK, t("["),
                   :tINTEGER, 1,
                   ",", t(","),
                   :tINTEGER, 2,
                   ",", t(","),
                   :tINTEGER, 3,
                   :tRBRACK, t("]"))
  end

  def test_yylex_or
    util_lex_token "|", :tPIPE, t("|")
  end

  def test_yylex_or2
    util_lex_token "||", :tOROP, t("||")
  end

  def test_yylex_or2_equals
    util_lex_token "||=", :tOP_ASGN, t("||")
  end

  def test_yylex_or_equals
    util_lex_token "|=", :tOP_ASGN, t("|")
  end

  def test_yylex_percent
    util_lex_token("a % 2",
                   :tIDENTIFIER, t("a"),
                   :tPERCENT, t("%"),
                   :tINTEGER, 2)
  end

  def test_yylex_percent_equals
    util_lex_token("a %= 2",
                   :tIDENTIFIER, t("a"),
                   :tOP_ASGN, t("%"),
                   :tINTEGER, 2)
  end

  def test_yylex_plus
    util_lex_token("1 + 1", # FIX lex_state?
                   :tINTEGER, 1,
                   :tPLUS, t("+"),
                   :tINTEGER, 1)
  end

  def test_yylex_plus_equals
    util_lex_token "+=", :tOP_ASGN, t("+")
  end

  def test_yylex_plus_unary
    @lex.lex_state = :expr_fname
    util_lex_token "+@", :tUPLUS, t("+@")
  end

  def test_yylex_rbracket
    util_lex_token "]", :tRBRACK, t("]")
  end

  def test_yylex_rcurly
    util_lex_token "}", :tRCURLY, t("end") # FIX?
  end

  def test_yylex_regexp
    util_lex_token("/regexp/",
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regexp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp
    util_lex_token "/./", :blah, t("/./")
  end

  def test_yylex_regexp_escape_chars
    util_lex_token('/re\tge\nxp/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "re\\tge\\nxp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_hex
    util_lex_token('/re\tge\x61xp/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "re\\tge\\x61xp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_oct
    util_lex_token('/re\tge\101xp/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "re\\tge\\101xp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_nm
    util_lex_token("/.*/nm",
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, ".*"),
                   :tREGEXP_END,     "nm")
  end

  def test_yylex_rparen
    util_lex_token ")", :tRPAREN, t(")")
  end

  def test_yylex_rshft
    util_lex_token("a >> 2",
                   :tIDENTIFIER, t("a"),
                   :tRSHFT, t(">>"),
                   :tINTEGER, 2)
  end

  def test_yylex_rshft_equals
    util_lex_token("a >>= 2",
                   :tIDENTIFIER, t("a"),
                   :tOP_ASGN, t(">>"),
                   :tINTEGER, 2)
  end

  def test_yylex_star
    util_lex_token("a * ",
                   :tIDENTIFIER, t("a"),
                   :tSTAR2, t("*"))

    assert_equal :expr_beg, @lex.lex_state
  end

  def test_yylex_star2
    util_lex_token("a ** ",
                   :tIDENTIFIER, t("a"),
                   :tPOW, t("**"))

    assert_equal :expr_beg, @lex.lex_state
  end

  def test_yylex_star2_equals
    util_lex_token("a **= ",
                   :tIDENTIFIER, t("a"),
                   :tOP_ASGN, t("**"))

    assert_equal :expr_beg, @lex.lex_state
  end

  def test_yylex_star_equals
    util_lex_token("a *= ",
                   :tIDENTIFIER, t("a"),
                   :tOP_ASGN, t("*"))

    assert_equal :expr_beg, @lex.lex_state
  end

  def test_yylex_string_double
    util_lex_token('"string"',
                   :tSTRING_BEG,     t('"'),
                   :tSTRING_CONTENT, s(:str, "string"),
                   :tSTRING_END,     t('"'))
  end

  def test_yylex_string_double_escape_M
    util_lex_token('"\M-g"',
                   :tSTRING_BEG,     t('"'),
                   :tSTRING_CONTENT, s(:str, "\347"),
                   :tSTRING_END,     t('"'))
  end

  def test_yylex_string_double_escape_chars
    util_lex_token('"s\tri\ng"',
                   :tSTRING_BEG,     t('"'),
                   :tSTRING_CONTENT, s(:str, "s\tri\ng"),
                   :tSTRING_END,     t('"'))
  end

  def test_yylex_string_double_escape_hex
    util_lex_token('"n = \x61\x62\x63"',
                   :tSTRING_BEG,     t('"'),
                   :tSTRING_CONTENT, s(:str, "n = abc"),
                   :tSTRING_END,     t('"'))
  end

  def test_yylex_string_double_escape_octal
    util_lex_token('"n = \101\102\103"',
                   :tSTRING_BEG,     t('"'),
                   :tSTRING_CONTENT, s(:str, "n = ABC"),
                   :tSTRING_END,     t('"'))
  end

  def test_yylex_string_pct_Q
    util_lex_token("%Q[string]",
                   :tSTRING_BEG,     t("%Q["),
                   :tSTRING_CONTENT, s(:str, "string"),
                   :tSTRING_END,     t("]"))
  end

  def test_yylex_string_single
    util_lex_token("'string'",
                   :tSTRING_BEG,     t("'"),
                   :tSTRING_CONTENT, s(:str, "string"),
                   :tSTRING_END,     t("'"))
  end

  def test_yylex_string_single_escape_chars
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

  def test_yylex_tilde
    util_lex_token "~", :tTILDE, t("~")
  end

  def test_yylex_tilde_unary
    @lex.lex_state = :expr_fname
    util_lex_token "~@", :tTILDE, t("~")
  end

  def test_yylex_trinary
    util_lex_token("a ? b : c",
                   :tIDENTIFIER, t("a"),
                   "?", t("?"), # FIX
                   :tIDENTIFIER, t("b"),
                   ":", t(":"), # FIX
                   :tIDENTIFIER, t("c"))
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

