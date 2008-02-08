#!/usr/local/bin/ruby

require 'test/unit'
require 'ruby_lexer'

class TestRubyLexer < Test::Unit::TestCase
  def deny cond, msg = nil
    assert ! cond, msg
  end

  def setup
    @lex = RubyLexer.new
    @lex.src = "blah blah"
    @lex.lex_state = :expr_beg # HACK ? I have no idea actually
  end

  def test_advance
    assert @lex.advance # blah
    assert @lex.advance # blah
    deny   @lex.advance # nada
  end

  def test_parse_number
    @lex.src = '142'
    c = @lex.src.getch # FIX or remove this test... too prescriptive/brittle
    node = @lex.parse_number(c)
    assert_equal :tINTEGER, node
    assert_equal 142, @lex.yacc_value
  end

  def test_parse_quote
    @lex.src = 'blah)'
    node = @lex.parse_quote('(')
    assert_equal :tSTRING_BEG, node
    assert_equal s(:strterm, RubyLexer::STR_DQUOTE, ")", "("), @lex.lex_strterm
    assert_equal ["%)"], @lex.yacc_value.args # FIX double check this
  end

  def test_parse_quote_angle
    @lex.src = 'blah>'
    node = @lex.parse_quote('<')
    assert_equal :tSTRING_BEG, node
    assert_equal s(:strterm, RubyLexer::STR_DQUOTE, ">", "<"), @lex.lex_strterm
    assert_equal ["%>"], @lex.yacc_value.args # FIX double check this
  end

  def test_parse_quote_curly
    @lex.src = 'blah}'
    node = @lex.parse_quote('{')
    assert_equal :tSTRING_BEG, node
    assert_equal s(:strterm, RubyLexer::STR_DQUOTE, "}", "{"), @lex.lex_strterm
    assert_equal ["%}"], @lex.yacc_value.args # FIX double check this
  end

  def test_parse_quote_other
    @lex.src = 'blah%'
    node = @lex.parse_quote('%')
    assert_equal :tSTRING_BEG, node
    assert_equal s(:strterm, RubyLexer::STR_DQUOTE, "%", "\0"), @lex.lex_strterm
    assert_equal ["%%"], @lex.yacc_value.args # FIX double check this
  end

  def test_parse_string_double
    @lex.src = 'blah # blah"'
    lex_term = s(:strterm, RubyLexer::STR_DQUOTE, '"', "\0")
    @lex.lex_strterm = lex_term
    node = @lex.parse_string(@lex.lex_strterm)
    assert_equal :tSTRING_CONTENT, node
    assert_equal s(:str, "blah # blah"), @lex.yacc_value
    assert_equal lex_term, @lex.lex_strterm
  end

  def test_parse_string_double_inter
    @lex.src = 'a #$a"'
    lex_term = s(:strterm, RubyLexer::STR_DQUOTE, '"', "\0")
    @lex.lex_strterm = lex_term
    node = @lex.parse_string(@lex.lex_strterm)
    assert_equal :tSTRING_CONTENT, node
    assert_equal s(:str, "a "), @lex.yacc_value
    assert_equal lex_term, @lex.lex_strterm

    node = @lex.parse_string(@lex.lex_strterm)
    assert_equal :tSTRING_DVAR, node
    # $a
    assert_equal lex_term, @lex.lex_strterm

    @lex.src = 'b #@b"'
    lex_term = s(:strterm, RubyLexer::STR_DQUOTE, '"', "\0")
    @lex.lex_strterm = lex_term
    node = @lex.parse_string(@lex.lex_strterm)
    assert_equal :tSTRING_CONTENT, node
    assert_equal s(:str, "b "), @lex.yacc_value
    assert_equal lex_term, @lex.lex_strterm

    node = @lex.parse_string(@lex.lex_strterm)
    assert_equal :tSTRING_DVAR, node
    # @b
    assert_equal lex_term, @lex.lex_strterm

    @lex.src = 'c #{c}"'
    lex_term = s(:strterm, RubyLexer::STR_DQUOTE, '"', "\0")
    @lex.lex_strterm = lex_term
    node = @lex.parse_string(@lex.lex_strterm)
    assert_equal :tSTRING_CONTENT, node
    assert_equal s(:str, "c "), @lex.yacc_value
    assert_equal lex_term, @lex.lex_strterm

    node = @lex.parse_string(@lex.lex_strterm)
    assert_equal :tSTRING_DBEG, node
    # #{c}
    assert_equal lex_term, @lex.lex_strterm
  end

  def test_parse_string_single
    @lex.src = "blah blah'"
    lex_term = s(:strterm, RubyLexer::STR_SQUOTE, "'", "\0")
    @lex.lex_strterm = lex_term
    node = @lex.parse_string(@lex.lex_strterm)
    assert_equal :tSTRING_CONTENT, node
    assert_equal s(:str, "blah blah"), @lex.yacc_value
    assert_equal lex_term, @lex.lex_strterm
  end

  def test_read_escape
    util_escape "\\",   '\\'
    util_escape "\n",   'n'
    util_escape "\t",   't'
    util_escape "\r",   'r'
    util_escape "\f",   'f'
    util_escape "\13",  'v'
    util_escape "\0",   '0'
    util_escape "\07",  'a'
    util_escape "\007", 'a'
    util_escape "\033", 'e'
    util_escape "\377", '377'
    util_escape "\377", 'xff'
    util_escape "\010", 'b'
    util_escape " ",    's'
    util_escape "q",    'q' # plain vanilla escape
  end

  def test_read_escape_c
    util_escape "\030", "C-x"
    util_escape "\030", "cx"
    util_escape "\230", 'C-\M-x'
    util_escape "\230", 'c\M-x'

    util_escape "\177", "C-?"
    util_escape "\177", "c?"
  end

  def test_read_escape_errors
    util_escape_bad ""

    util_escape_bad "M"
    util_escape_bad "M-"
    util_escape_bad "Mx"

    util_escape_bad "Cx"
    util_escape_bad "C"
    util_escape_bad "C-"

    util_escape_bad "c"
  end

  def test_read_escape_m
    util_escape "\370", "M-x"
    util_escape "\230", 'M-\C-x'
    util_escape "\230", 'M-\cx'
  end

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

  def test_yylex_back_ref
    util_lex_token("[$&, $`, $', $+]",
                   :tLBRACK,   t("["),
                   :tBACK_REF, s(:back_ref, :"&"), ",", t(","),
                   :tBACK_REF, s(:back_ref, :"`"), ",", t(","),
                   :tBACK_REF, s(:back_ref, :"'"), ",", t(","),
                   :tBACK_REF, s(:back_ref, :"+"),
                   :tRBRACK,   t("]"))
  end

  def test_yylex_backtick
    util_lex_token("`ls`",
                   :tXSTRING_BEG, t("`"),
                   :tSTRING_CONTENT, s(:str, "ls"),
                   :tSTRING_END, t("`"))
  end

  def test_yylex_backtick_dot
    @lex.lex_state = :expr_dot
    util_lex_token("a.`(3)",
                   :tIDENTIFIER, t("a"),
                   :tDOT, t("."),
                   :tBACK_REF2, t("`"),
                   :tLPAREN2, t("("),
                   :tINTEGER, 3,
                   :tRPAREN, t(")"))
  end

  def test_yylex_backtick_method
    @lex.lex_state = :expr_fname
    util_lex_token("`", :tBACK_REF2, t("`"))
    assert_equal :expr_end, @lex.lex_state
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
    util_lex_token("::Array",
                   :tCOLON3, t("::"),
                   :tCONSTANT, t("Array"))
  end

  def test_yylex_comma
    util_lex_token ",", ",", t(",") # FIX
  end

  def test_yylex_comment
    util_lex_token("1 # one\n# two\n2",
                   :tINTEGER, 1,
                   "\n", 1,
                   :tINTEGER, 2)
    assert_equal "# one\n# two\n", @lex.comments
  end

  def test_yylex_comment_begin
    util_lex_token("=begin\nblah\nblah\n=end\n42",
                   :tINTEGER, 42)
    assert_equal "=begin\nblah\nblah\n=end\n", @lex.comments
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

  def test_yylex_cvar
    util_lex_token "@@blah", :tCVAR, t("@@blah")
  end

  def test_yylex_cvar_bad
    assert_raises SyntaxError do
      util_lex_token "@@1"
    end
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

  def test_yylex_dollar
    util_lex_token("$", '$', t("$")) # FIX: wtf is this?!?
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

  def test_yylex_float_dot_e
    util_lex_token "1.0e10", :tFLOAT, 1.0e10
  end

  def test_yylex_float_bad_trailing_underscore
    util_bad_token "123_.0"
  end

  def test_yylex_float_call
    util_lex_token("1.0.to_s",
                   :tFLOAT, 1.0,
                   :tDOT, t('.'),
                   :tIDENTIFIER, t('to_s'))
  end

  def test_yylex_float_e
    util_lex_token "1e10", :tFLOAT, 1e10
  end

  def test_yylex_float_e_bad_double_e
    util_bad_token "1e2e3"
  end

  def test_yylex_float_e_bad_trailing_underscore
    util_bad_token "123_e10"
  end

  def test_yylex_float_e_minus
    util_lex_token "1e-10", :tFLOAT, 1e-10
  end

  def test_yylex_float_e_neg
    util_lex_token("-1e10",
                   :tUMINUS_NUM, t("-"),
                   :tFLOAT, 1e10)
  end

  def test_yylex_float_e_neg_minus
    util_lex_token("-1e-10",
                   :tUMINUS_NUM, t("-"),
                   :tFLOAT, 1e-10)
  end

  def test_yylex_float_e_neg_plus
    util_lex_token("-1e+10",
                   :tUMINUS_NUM, t("-"),
                   :tFLOAT, 1e10)
  end

  def test_yylex_float_e_plus
    util_lex_token "1e+10", :tFLOAT, 1e10
  end

  def test_yylex_float_e_zero
    util_lex_token "0e0", :tFLOAT, 0e0
  end

  def test_yylex_float_neg
    util_lex_token("-1.0",
                   :tUMINUS_NUM, t("-"),
                   :tFLOAT, 1.0)
  end

  def test_yylex_ge
    util_lex_token("a >= 2",
                   :tIDENTIFIER, t("a"),
                   :tGEQ, t(">="),
                   :tINTEGER, 2)
  end

  def test_yylex_global
    util_lex_token("$blah", :tGVAR, t("$blah"))
  end

  def test_yylex_global_backref
    @lex.lex_state = :expr_fname
    util_lex_token("$`", :tGVAR, t("$`"))
  end

  def test_yylex_global_dash_something
    util_lex_token('$-x', :tGVAR, t("$-x"))
  end

  def test_yylex_global_other
    util_lex_token('[$~, $*, $$, $?, $!, $@, $/, $\, $;, $,, $., $=, $:, $<, $>, $"]',
                   :tLBRACK, t("["),
                   :tGVAR,   t("$~"),  ",", t(","),
                   :tGVAR,   t("$*"),  ",", t(","),
                   :tGVAR,   t("$$"),  ",", t(","),
                   :tGVAR,   t("$?"),  ",", t(","),
                   :tGVAR,   t("$!"),  ",", t(","),
                   :tGVAR,   t("$@"),  ",", t(","),
                   :tGVAR,   t("$/"),  ",", t(","),
                   :tGVAR,   t("$\\"), ",", t(","),
                   :tGVAR,   t("$;"),  ",", t(","),
                   :tGVAR,   t("$,"),  ",", t(","),
                   :tGVAR,   t("$."),  ",", t(","),
                   :tGVAR,   t("$="),  ",", t(","),
                   :tGVAR,   t("$:"),  ",", t(","),
                   :tGVAR,   t("$<"),  ",", t(","),
                   :tGVAR,   t("$>"),  ",", t(","),
                   :tGVAR,   t("$\""),
                   :tRBRACK, t("]"))
  end

  def test_yylex_global_underscore
    util_lex_token("$_",
                   :tGVAR,     t("$_"))
  end

  def test_yylex_global_wierd
    util_lex_token("$__blah",
                   :tGVAR,     t("$__blah"))
  end

  def test_yylex_global_zero
    util_lex_token('$0', :tGVAR, t("$0"))
  end

  def test_yylex_gt
    util_lex_token("a > 2",
                   :tIDENTIFIER, t("a"),
                   :tGT, t(">"),
                   :tINTEGER, 2)
  end

  def test_yylex_heredoc_backtick
    util_lex_token("a = <<`EOF`\n  blah blah\nEOF\n",
                   :tIDENTIFIER,     t("a"),
                   "=",              t("="),
                   :tXSTRING_BEG,    t("`"),
                   :tSTRING_CONTENT, s(:str, "  blah blah\n"),
                   :tSTRING_END,     t("EOF"),
                   "\n",             t("EOF"))
  end

  def test_yylex_heredoc_bad_eos
    util_bad_token("a = <<EOF",
                   :tIDENTIFIER,     t("a"),
                   "=",              t("="),
                   :tSTRING_BEG,     t("\""))
  end

  def test_yylex_heredoc_double
    util_lex_token("a = <<\"EOF\"\n  blah blah\nEOF\n",
                   :tIDENTIFIER,     t("a"),
                   "=",              t("="),
                   :tSTRING_BEG,     t("\""),
                   :tSTRING_CONTENT, s(:str, "  blah blah\n"),
                   :tSTRING_END,     t("EOF"),
                   "\n",             t("EOF"))
  end

  def test_yylex_heredoc_double_dash
    util_lex_token("a = <<-\"EOF\"\n  blah blah\n  EOF\n",
                   :tIDENTIFIER,     t("a"),
                   "=",              t("="),
                   :tSTRING_BEG,     t("\""),
                   :tSTRING_CONTENT, s(:str, "  blah blah\n"),
                   :tSTRING_END,     t("EOF"),
                   "\n",             t("EOF"))
  end

  def test_yylex_heredoc_double_interp
    util_lex_token("a = <<\"EOF\"\n#x a \#@a b \#$b c \#{3} \nEOF\n",
                   :tIDENTIFIER,     t("a"),
                   "=",              t("="),
                   :tSTRING_BEG,     t("\""),
                   :tSTRING_CONTENT, s(:str, "#x a "),
                   :tSTRING_DVAR,    t("\#@"),
                   :tSTRING_CONTENT, s(:str, "@a b "), # HUH?
                   :tSTRING_DVAR,    t("\#$"),
                   :tSTRING_CONTENT, s(:str, "$b c "), # HUH?
                   :tSTRING_DBEG,    t("\#{"),
                   :tSTRING_CONTENT, s(:str, "3} \n"), # HUH?
                   :tSTRING_END,     t("EOF"),
                   "\n",             t("EOF"))
  end

  def test_yylex_heredoc_single
    util_lex_token("a = <<'EOF'\n  blah blah\nEOF\n",
                   :tIDENTIFIER,     t("a"),
                   "=",              t("="),
                   :tSTRING_BEG,     t("\""),
                   :tSTRING_CONTENT, s(:str, "  blah blah\n"),
                   :tSTRING_END,     t("EOF"),
                   "\n",             t("EOF"))
  end

  def test_yylex_heredoc_single_dash
    util_lex_token("a = <<-'EOF'\n  blah blah\n  EOF\n",
                   :tIDENTIFIER,     t("a"),
                   "=",              t("="),
                   :tSTRING_BEG,     t("\""),
                   :tSTRING_CONTENT, s(:str, "  blah blah\n"),
                   :tSTRING_END,     t("EOF"),
                   "\n",             t("EOF"))
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

  def test_yylex_integer_bin_bad_none
    util_bad_token "0b "
  end

  def test_yylex_integer_bin_bad_underscores
    util_bad_token "0b10__01"
  end

  def test_yylex_integer_dec
    util_lex_token "42", :tINTEGER, 42
  end

  def test_yylex_integer_dec_bad_underscores
    util_bad_token "42__24"
  end

  def test_yylex_integer_dec_d
    util_lex_token "0d42", :tINTEGER, 42
  end

  def test_yylex_integer_dec_d_bad_none
    util_bad_token "0d"
  end

  def test_yylex_integer_dec_d_bad_underscores
    util_bad_token "0d42__24"
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

  def test_yylex_integer_hex_bad_none
    util_bad_token "0x "
  end

  def test_yylex_integer_hex_bad_underscores
    util_bad_token "0xab__cd"
  end

  def test_yylex_integer_oct
    util_lex_token "052", :tINTEGER, 42
  end

  def test_yylex_integer_oct_bad_range
    util_bad_token "08"
  end

  def test_yylex_integer_oct_bad_underscores
    util_bad_token "01__23"
  end

  def test_yylex_integer_oct_o
    util_lex_token "0o52", :tINTEGER, 42
  end

  def test_yylex_integer_oct_o_bad_range
    util_bad_token "0o8"
  end

  def test_yylex_integer_oct_o_bad_underscores
    util_bad_token "0o1__23"
  end

  def test_yylex_integer_oct_o_not_bad_none
    util_lex_token "0o ", :tINTEGER, 0
  end

  def test_yylex_integer_trailing
    util_lex_token("1.to_s",
                   :tINTEGER, 1,
                   :tDOT, t('.'),
                   :tIDENTIFIER, t('to_s'))
  end

  def test_yylex_integer_underscore
    util_lex_token "4_2", :tINTEGER, 42
  end

  def test_yylex_integer_underscore_bad
    util_bad_token "4__2"
  end

  def test_yylex_integer_zero
    util_lex_token "0", :tINTEGER, 0
  end

  def test_yylex_ivar
    util_lex_token "@blah", :tIVAR, t("@blah")
  end

  def test_yylex_ivar_bad
    assert_raises SyntaxError do
      util_lex_token "@1"
    end
  end

  def test_yylex_lt
    util_lex_token "<", :tLT, t("<")
  end

  def test_yylex_lt2
    util_lex_token("a <\< b",
                   :tIDENTIFIER, t("a"),
                   :tLSHFT, t("<\<"),
                   :tIDENTIFIER, t("b"))

  end

  def test_yylex_lt2_equals
    util_lex_token("a <\<= b",
                   :tIDENTIFIER, t("a"),
                   :tOP_ASGN, t("<\<"),
                   :tIDENTIFIER, t("b"))
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

  def test_yylex_minus_method
    @lex.lex_state = :expr_fname
    util_lex_token "-", :tMINUS, t("-")
  end

  def test_yylex_minus_unary_method
    @lex.lex_state = :expr_fname
    util_lex_token "-@", :tUMINUS, t("-@")
  end

  def test_yylex_minus_unary_number
    util_lex_token("-42",
                   :tUMINUS_NUM, t("-"),
                   :tINTEGER, 42)
  end

  def test_yylex_nth_ref
    util_lex_token('[$1, $2, $3, $4, $5, $6, $7, $8, $9]',
                   :tLBRACK,  t("["),
                   :tNTH_REF, s(:nth_ref, 1), ",", t(","),
                   :tNTH_REF, s(:nth_ref, 2), ",", t(","),
                   :tNTH_REF, s(:nth_ref, 3), ",", t(","),
                   :tNTH_REF, s(:nth_ref, 4), ",", t(","),
                   :tNTH_REF, s(:nth_ref, 5), ",", t(","),
                   :tNTH_REF, s(:nth_ref, 6), ",", t(","),
                   :tNTH_REF, s(:nth_ref, 7), ",", t(","),
                   :tNTH_REF, s(:nth_ref, 8), ",", t(","),
                   :tNTH_REF, s(:nth_ref, 9),
                   :tRBRACK,  t("]"))
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

  def test_yylex_plus_method
    @lex.lex_state = :expr_fname
    util_lex_token "+", :tPLUS, t("+")
  end

  def test_yylex_plus_unary_method
    @lex.lex_state = :expr_fname
    util_lex_token "+@", :tUPLUS, t("+@")
  end

  def test_yylex_plus_unary_number
    util_lex_token("+42",
                   :tINTEGER, 42)
  end

  def test_yylex_question
    util_lex_token "?*", :tINTEGER, 42
  end

  def test_yylex_question_ws
    util_lex_token "? ",  "?", t("?")
    util_lex_token "?\n", "?", t("?")
    util_lex_token "?\t", "?", t("?")
    util_lex_token "?\v", "?", t("?")
    util_lex_token "?\r", "?", t("?")
    util_lex_token "?\f", "?", t("?")
  end

  def test_yylex_question_ws_backslashed
    @lex.lex_state = :expr_beg
    util_lex_token "?\\ ", :tINTEGER, 32
    @lex.lex_state = :expr_beg
    util_lex_token "?\\n", :tINTEGER, 10
    @lex.lex_state = :expr_beg
    util_lex_token "?\\t", :tINTEGER, 9
    @lex.lex_state = :expr_beg
    util_lex_token "?\\v", :tINTEGER, 11
    @lex.lex_state = :expr_beg
    util_lex_token "?\\r", :tINTEGER, 13
    @lex.lex_state = :expr_beg
    util_lex_token "?\\f", :tINTEGER, 12
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

  def test_yylex_regexp_bad
    util_bad_token("/.*/xyz",
                   :tREGEXP_BEG, t("/"),
                   :tSTRING_CONTENT, s(:str, ".*"))
  end

  def test_yylex_regexp_escape_C
    util_lex_token('/regex\\C-x/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\C-x"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_C_M
    util_lex_token('/regex\\C-\\M-x/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\C-\\M-x"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_C_M_craaaazy
    util_lex_token("/regex\\C-\\\n\\M-x/",
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\C-\\M-x"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_C_bad_dash
    util_bad_token '/regex\\Cx/', :tREGEXP_BEG, t("/")
  end

  def test_yylex_regexp_escape_C_bad_dash_eos
    util_bad_token '/regex\\C-/', :tREGEXP_BEG, t("/")
  end

  def test_yylex_regexp_escape_C_bad_dash_eos2
    util_bad_token '/regex\\C-', :tREGEXP_BEG, t("/")
  end

  def test_yylex_regexp_escape_C_bad_eos
    util_bad_token '/regex\\C/', :tREGEXP_BEG, t("/")
  end

  def test_yylex_regexp_escape_C_bad_eos2
    util_bad_token '/regex\\c', :tREGEXP_BEG, t("/")
  end

  def test_yylex_regexp_escape_M
    util_lex_token('/regex\\M-x/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\M-x"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_M_C
    util_lex_token('/regex\\M-\\C-x/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\M-\\C-x"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_M_bad_dash
    util_bad_token '/regex\\Mx/', :tREGEXP_BEG, t("/")
  end

  def test_yylex_regexp_escape_M_bad_dash_eos
    util_bad_token '/regex\\M-/', :tREGEXP_BEG, t("/")
  end

  def test_yylex_regexp_escape_M_bad_dash_eos2
    util_bad_token '/regex\\M-', :tREGEXP_BEG, t("/")
  end

  def test_yylex_regexp_escape_M_bad_eos
    util_bad_token '/regex\\M/', :tREGEXP_BEG, t("/")
  end

  def test_yylex_regexp_escape_bad_eos
    util_bad_token '/regex\\', :tREGEXP_BEG, t("/")
  end

  def test_yylex_regexp_escape_c
    util_lex_token('/regex\\cxxx/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\cxxx"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_c_backslash
    util_lex_token('/regex\\c\\n/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\c\\n"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_chars
    util_lex_token('/re\\tge\\nxp/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "re\\tge\\nxp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_hex
    util_lex_token('/regex\\x61xp/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\x61xp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_hex_bad
    util_bad_token '/regex\\x6zxp/', :tREGEXP_BEG, t("/")
  end

  def test_yylex_regexp_escape_oct1
    util_lex_token('/regex\\0xp/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\0xp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_oct2
    util_lex_token('/regex\\07xp/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\07xp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_oct3
    util_lex_token('/regex\\10142/',
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\10142"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_return
    util_lex_token("/regex\\\nregex/",
                   :tREGEXP_BEG,     t("/"),
                   :tSTRING_CONTENT, s(:str, "regexregex"),
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
    util_lex_token('"\\M-g"',
                   :tSTRING_BEG,     t('"'),
                   :tSTRING_CONTENT, s(:str, "\347"),
                   :tSTRING_END,     t('"'))
  end

  def test_yylex_string_double_escape_chars
    util_lex_token('"s\\tri\\ng"',
                   :tSTRING_BEG,     t('"'),
                   :tSTRING_CONTENT, s(:str, "s\tri\ng"),
                   :tSTRING_END,     t('"'))
  end

  def test_yylex_string_double_escape_hex
    util_lex_token('"n = \\x61\\x62\\x63"',
                   :tSTRING_BEG,     t('"'),
                   :tSTRING_CONTENT, s(:str, "n = abc"),
                   :tSTRING_END,     t('"'))
  end

  def test_yylex_string_double_escape_octal
    util_lex_token('"n = \\101\\102\\103"',
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

  def test_yylex_ternary
    util_lex_token("a ? b : c",
                   :tIDENTIFIER, t("a"),
                   "?", t("?"), # FIX
                   :tIDENTIFIER, t("b"),
                   ":", t(":"), # FIX
                   :tIDENTIFIER, t("c"))

    util_lex_token("a ?bb : c", # GAH! MATZ!!!
                   :tIDENTIFIER, t("a"),
                   "?", t("?"), # FIX
                   :tIDENTIFIER, t("bb"),
                   ":", t(":"), # FIX
                   :tIDENTIFIER, t("c"))

    util_lex_token("42 ?", # 42 forces expr_end
                   :tINTEGER, 42,
                   "?", t("?"))
  end

  def test_yylex_tilde
    util_lex_token "~", :tTILDE, t("~")
  end

  def test_yylex_tilde_unary
    @lex.lex_state = :expr_fname
    util_lex_token "~@", :tTILDE, t("~")
  end

  def test_yylex_underscore
    util_lex_token("_var", :tIDENTIFIER, t("_var"))
  end

  def test_yylex_underscore_end
    @lex.src = "__END__\n"
    deny @lex.advance
  end

  def test_zbug_id_equals
    util_lex_token("a =",
                   :tIDENTIFIER, t("a"),
                   '=', t("="))

    assert_equal :expr_beg, @lex.lex_state

    util_lex_token("0.0",
                   :tFLOAT, 0.0)
  end

  def test_zbug_no_spaces_in_decl
    util_lex_token("def initialize(u=",
                   :kDEF, t("def"),
                   :tIDENTIFIER, t("initialize"),
                   :tLPAREN2, t("("),
                   :tIDENTIFIER, t("u"),
                   '=', t("="))

    assert_equal :expr_beg, @lex.lex_state

    util_lex_token("0.0,s=0.0",
                   :tFLOAT, 0.0,
                   ',', t(','),
                   :tIDENTIFIER, t("s"),
                   '=', t("="),
                   :tFLOAT, 0.0)
  end

  def test_zbug_float_in_decl
    util_lex_token("def initialize(u = ",
                   :kDEF, t("def"),
                   :tIDENTIFIER, t("initialize"),
                   :tLPAREN2, t("("),
                   :tIDENTIFIER, t("u"),
                   '=', t("="))

    assert_equal :expr_beg, @lex.lex_state

    util_lex_token("0.0, s = 0.0",
                   :tFLOAT, 0.0,
                   ',', t(','),
                   :tIDENTIFIER, t("s"),
                   '=', t("="),
                   :tFLOAT, 0.0)
  end

  ############################################################

  def util_bad_token s, *args
    assert_raises SyntaxError do
      util_lex_token s, *args
    end
  end

  def util_escape expected, input
    @lex.src = input
    assert_equal expected, @lex.read_escape
  end

  def util_escape_bad input
    @lex.src = input
    assert_raises SyntaxError do
      @lex.read_escape
    end
  end

  def util_lex_token input, *args
    @lex.src = input

    until args.empty? do
      token = args.shift
      value = args.shift
      assert @lex.advance, "no more tokens"
      assert_equal [token, value], [@lex.token, @lex.yacc_value]
    end

    deny @lex.advance, "must be empty, but had #{[@lex.token, @lex.yacc_value].inspect}"
  end
end

