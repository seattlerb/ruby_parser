#!/usr/local/bin/ruby

require 'test/unit'
require 'ruby_lexer'

class TestRubyLexer < Test::Unit::TestCase
  def deny cond, msg = nil
    assert ! cond, msg
  end

  def setup
    p = RubyParser.new
    @lex = p.lexer
    @lex.src = "blah blah"
    @lex.lex_state = :expr_beg
  end

  def test_advance
    assert @lex.advance # blah
    assert @lex.advance # blah
    deny   @lex.advance # nada
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

  def test_yylex_ambiguous_uminus
    util_lex_token("m -3",
                   :tIDENTIFIER, s("m"),
                   :tUMINUS_NUM, s("-"),
                   :tINTEGER, 3)
    # TODO: verify warning
  end

  def test_yylex_ambiguous_uplus
    util_lex_token("m +3",
                   :tIDENTIFIER, s("m"),
                   :tINTEGER, 3)
    # TODO: verify warning
  end

  def test_yylex_and
    util_lex_token "&", :tAMPER, s("&")
  end

  def test_yylex_and2
    util_lex_token "&&", :tANDOP, s("&&")
  end

  def test_yylex_and2_equals
    util_lex_token "&&=", :tOP_ASGN, s("&&")
  end

  def test_yylex_and_arg
    @lex.lex_state = :expr_arg

    util_lex_token(" &y",
                   :tAMPER, s("&"),
                   :tIDENTIFIER, s("y"))
  end

  def test_yylex_and_equals
    util_lex_token "&=", :tOP_ASGN, s("&")
  end

  def test_yylex_and_expr
    @lex.lex_state = :expr_arg

    util_lex_token("x & y",
                   :tIDENTIFIER, s("x"),
                   :tAMPER2, s("&"),
                   :tIDENTIFIER, s("y"))
  end

  def test_yylex_and_meth
    util_lex_fname "&", :tAMPER2
  end

  def test_yylex_assoc
    util_lex_token "=>", :tASSOC, s("=>")
  end

  def test_yylex_at
    util_lex_token " @ ", "@", s("@")
  end

  def test_yylex_back_ref
    util_lex_token("[$&, $`, $', $+]",
                   :tLBRACK,   s("["),
                   :tBACK_REF, s(:back_ref, :"&"), ",", s(","),
                   :tBACK_REF, s(:back_ref, :"`"), ",", s(","),
                   :tBACK_REF, s(:back_ref, :"'"), ",", s(","),
                   :tBACK_REF, s(:back_ref, :"+"),
                   :tRBRACK,   s("]"))
  end

  def test_yylex_backslash
    util_lex_token("1 \\\n+ 2",
                   :tINTEGER, 1,
                   :tPLUS, s("+"),
                   :tINTEGER, 2)
  end

  def test_yylex_backslash_bad
    util_bad_token("1 \\ + 2",
                   :tINTEGER, 1)
  end

  def test_yylex_backtick
    util_lex_token("`ls`",
                   :tXSTRING_BEG, s("`"),
                   :tSTRING_CONTENT, s(:str, "ls"),
                   :tSTRING_END, s("`"))
  end

  def test_yylex_backtick_cmdarg
    @lex.lex_state = :expr_dot
    util_lex_token("\n`", :tBACK_REF2, s("`")) # \n ensures expr_cmd

    assert_equal :expr_cmdarg, @lex.lex_state
  end

  def test_yylex_backtick_dot
    @lex.lex_state = :expr_dot
    util_lex_token("a.`(3)",
                   :tIDENTIFIER, s("a"),
                   :tDOT, s("."),
                   :tBACK_REF2, s("`"),
                   :tLPAREN2, s("("),
                   :tINTEGER, 3,
                   :tRPAREN, s(")"))
  end

  def test_yylex_backtick_method
    @lex.lex_state = :expr_fname
    util_lex_token("`", :tBACK_REF2, s("`"))
    assert_equal :expr_end, @lex.lex_state
  end

  def test_yylex_bad_char
    util_bad_token(" \010 ")
  end

  def test_yylex_bang
    util_lex_token "!", :tBANG, s("!")
  end

  def test_yylex_bang_equals
    util_lex_token "!=", :tNEQ, s("!=")
  end

  def test_yylex_bang_tilde
    util_lex_token "!~", :tNMATCH, s("!~")
  end

  def test_yylex_carat
    util_lex_token "^", :tCARET, s("^")
  end

  def test_yylex_carat_equals
    util_lex_token "^=", :tOP_ASGN, s("^")
  end

  def test_yylex_colon2
    util_lex_token("A::B",
                   :tCONSTANT, s("A"),
                   :tCOLON2,   s("::"),
                   :tCONSTANT, s("B"))
  end

  def test_yylex_colon3
    util_lex_token("::Array",
                   :tCOLON3, s("::"),
                   :tCONSTANT, s("Array"))
  end

  def test_yylex_comma
    util_lex_token ",", ",", s(",") # FIX
  end

  def test_yylex_comment
    util_lex_token("1 # one\n# two\n2",
                   :tINTEGER, 1,
                   "\n", nil,
                   :tINTEGER, 2)
    assert_equal "# one\n# two\n", @lex.comments
  end

  def test_yylex_comment_begin
    util_lex_token("=begin\nblah\nblah\n=end\n42",
                   :tINTEGER, 42)
    assert_equal "=begin\nblah\nblah\n=end\n", @lex.comments
  end

  def test_yylex_comment_begin_bad
    util_bad_token("=begin\nblah\nblah\n")
    assert_equal '', @lex.comments
  end

  def test_yylex_comment_begin_not_comment
    util_lex_token("beginfoo = 5\np x \\\n=beginfoo",
                   :tIDENTIFIER, s("beginfoo"),
                   '=',          s('='),
                   :tINTEGER,    5,
                   "\n",         nil,
                   :tIDENTIFIER, s("p"),
                   :tIDENTIFIER, s("x"),
                   '=',          s('='),
                   :tIDENTIFIER, s("beginfoo"))
  end

  def test_yylex_comment_begin_space
    util_lex_token("=begin blah\nblah\n=end\n")
    assert_equal "=begin blah\nblah\n=end\n", @lex.comments
  end

  def test_yylex_comment_eos
    util_lex_token("# comment")
  end

  def test_yylex_constant
    util_lex_token("ArgumentError",
                   :tCONSTANT, s("ArgumentError"))
  end

  def test_yylex_constant_semi
    util_lex_token("ArgumentError;",
                   :tCONSTANT, s("ArgumentError"),
                   ";", s(";"))
  end

  def test_yylex_cvar
    util_lex_token "@@blah", :tCVAR, s("@@blah")
  end

  def test_yylex_cvar_bad
    assert_raises SyntaxError do
      util_lex_token "@@1"
    end
  end

  def test_yylex_def_bad_name
    @lex.lex_state = :expr_fname
    util_bad_token("def [ ", :kDEF, s("def"))
  end

  def test_yylex_div
    util_lex_token("a / 2",
                   :tIDENTIFIER, s("a"),
                   :tDIVIDE, s("/"),
                   :tINTEGER, 2)
  end

  def test_yylex_div_equals
    util_lex_token("a /= 2",
                   :tIDENTIFIER, s("a"),
                   :tOP_ASGN, s("/"),
                   :tINTEGER, 2)
  end

  def test_yylex_do
    util_lex_token("x do 42 end",
                   :tIDENTIFIER, s("x"),
                   :kDO, s("do"),
                   :tINTEGER, 42,
                   :kEND, s("end"))
  end

  def test_yylex_do_block
    @lex.lex_state = :expr_endarg
    @lex.cmdarg.push true

    util_lex_token("x.y do 42 end",
                   :tIDENTIFIER, s("x"),
                   :tDOT, s("."),
                   :tIDENTIFIER, s("y"),
                   :kDO_BLOCK, s("do"),
                   :tINTEGER, 42,
                   :kEND, s("end"))
  end

  def test_yylex_do_block2
    @lex.lex_state = :expr_endarg

    util_lex_token("do 42 end",
                   :kDO_BLOCK, s("do"),
                   :tINTEGER, 42,
                   :kEND, s("end"))
  end

  def test_yylex_do_cond
    @lex.cond.push true

    util_lex_token("x do 42 end",
                   :tIDENTIFIER, s("x"),
                   :kDO_COND, s("do"),
                   :tINTEGER, 42,
                   :kEND, s("end"))
  end

  def test_yylex_dollar
    util_lex_token('$', '$', s('$')) # FIX: wtf is this?!?
  end

  def test_yylex_dot # HINT message sends
    util_lex_token ".", :tDOT, s(".")
  end

  def test_yylex_dot2
    util_lex_token "..", :tDOT2, s("..")
  end

  def test_yylex_dot3
    util_lex_token "...", :tDOT3, s("...")
  end

  def test_yylex_equals
    util_lex_token "=", '=', s("=") # FIX: this sucks
  end

  def test_yylex_equals2
    util_lex_token "==", :tEQ, s("==")
  end

  def test_yylex_equals3
    util_lex_token "===", :tEQQ, s("===")
  end

  def test_yylex_equals_tilde
    util_lex_token "=~", :tMATCH, s("=~")
  end

  def test_yylex_float
    util_lex_token "1.0", :tFLOAT, 1.0
  end

  def test_yylex_float_bad_no_underscores
    util_bad_token "1__0.0"
  end

  def test_yylex_float_bad_no_zero_leading
    util_bad_token ".0"
  end

  def test_yylex_float_bad_trailing_underscore
    util_bad_token "123_.0"
  end

  def test_yylex_float_call
    util_lex_token("1.0.to_s",
                   :tFLOAT, 1.0,
                   :tDOT, s('.'),
                   :tIDENTIFIER, s('to_s'))
  end

  def test_yylex_float_dot_E
    util_lex_token "1.0E10", :tFLOAT, 1.0e10
  end

  def test_yylex_float_dot_E_neg
    util_lex_token("-1.0E10",
                   :tUMINUS_NUM, s("-"),
                   :tFLOAT, 1.0e10)
  end

  def test_yylex_float_dot_e
    util_lex_token "1.0e10", :tFLOAT, 1.0e10
  end

  def test_yylex_float_dot_e_neg
    util_lex_token("-1.0e10",
                   :tUMINUS_NUM, s("-"),
                   :tFLOAT, 1.0e10)
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
                   :tUMINUS_NUM, s("-"),
                   :tFLOAT, 1e10)
  end

  def test_yylex_float_e_neg_minus
    util_lex_token("-1e-10",
                   :tUMINUS_NUM, s("-"),
                   :tFLOAT, 1e-10)
  end

  def test_yylex_float_e_neg_plus
    util_lex_token("-1e+10",
                   :tUMINUS_NUM, s("-"),
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
                   :tUMINUS_NUM, s("-"),
                   :tFLOAT, 1.0)
  end

  def test_yylex_ge
    util_lex_token("a >= 2",
                   :tIDENTIFIER, s("a"),
                   :tGEQ, s(">="),
                   :tINTEGER, 2)
  end

  def test_yylex_global
    util_lex_token("$blah", :tGVAR, s("$blah"))
  end

  def test_yylex_global_backref
    @lex.lex_state = :expr_fname
    util_lex_token("$`", :tGVAR, s("$`"))
  end

  def test_yylex_global_dash_nothing
    util_lex_token('$- ', :tGVAR, s("$-"))
  end

  def test_yylex_global_dash_something
    util_lex_token('$-x', :tGVAR, s("$-x"))
  end

  def test_yylex_global_number
    @lex.lex_state = :expr_fname
    util_lex_token("$1", :tGVAR, s("$1"))
  end

  def test_yylex_global_number_big
    @lex.lex_state = :expr_fname
    util_lex_token("$1234", :tGVAR, s("$1234"))
  end

  def test_yylex_global_other
    util_lex_token('[$~, $*, $$, $?, $!, $@, $/, $\, $;, $,, $., $=, $:, $<, $>, $"]',
                   :tLBRACK, s("["),
                   :tGVAR,   s("$~"),  ",", s(","),
                   :tGVAR,   s("$*"),  ",", s(","),
                   :tGVAR,   s("$$"),  ",", s(","),
                   :tGVAR,   s("$?"),  ",", s(","),
                   :tGVAR,   s("$!"),  ",", s(","),
                   :tGVAR,   s("$@"),  ",", s(","),
                   :tGVAR,   s("$/"),  ",", s(","),
                   :tGVAR,   s("$\\"), ",", s(","),
                   :tGVAR,   s("$;"),  ",", s(","),
                   :tGVAR,   s("$,"),  ",", s(","),
                   :tGVAR,   s("$."),  ",", s(","),
                   :tGVAR,   s("$="),  ",", s(","),
                   :tGVAR,   s("$:"),  ",", s(","),
                   :tGVAR,   s("$<"),  ",", s(","),
                   :tGVAR,   s("$>"),  ",", s(","),
                   :tGVAR,   s("$\""),
                   :tRBRACK, s("]"))
  end

  def test_yylex_global_underscore
    util_lex_token("$_",
                   :tGVAR,     s("$_"))
  end

  def test_yylex_global_wierd
    util_lex_token("$__blah",
                   :tGVAR,     s("$__blah"))
  end

  def test_yylex_global_zero
    util_lex_token('$0', :tGVAR, s("$0"))
  end

  def test_yylex_gt
    util_lex_token("a > 2",
                   :tIDENTIFIER, s("a"),
                   :tGT, s(">"),
                   :tINTEGER, 2)
  end

  def test_yylex_heredoc_backtick
    util_lex_token("a = <<`EOF`\n  blah blah\nEOF\n",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tXSTRING_BEG,    s("`"),
                   :tSTRING_CONTENT, s(:str, "  blah blah\n"),
                   :tSTRING_END,     s("EOF"),
                   "\n",             nil)
  end

  def test_yylex_heredoc_double
    util_lex_token("a = <<\"EOF\"\n  blah blah\nEOF\n",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""),
                   :tSTRING_CONTENT, s(:str, "  blah blah\n"),
                   :tSTRING_END,     s("EOF"),
                   "\n",             nil)
  end

  def test_yylex_heredoc_double_dash
    util_lex_token("a = <<-\"EOF\"\n  blah blah\n  EOF\n",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""),
                   :tSTRING_CONTENT, s(:str, "  blah blah\n"),
                   :tSTRING_END,     s("EOF"),
                   "\n",             nil)
  end

  def test_yylex_heredoc_double_eos
    util_bad_token("a = <<\"EOF\"\nblah",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""))
  end

  def test_yylex_heredoc_double_eos_nl
    util_bad_token("a = <<\"EOF\"\nblah\n",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""))
  end

  def test_yylex_heredoc_double_interp
    util_lex_token("a = <<\"EOF\"\n#x a \#@a b \#$b c \#{3} \nEOF\n",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""),
                   :tSTRING_CONTENT, s(:str, "#x a "),
                   :tSTRING_DVAR,    s("\#@"),
                   :tSTRING_CONTENT, s(:str, "@a b "), # HUH?
                   :tSTRING_DVAR,    s("\#$"),
                   :tSTRING_CONTENT, s(:str, "$b c "), # HUH?
                   :tSTRING_DBEG,    s("\#{"),
                   :tSTRING_CONTENT, s(:str, "3} \n"), # HUH?
                   :tSTRING_END,     s("EOF"),
                   "\n",             nil)
  end

  def test_yylex_heredoc_none
    util_lex_token("a = <<EOF\nblah\nblah\nEOF",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""),
                   :tSTRING_CONTENT, s(:str, "blah\nblah\n"),
                   :tSTRING_CONTENT, s(:str, ""),
                   :tSTRING_END,     s("EOF"),
                   "\n",             nil)
  end

  def test_yylex_heredoc_none_bad_eos
    util_bad_token("a = <<EOF",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""))
  end

  def test_yylex_heredoc_none_dash
    util_lex_token("a = <<-EOF\nblah\nblah\n  EOF",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""),
                   :tSTRING_CONTENT, s(:str, "blah\nblah\n"),
                   :tSTRING_CONTENT, s(:str, ""),
                   :tSTRING_END,     s("EOF"),
                   "\n",             nil)
  end

  def test_yylex_heredoc_single
    util_lex_token("a = <<'EOF'\n  blah blah\nEOF\n",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""),
                   :tSTRING_CONTENT, s(:str, "  blah blah\n"),
                   :tSTRING_END,     s("EOF"),
                   "\n",             nil)
  end

  def test_yylex_heredoc_single_bad_eos_body
    util_bad_token("a = <<'EOF'\nblah",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""))
  end

  def test_yylex_heredoc_single_bad_eos_empty
    util_bad_token("a = <<''\n",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""))
  end

  def test_yylex_heredoc_single_bad_eos_term
    util_bad_token("a = <<'EOF",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""))
  end

  def test_yylex_heredoc_single_bad_eos_term_nl
    util_bad_token("a = <<'EOF\ns = 'blah blah'",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""))
  end

  def test_yylex_heredoc_single_dash
    util_lex_token("a = <<-'EOF'\n  blah blah\n  EOF\n",
                   :tIDENTIFIER,     s("a"),
                   "=",              s("="),
                   :tSTRING_BEG,     s("\""),
                   :tSTRING_CONTENT, s(:str, "  blah blah\n"),
                   :tSTRING_END,     s("EOF"),
                   "\n",             nil)
  end

  def test_yylex_identifier
    util_lex_token("identifier", :tIDENTIFIER, s("identifier"))
  end

  def test_yylex_identifier_bang
    util_lex_token("identifier!", :tFID, s("identifier!"))
  end

  def test_yylex_identifier_cmp
    util_lex_fname "<=>", :tCMP
  end

  def test_yylex_identifier_def
    util_lex_fname "identifier", :tIDENTIFIER, :expr_end
  end

  def test_yylex_identifier_eh
    util_lex_token("identifier?", :tFID, s("identifier?"))
  end

  def test_yylex_identifier_equals_arrow
    @lex.lex_state = :expr_fname
    util_lex_token(":blah==>",
                   :tSYMBEG, s(":"),
                   :tIDENTIFIER, s("blah="),
                   :tASSOC, s("=>"))
  end

  def test_yylex_identifier_equals_caret
    util_lex_fname "^", :tCARET
  end

  def test_yylex_identifier_equals_def
    util_lex_fname "identifier=", :tIDENTIFIER, :expr_end
  end

  def test_yylex_identifier_equals_def2
    util_lex_fname "==", :tEQ
  end

  def test_yylex_identifier_equals_expr
    @lex.lex_state = :expr_dot
    util_lex_token("y = arg",
                   :tIDENTIFIER, s("y"),
                   "=", s("="),
                   :tIDENTIFIER, s("arg"))

    assert_equal :expr_arg, @lex.lex_state
  end

  def test_yylex_identifier_equals_or
    util_lex_fname "|", :tPIPE
  end

  def test_yylex_identifier_equals_slash
    util_lex_fname "/", :tDIVIDE
  end

  def test_yylex_identifier_equals_tilde
    @lex.lex_state = :expr_fname # can only set via parser's defs
    util_lex_token("identifier=~",
                   :tIDENTIFIER, s("identifier"),
                   :tMATCH, s("=~"))
  end

  def test_yylex_identifier_gt
    util_lex_fname ">", :tGT
  end

  def test_yylex_identifier_le
    util_lex_fname "<=", :tLEQ
  end

  def test_yylex_identifier_lt
    util_lex_fname "<", :tLT
  end

  def test_yylex_identifier_tilde
    util_lex_fname "~", :tTILDE
  end

  def test_yylex_index
    util_lex_fname "[]", :tAREF
  end

  def test_yylex_index_equals
    util_lex_fname "[]=", :tASET
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
                   :tDOT, s('.'),
                   :tIDENTIFIER, s('to_s'))
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
    util_lex_token "@blah", :tIVAR, s("@blah")
  end

  def test_yylex_ivar_bad
    assert_raises SyntaxError do
      util_lex_token "@1"
    end
  end

  def test_yylex_keyword_expr
    @lex.lex_state = :expr_endarg

    util_lex_token("if", :kIF_MOD, s("if"))

    assert_equal :expr_beg, @lex.lex_state
  end

  def test_yylex_lt
    util_lex_token "<", :tLT, s("<")
  end

  def test_yylex_lt2
    util_lex_token("a <\< b",
                   :tIDENTIFIER, s("a"),
                   :tLSHFT, s("<\<"),
                   :tIDENTIFIER, s("b"))

  end

  def test_yylex_lt2_equals
    util_lex_token("a <\<= b",
                   :tIDENTIFIER, s("a"),
                   :tOP_ASGN, s("<\<"),
                   :tIDENTIFIER, s("b"))
  end

  def test_yylex_lt_equals
    util_lex_token "<=", :tLEQ, s("<=")
  end

  def test_yylex_minus
    util_lex_token("1 - 2",
                   :tINTEGER, 1,
                   :tMINUS, s("-"),
                   :tINTEGER, 2)
  end

  def test_yylex_minus_equals
    util_lex_token "-=", :tOP_ASGN, s("-")
  end

  def test_yylex_minus_method
    @lex.lex_state = :expr_fname
    util_lex_token "-", :tMINUS, s("-")
  end

  def test_yylex_minus_unary_method
    @lex.lex_state = :expr_fname
    util_lex_token "-@", :tUMINUS, s("-@")
  end

  def test_yylex_minus_unary_number
    util_lex_token("-42",
                   :tUMINUS_NUM, s("-"),
                   :tINTEGER, 42)
  end

  def test_yylex_nth_ref
    util_lex_token('[$1, $2, $3, $4, $5, $6, $7, $8, $9]',
                   :tLBRACK,  s("["),
                   :tNTH_REF, s(:nth_ref, 1), ",", s(","),
                   :tNTH_REF, s(:nth_ref, 2), ",", s(","),
                   :tNTH_REF, s(:nth_ref, 3), ",", s(","),
                   :tNTH_REF, s(:nth_ref, 4), ",", s(","),
                   :tNTH_REF, s(:nth_ref, 5), ",", s(","),
                   :tNTH_REF, s(:nth_ref, 6), ",", s(","),
                   :tNTH_REF, s(:nth_ref, 7), ",", s(","),
                   :tNTH_REF, s(:nth_ref, 8), ",", s(","),
                   :tNTH_REF, s(:nth_ref, 9),
                   :tRBRACK,  s("]"))
  end

  def test_yylex_open_bracket
    util_lex_token("(", :tLPAREN, s("("))
  end

  def test_yylex_open_bracket_cmdarg
    @lex.lex_state = :expr_cmdarg
    util_lex_token(" (", :tLPAREN_ARG, s("("))
  end

  def test_yylex_open_bracket_exprarg
    @lex.lex_state = :expr_arg
    util_lex_token(" (", :tLPAREN2, s("("))
  end

  def test_yylex_open_curly_bracket
    util_lex_token("{",
                   :tLBRACE, s("{"))
  end

  def test_yylex_open_curly_bracket_arg
    @lex.lex_state = :expr_arg
    util_lex_token("m { 3 }",
                   :tIDENTIFIER, s("m"),
                   :tLCURLY, s("{"),
                   :tINTEGER, 3,
                   :tRCURLY, s("}"))
  end

  def test_yylex_open_curly_bracket_block
    @lex.lex_state = :expr_endarg # seen m(3)
    util_lex_token("{ 4 }",
                   :tLBRACE_ARG, s("{"),
                   :tINTEGER, 4,
                   :tRCURLY, s("}"))
  end

  def test_yylex_open_square_bracket_arg
    @lex.lex_state = :expr_arg
    util_lex_token("m [ 3 ]",
                   :tIDENTIFIER, s("m"),
                   :tLBRACK, s("["),
                   :tINTEGER, 3,
                   :tRBRACK, s("]"))
  end

  def test_yylex_open_square_bracket_ary
    util_lex_token("[1, 2, 3]",
                   :tLBRACK, s("["),
                   :tINTEGER, 1,
                   ",", s(","),
                   :tINTEGER, 2,
                   ",", s(","),
                   :tINTEGER, 3,
                   :tRBRACK, s("]"))
  end

  def test_yylex_open_square_bracket_meth
    util_lex_token("m[3]",
                   :tIDENTIFIER, s("m"),
                   "[", s("["),
                   :tINTEGER, 3,
                   :tRBRACK, s("]"))
  end

  def test_yylex_or
    util_lex_token "|", :tPIPE, s("|")
  end

  def test_yylex_or2
    util_lex_token "||", :tOROP, s("||")
  end

  def test_yylex_or2_equals
    util_lex_token "||=", :tOP_ASGN, s("||")
  end

  def test_yylex_or_equals
    util_lex_token "|=", :tOP_ASGN, s("|")
  end

  def test_yylex_percent
    util_lex_token("a % 2",
                   :tIDENTIFIER, s("a"),
                   :tPERCENT, s("%"),
                   :tINTEGER, 2)
  end

  def test_yylex_percent_equals
    util_lex_token("a %= 2",
                   :tIDENTIFIER, s("a"),
                   :tOP_ASGN, s("%"),
                   :tINTEGER, 2)
  end

  def test_yylex_plus
    util_lex_token("1 + 1", # TODO lex_state?
                   :tINTEGER, 1,
                   :tPLUS, s("+"),
                   :tINTEGER, 1)
  end

  def test_yylex_plus_equals
    util_lex_token "+=", :tOP_ASGN, s("+")
  end

  def test_yylex_plus_method
    @lex.lex_state = :expr_fname
    util_lex_token "+", :tPLUS, s("+")
  end

  def test_yylex_plus_unary_method
    @lex.lex_state = :expr_fname
    util_lex_token "+@", :tUPLUS, s("+@")
  end

  def test_yylex_plus_unary_number
    util_lex_token("+42",
                   :tINTEGER, 42)
  end

  def test_yylex_question
    util_lex_token "?*", :tINTEGER, 42
  end

  def test_yylex_question_bad_eos
    util_bad_token "?"
  end

  def test_yylex_question_ws
    util_lex_token "? ",  "?", s("?")
    util_lex_token "?\n", "?", s("?")
    util_lex_token "?\t", "?", s("?")
    util_lex_token "?\v", "?", s("?")
    util_lex_token "?\r", "?", s("?")
    util_lex_token "?\f", "?", s("?")
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
    util_lex_token "]", :tRBRACK, s("]")
  end

  def test_yylex_rcurly
    util_lex_token "}", :tRCURLY, s("}")
  end

  def test_yylex_regexp
    util_lex_token("/regexp/",
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regexp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_ambiguous
    util_lex_token("method /regexp/",
                   :tIDENTIFIER,     s("method"),
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regexp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_bad
    util_bad_token("/.*/xyz",
                   :tREGEXP_BEG, s("/"),
                   :tSTRING_CONTENT, s(:str, ".*"))
  end

  def test_yylex_regexp_escape_C
    util_lex_token('/regex\\C-x/',
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\C-x"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_C_M
    util_lex_token('/regex\\C-\\M-x/',
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\C-\\M-x"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_C_M_craaaazy
    util_lex_token("/regex\\C-\\\n\\M-x/",
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\C-\\M-x"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_C_bad_dash
    util_bad_token '/regex\\Cx/', :tREGEXP_BEG, s("/")
  end

  def test_yylex_regexp_escape_C_bad_dash_eos
    util_bad_token '/regex\\C-/', :tREGEXP_BEG, s("/")
  end

  def test_yylex_regexp_escape_C_bad_dash_eos2
    util_bad_token '/regex\\C-', :tREGEXP_BEG, s("/")
  end

  def test_yylex_regexp_escape_C_bad_eos
    util_bad_token '/regex\\C/', :tREGEXP_BEG, s("/")
  end

  def test_yylex_regexp_escape_C_bad_eos2
    util_bad_token '/regex\\c', :tREGEXP_BEG, s("/")
  end

  def test_yylex_regexp_escape_M
    util_lex_token('/regex\\M-x/',
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\M-x"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_M_C
    util_lex_token('/regex\\M-\\C-x/',
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\M-\\C-x"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_M_bad_dash
    util_bad_token '/regex\\Mx/', :tREGEXP_BEG, s("/")
  end

  def test_yylex_regexp_escape_M_bad_dash_eos
    util_bad_token '/regex\\M-/', :tREGEXP_BEG, s("/")
  end

  def test_yylex_regexp_escape_M_bad_dash_eos2
    util_bad_token '/regex\\M-', :tREGEXP_BEG, s("/")
  end

  def test_yylex_regexp_escape_M_bad_eos
    util_bad_token '/regex\\M/', :tREGEXP_BEG, s("/")
  end

  def test_yylex_regexp_escape_backslash_slash
    util_lex_token('/\\//',
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, '\\/'),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_backslash_terminator
    util_lex_token('%r%blah\\%blah%',
                   :tREGEXP_BEG,     s("%r\000"), # FIX ?!?
                   :tSTRING_CONTENT, s(:str, "blah\\%blah"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_backslash_terminator_meta1
    util_lex_token('%r{blah\\}blah}',
                   :tREGEXP_BEG,     s("%r{"), # FIX ?!?
                   :tSTRING_CONTENT, s(:str, "blah\\}blah"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_backslash_terminator_meta2
    util_lex_token('%r/blah\\/blah/',
                   :tREGEXP_BEG,     s("%r\000"), # FIX ?!?
                   :tSTRING_CONTENT, s(:str, "blah\\/blah"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_backslash_terminator_meta3
    util_lex_token('%r/blah\\%blah/',
                   :tREGEXP_BEG,     s("%r\000"), # FIX ?!?
                   :tSTRING_CONTENT, s(:str, "blah\\%blah"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_bad_eos
    util_bad_token '/regex\\', :tREGEXP_BEG, s("/")
  end

  def test_yylex_regexp_escape_bs
    util_lex_token('/regex\\\\regex/',
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\\\regex"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_c
    util_lex_token('/regex\\cxxx/',
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\cxxx"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_c_backslash
    util_lex_token('/regex\\c\\n/',
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\c\\n"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_chars
    util_lex_token('/re\\tge\\nxp/',
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "re\\tge\\nxp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_double_backslash
    regexp = '/[\\/\\\\]$/'
    util_lex_token(regexp,
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, regexp[1..-2]),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_hex
    util_lex_token('/regex\\x61xp/',
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\x61xp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_hex_one
    util_lex_token('/^[\\xd\\xa]{2}/on',
                   :tREGEXP_BEG,     s('/'),
                   :tSTRING_CONTENT, s(:str, '^[\\xd\\xa]{2}'),
                   :tREGEXP_END,     'on')
  end

  def test_yylex_regexp_escape_hex_bad
    util_bad_token '/regex\\xzxp/', :tREGEXP_BEG, s("/")
  end

  def test_yylex_regexp_escape_oct1
    util_lex_token('/regex\\0xp/',
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\0xp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_oct2
    util_lex_token('/regex\\07xp/',
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\07xp"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_oct3
    util_lex_token('/regex\\10142/',
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regex\\10142"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_escape_return
    util_lex_token("/regex\\\nregex/",
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, "regexregex"),
                   :tREGEXP_END,     "")
  end

  def test_yylex_regexp_nm
    util_lex_token("/.*/nm",
                   :tREGEXP_BEG,     s("/"),
                   :tSTRING_CONTENT, s(:str, ".*"),
                   :tREGEXP_END,     "nm")
  end

  def test_yylex_rparen
    util_lex_token ")", :tRPAREN, s(")")
  end

  def test_yylex_rshft
    util_lex_token("a >> 2",
                   :tIDENTIFIER, s("a"),
                   :tRSHFT, s(">>"),
                   :tINTEGER, 2)
  end

  def test_yylex_rshft_equals
    util_lex_token("a >>= 2",
                   :tIDENTIFIER, s("a"),
                   :tOP_ASGN, s(">>"),
                   :tINTEGER, 2)
  end

  def test_yylex_star
    util_lex_token("a * ",
                   :tIDENTIFIER, s("a"),
                   :tSTAR2, s("*"))

    assert_equal :expr_beg, @lex.lex_state
  end

  def test_yylex_star2
    util_lex_token("a ** ",
                   :tIDENTIFIER, s("a"),
                   :tPOW, s("**"))

    assert_equal :expr_beg, @lex.lex_state
  end

  def test_yylex_star2_equals
    util_lex_token("a **= ",
                   :tIDENTIFIER, s("a"),
                   :tOP_ASGN, s("**"))

    assert_equal :expr_beg, @lex.lex_state
  end

  def test_yylex_star_arg
    @lex.lex_state = :expr_arg

    util_lex_token(" *a",
                   :tSTAR, s("*"),
                   :tIDENTIFIER, s("a"))

    assert_equal :expr_arg, @lex.lex_state
  end

  def test_yylex_star_arg_beg
    @lex.lex_state = :expr_beg

    util_lex_token("*a",
                   :tSTAR, s("*"),
                   :tIDENTIFIER, s("a"))

    assert_equal :expr_arg, @lex.lex_state
  end

  def test_yylex_star_arg_beg_fname
    @lex.lex_state = :expr_fname

    util_lex_token("*a",
                   :tSTAR2, s("*"),
                   :tIDENTIFIER, s("a"))

    assert_equal :expr_arg, @lex.lex_state
  end

  def test_yylex_star_equals
    util_lex_token("a *= ",
                   :tIDENTIFIER, s("a"),
                   :tOP_ASGN, s("*"))

    assert_equal :expr_beg, @lex.lex_state
  end

  def test_yylex_string_bad_eos
    util_bad_token('%',
                   :tSTRING_BEG,     s('%'))
  end

  def test_yylex_string_bad_eos_quote
    util_bad_token('%{nest',
                   :tSTRING_BEG,     s('%}'))
  end

  def test_yylex_string_double
    util_lex_token('"string"',
                   :tSTRING_BEG,     s('"'),
                   :tSTRING_CONTENT, s(:str, "string"),
                   :tSTRING_END,     s('"'))
  end

  def test_yylex_string_double_escape_M
    util_lex_token('"\\M-g"',
                   :tSTRING_BEG,     s('"'),
                   :tSTRING_CONTENT, s(:str, "\347"),
                   :tSTRING_END,     s('"'))
  end

  def test_yylex_string_escape_x_single
    util_lex_token('"\\x0"',
                   :tSTRING_BEG,     s('"'),
                   :tSTRING_CONTENT, s(:str, "\000"),
                   :tSTRING_END,     s('"'))
  end

  def test_yylex_string_double_escape_chars
    util_lex_token('"s\\tri\\ng"',
                   :tSTRING_BEG,     s('"'),
                   :tSTRING_CONTENT, s(:str, "s\tri\ng"),
                   :tSTRING_END,     s('"'))
  end

  def test_yylex_string_double_escape_hex
    util_lex_token('"n = \\x61\\x62\\x63"',
                   :tSTRING_BEG,     s('"'),
                   :tSTRING_CONTENT, s(:str, "n = abc"),
                   :tSTRING_END,     s('"'))
  end

  def test_yylex_string_double_escape_bs1
    util_lex_token('"a\\a\\a"',
                   :tSTRING_BEG,     s('"'),
                   :tSTRING_CONTENT, s(:str, "a\a\a"),
                   :tSTRING_END,     s('"'))
  end

  def test_yylex_string_double_escape_bs2
    util_lex_token('"a\\\\a"',
                   :tSTRING_BEG,     s('"'),
                   :tSTRING_CONTENT, s(:str, "a\\a"),
                   :tSTRING_END,     s('"'))
  end

  def test_yylex_string_double_escape_octal
    util_lex_token('"n = \\101\\102\\103"',
                   :tSTRING_BEG,     s('"'),
                   :tSTRING_CONTENT, s(:str, "n = ABC"),
                   :tSTRING_END,     s('"'))
  end

  def test_yylex_string_double_interp
    util_lex_token("\"blah #x a \#@a b \#$b c \#{3} # \"",
                   :tSTRING_BEG,     s("\""),
                   :tSTRING_CONTENT, s(:str, "blah #x a "),
                   :tSTRING_DVAR,    nil,
                   :tSTRING_CONTENT, s(:str, "@a b "),
                   :tSTRING_DVAR,    nil,
                   :tSTRING_CONTENT, s(:str, "$b c "),
                   :tSTRING_DBEG,    nil,
                   :tSTRING_CONTENT, s(:str, "3} # "),
                   :tSTRING_END,     s("\""))
  end

  def test_yylex_string_double_nested_curlies
    util_lex_token('%{nest{one{two}one}nest}',
                   :tSTRING_BEG,     s('%}'),
                   :tSTRING_CONTENT, s(:str, "nest{one{two}one}nest"),
                   :tSTRING_END,     s('}'))
  end

  def test_yylex_string_double_no_interp
    util_lex_token("\"# blah\"",                                # pound first
                   :tSTRING_BEG,     s("\""),
                   :tSTRING_CONTENT, s(:str, "# blah"),
                   :tSTRING_END,     s("\""))

    util_lex_token("\"blah # blah\"",                           # pound not first
                   :tSTRING_BEG,     s("\""),
                   :tSTRING_CONTENT, s(:str, "blah # blah"),
                   :tSTRING_END,     s("\""))
  end

  def test_yylex_string_pct_Q
    util_lex_token("%Q[s1 s2]",
                   :tSTRING_BEG,     s("%Q["),
                   :tSTRING_CONTENT, s(:str, "s1 s2"),
                   :tSTRING_END,     s("]"))
  end

  def test_yylex_string_pct_W
    util_lex_token("%W[s1 s2\ns3]", # TODO: add interpolation to these
                   :tWORDS_BEG,      s("%W["),
                   :tSTRING_CONTENT, s(:str, "s1"),
                   " ",              nil,
                   :tSTRING_CONTENT, s(:str, "s2"),
                   " ",              nil,
                   :tSTRING_CONTENT, s(:str, "s3"),
                   " ",              nil,
                   :tSTRING_END,     nil)
  end

  def test_yylex_string_pct_W_bs_nl
    util_lex_token("%W[s1 \\\ns2]", # TODO: add interpolation to these
                   :tWORDS_BEG,      s("%W["),
                   :tSTRING_CONTENT, s(:str, "s1"),
                   " ",              nil,
                   :tSTRING_CONTENT, s(:str, "\ns2"),
                   " ",              nil,
                   :tSTRING_END,     nil)
  end

  def test_yylex_string_pct_angle
    util_lex_token("%<blah>",
                   :tSTRING_BEG, s("%>"),
                   :tSTRING_CONTENT, s(:str, "blah"),
                   :tSTRING_END, s(">"))
  end

  def test_yylex_string_pct_other
    util_lex_token("%%blah%",
                   :tSTRING_BEG, s("%%"),
                   :tSTRING_CONTENT, s(:str, "blah"),
                   :tSTRING_END, s("%"))
  end

  def test_yylex_string_pct_w
    util_bad_token("%w[s1 s2 ",
                   :tAWORDS_BEG,     s("%w["),
                   :tSTRING_CONTENT, s(:str, "s1"),
                   " ",              nil,
                   :tSTRING_CONTENT, s(:str, "s2"),
                   " ",              nil)
  end

  def test_yylex_string_pct_w_bs_nl
    util_lex_token("%w[s1 \\\ns2]",
                   :tAWORDS_BEG,     s("%w["),
                   :tSTRING_CONTENT, s(:str, "s1"),
                   " ",              nil,
                   :tSTRING_CONTENT, s(:str, "\ns2"),
                   " ",              nil,
                   :tSTRING_END,     nil)
  end

  def test_yylex_string_pct_w_bs_sp
    util_lex_token("%w[s\\ 1 s\\ 2]",
                   :tAWORDS_BEG,     s("%w["),
                   :tSTRING_CONTENT, s(:str, "s 1"),
                   " ",              nil,
                   :tSTRING_CONTENT, s(:str, "s 2"),
                   " ",              nil,
                   :tSTRING_END,     nil)
  end

  def test_yylex_string_single
    util_lex_token("'string'",
                   :tSTRING_BEG,     s("'"),
                   :tSTRING_CONTENT, s(:str, "string"),
                   :tSTRING_END,     s("'"))
  end

  def test_yylex_string_single_escape_chars
    util_lex_token("'s\\tri\\ng'",
                   :tSTRING_BEG,     s("'"),
                   :tSTRING_CONTENT, s(:str, "s\\tri\\ng"),
                   :tSTRING_END,     s("'"))
  end

  def test_yylex_string_single_nl
    util_lex_token("'blah\\\nblah'",
                   :tSTRING_BEG,     s("'"),
                   :tSTRING_CONTENT, s(:str, "blah\\\nblah"),
                   :tSTRING_END,     s("'"))
  end

  def test_yylex_symbol
    util_lex_token(":symbol",
                   :tSYMBEG, s(":"),
                   :tIDENTIFIER, s("symbol"))
  end

  def test_yylex_symbol_bad_zero
    util_bad_token(":\"blah\0\"",
                   :tSYMBEG, s(":"))
  end

  def test_yylex_symbol_double
    util_lex_token(":\"symbol\"",
                   :tSYMBEG, s(":"),
                   :tSTRING_CONTENT, s(:str, "symbol"),
                   :tSTRING_END, s('"'))
  end

  def test_yylex_symbol_single
    util_lex_token(":'symbol'",
                   :tSYMBEG, s(":"),
                   :tSTRING_CONTENT, s(:str, "symbol"),
                   :tSTRING_END, s("'"))
  end

  def test_yylex_ternary
    util_lex_token("a ? b : c",
                   :tIDENTIFIER, s("a"),
                   "?",          s("?"), # FIX
                   :tIDENTIFIER, s("b"),
                   ":",          s(":"), # FIX
                   :tIDENTIFIER, s("c"))

    util_lex_token("a ?bb : c", # GAH! MATZ!!!
                   :tIDENTIFIER, s("a"),
                   "?",          s("?"), # FIX
                   :tIDENTIFIER, s("bb"),
                   ":",          s(":"), # FIX
                   :tIDENTIFIER, s("c"))

    util_lex_token("42 ?", # 42 forces expr_end
                   :tINTEGER, 42,
                   "?",          s("?"))
  end

  def test_yylex_tilde
    util_lex_token "~", :tTILDE, s("~")
  end

  def test_yylex_tilde_unary
    @lex.lex_state = :expr_fname
    util_lex_token "~@", :tTILDE, s("~")
  end

  def test_yylex_uminus
    util_lex_token("-blah",
                   :tUMINUS, s("-"),
                   :tIDENTIFIER, s("blah"))
  end

  def test_yylex_underscore
    util_lex_token("_var", :tIDENTIFIER, s("_var"))
  end

  def test_yylex_underscore_end
    @lex.src = "__END__\n"
    deny @lex.advance
  end

  def test_yylex_uplus
    util_lex_token("+blah",
                   :tUPLUS, s("+"),
                   :tIDENTIFIER, s("blah"))
  end

  def test_zbug_float_in_decl
    util_lex_token("def initialize(u = ",
                   :kDEF, s("def"),
                   :tIDENTIFIER, s("initialize"),
                   :tLPAREN2, s("("),
                   :tIDENTIFIER, s("u"),
                   '=', s("="))

    assert_equal :expr_beg, @lex.lex_state

    util_lex_token("0.0, s = 0.0",
                   :tFLOAT, 0.0,
                   ',', s(','),
                   :tIDENTIFIER, s("s"),
                   '=', s("="),
                   :tFLOAT, 0.0)
  end

  def test_zbug_id_equals
    util_lex_token("a =",
                   :tIDENTIFIER, s("a"),
                   '=', s("="))

    assert_equal :expr_beg, @lex.lex_state

    util_lex_token("0.0",
                   :tFLOAT, 0.0)
  end

  def test_zbug_no_spaces_in_decl
    util_lex_token("def initialize(u=",
                   :kDEF, s("def"),
                   :tIDENTIFIER, s("initialize"),
                   :tLPAREN2, s("("),
                   :tIDENTIFIER, s("u"),
                   '=', s("="))

    assert_equal :expr_beg, @lex.lex_state

    util_lex_token("0.0,s=0.0",
                   :tFLOAT, 0.0,
                   ',', s(','),
                   :tIDENTIFIER, s("s"),
                   '=', s("="),
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

  def util_lex_fname name, type, end_state = :expr_arg
    @lex.lex_state = :expr_fname # can only set via parser's defs

    util_lex_token("def #{name} ", :kDEF, s("def"), type, s(name))

    assert_equal end_state, @lex.lex_state
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

