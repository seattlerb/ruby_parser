# frozen_string_literal: true
# encoding: UTF-8

$DEBUG = true if ENV["DEBUG"]

class RubyLexer
  # :stopdoc:
  EOF = :eof_haha!

  ESCAPES = {
    "a"    => "\007",
    "b"    => "\010",
    "e"    => "\033",
    "f"    => "\f",
    "n"    => "\n",
    "r"    => "\r",
    "s"    => " ",
    "t"    => "\t",
    "v"    => "\13",
    "\\"   => '\\',
    "\n"   => "",
    "C-\?" => 127.chr,
    "c\?"  => 127.chr,
  }

  HAS_ENC = "".respond_to? :encoding

  BTOKENS = {
    ".."  => :tBDOT2,
    "..." => :tBDOT3,
  }

  TOKENS = {
    "!"   => :tBANG,
    "!="  => :tNEQ,
    "!@"  => :tBANG,
    "!~"  => :tNMATCH,
    ","   => :tCOMMA,
    ".."  => :tDOT2,
    "..." => :tDOT3,
    "="   => :tEQL,
    "=="  => :tEQ,
    "===" => :tEQQ,
    "=>"  => :tASSOC,
    "=~"  => :tMATCH,
    "->"  => :tLAMBDA,
  }

  PERCENT_END = {
    "(" => ")",
    "[" => "]",
    "{" => "}",
    "<" => ">",
  }

  SIMPLE_RE_META = /[\$\*\+\.\?\^\|\)\]\}\>]/

  @@regexp_cache = Hash.new { |h, k| h[k] = Regexp.new(Regexp.escape(k)) }
  @@regexp_cache[nil] = nil

  def regexp_cache
    @@regexp_cache
  end

  if $DEBUG then
    attr_reader :lex_state

    def lex_state= o
      return if @lex_state == o

      from = ""
      if ENV["VERBOSE"]
        path = caller[0]
        path = caller[1] if path =~ /result/
        path, line, *_ = path.split(/:/)
        path.delete_prefix! File.dirname File.dirname __FILE__
        from = " at .%s:%s" % [path, line]
      end

      warn "lex_state: %p -> %p%s" % [lex_state, o, from]

      @lex_state = o
    end
  end

  # :startdoc:

  attr_accessor :lex_state unless $DEBUG

  attr_accessor :brace_nest
  attr_accessor :cmdarg
  attr_accessor :command_start
  attr_accessor :cmd_state # temporary--ivar to avoid passing everywhere
  attr_accessor :last_state
  attr_accessor :cond

  ##
  # Additional context surrounding tokens that both the lexer and
  # grammar use.

  attr_accessor :lex_strterm
  attr_accessor :lpar_beg
  attr_accessor :paren_nest
  attr_accessor :parser # HACK for very end of lexer... *sigh*
  attr_accessor :space_seen
  attr_accessor :string_buffer
  attr_accessor :string_nest

  # Last token read via next_token.
  attr_accessor :token

  attr_writer :comments

  def initialize _ = nil
    @lex_state = nil # remove one warning under $DEBUG
    self.lex_state = EXPR_NONE

    self.cond   = RubyParserStuff::StackState.new(:cond, $DEBUG)
    self.cmdarg = RubyParserStuff::StackState.new(:cmdarg, $DEBUG)

    reset
  end

  def arg_ambiguous
    self.warning "Ambiguous first argument. make sure."
  end

  def arg_state
    is_after_operator? ? EXPR_ARG : EXPR_BEG
  end

  def ignore_body_comments
    @comments.clear
  end

  def comments # TODO: remove this... maybe comment_string + attr_accessor
    c = @comments.join
    @comments.clear
    c
  end

  def debug n
    raise "debug #{n}"
  end

  def eat_whitespace
    r = scan(/\s+/)
    self.lineno += r.count("\n") if r

    r += eat_whitespace if eos? && ss_stack.size > 1

    r
  end

  def expr_dot?
    lex_state =~ EXPR_DOT
  end

  def expr_fname? # REFACTOR
    lex_state =~ EXPR_FNAME
  end

  def expr_result token, text
    cond.push false
    cmdarg.push false
    result EXPR_BEG, token, text
  end

  def heredoc here                              # ../compare/parse30.y:7678
    _, term, func, _indent_max, _lineno, range = here

    start_line = lineno
    eos = term # HACK
    indent = func =~ STR_FUNC_INDENT

    self.string_buffer = []

    last_line = self.ss_string[range] if range
    eol = last_line && last_line.end_with?("\r\n") ? "\r\n" : "\n" # HACK

    expand = func =~ STR_FUNC_EXPAND

    # TODO? p->heredoc_line_indent == -1

    indent_re = indent ? "[ \t]*" : nil
    eos_re    = /#{indent_re}#{Regexp.escape eos}(?=\r?\n|\z)/
    err_msg   = "can't match #{eos_re.inspect} anywhere in "

    maybe_pop_stack
    rb_compile_error err_msg if end_of_stream?

    if beginning_of_line? && scan(eos_re) then
      scan(/\r?\n|\z/)
      self.lineno += 1 if matched =~ /\n/

      heredoc_restore

      self.lex_strterm = nil
      self.lex_state = EXPR_END

      return :tSTRING_END, [term, func, range]
    end

    if expand then
      case
      when scan(/#(?=\$(-.|[a-zA-Z_0-9~\*\$\?!@\/\\;,\.=:<>\"\&\`\'+]))/) then
        # TODO: !ISASCII
        # ?! see parser_peek_variable_name
        return :tSTRING_DVAR, matched
      when scan(/#(?=\@\@?[a-zA-Z_])/) then
        # TODO: !ISASCII
        return :tSTRING_DVAR, matched
      when scan(/#[{]/) then
        self.command_start = true
        return :tSTRING_DBEG, matched
      when scan(/#/) then
        string_buffer << "#"
      end

      begin
        # NOTE: this visibly diverges from the C code but uses tokadd_string
        #       to stay clean.

        str = tokadd_string func, eol, nil
        rb_compile_error err_msg if str == RubyLexer::EOF

        if str != eol then
          str = string_buffer.join
          string_buffer.clear
          return result nil, :tSTRING_CONTENT, str, start_line
        else
          string_buffer << scan(/\r?\n/)
          self.lineno += 1 # TODO: try to remove most scan(/\n/) and friends
        end
      end until check eos_re
    else
      until check(eos_re) do
        string_buffer << scan(/.*(\r?\n|\z)/)
        self.lineno += 1
        rb_compile_error err_msg if end_of_stream?
      end
    end

    string_content = begin
                       s = string_buffer.join
                       s.b.force_encoding Encoding::UTF_8
                       s
                     end
    string_buffer.clear

    result nil, :tSTRING_CONTENT, string_content, start_line
  end

  def heredoc_identifier                        # ../compare/parse30.y:7354
    token  = :tSTRING_BEG
    func   = STR_FUNC_BORING
    term   = nil
    indent = nil
    quote  = nil
    char_pos = nil
    byte_pos = nil

    heredoc_indent_mods = "-"
    heredoc_indent_mods += '\~' if ruby23plus?

    case
    when scan(/([#{heredoc_indent_mods}]?)([\'\"\`])(.*?)\2/) then
      mods, quote, term = self.captures
      char_pos = ss.charpos
      byte_pos = ss.pos

      func |= STR_FUNC_INDENT unless mods.empty?
      func |= STR_FUNC_DEDENT if mods == "~"
      func |= case quote
              when "\'" then
                STR_SQUOTE
              when '"' then
                STR_DQUOTE
              when "`" then
                token = :tXSTRING_BEG
                STR_XQUOTE
              else
                debug 1
              end
    when scan(/[#{heredoc_indent_mods}]?([\'\"\`])(?!\1*\Z)/) then
      rb_compile_error "unterminated here document identifier"
    when scan(/([#{heredoc_indent_mods}]?)(#{IDENT_CHAR}+)/) then
      mods, term = self.captures
      quote = '"'
      char_pos = ss.charpos
      byte_pos = ss.pos

      func |= STR_FUNC_INDENT unless mods.empty?
      func |= STR_FUNC_DEDENT if mods == "~"
      func |= STR_DQUOTE
    else
      return
    end

    old_lineno = self.lineno
    rest_of_line = scan(/.*(?:\r?\n|\z)/)
    self.lineno += rest_of_line.count "\n"

    char_pos_end = ss.charpos - 1

    range = nil
    range = char_pos..char_pos_end unless rest_of_line.empty?

    self.lex_strterm = [:heredoc, term, func, indent, old_lineno, range, byte_pos]

    result nil, token, quote, old_lineno
  end

  def heredoc_restore                           # ../compare/parse30.y:7438
    _, _term, _func, _indent, lineno, range, bytepos = lex_strterm

    new_ss = ss.class.new self.ss_string[0..range.max]
    new_ss.pos = bytepos

    lineno_push self.lineno
    ss_push new_ss
    self.lineno = lineno

    nil
  end

  def in_fname? # REFACTOR
    lex_state =~ EXPR_FNAME
  end

  def int_with_base base
    rb_compile_error "Invalid numeric format" if matched =~ /__/

    text = matched
    case
    when text.end_with?("ri")
      result EXPR_NUM, :tIMAGINARY, Complex(0, Rational(text.chop.chop.to_i(base)))
    when text.end_with?("r")
      result EXPR_NUM, :tRATIONAL, Rational(text.chop.to_i(base))
    when text.end_with?("i")
      result EXPR_NUM, :tIMAGINARY, Complex(0, text.chop.to_i(base))
    else
      result EXPR_NUM, :tINTEGER, text.to_i(base)
    end
  end

  def is_after_operator?
    lex_state =~ EXPR_FNAME|EXPR_DOT
  end

  def is_arg?
    lex_state =~ EXPR_ARG_ANY
  end

  def is_beg?
    lex_state =~ EXPR_BEG_ANY || lex_state == EXPR_LAB # yes, == EXPR_LAB
  end

  def is_end?
    lex_state =~ EXPR_END_ANY
  end

  def is_label_possible?
    (lex_state =~ EXPR_LABEL|EXPR_ENDFN && !cmd_state) || is_arg?
  end

  def is_label_suffix?
    check(/:(?!:)/)
  end

  def is_space_arg? c = "x"
    is_arg? and space_seen and c !~ /\s/
  end

  def lambda_beginning?
    lpar_beg && lpar_beg == paren_nest
  end

  def is_local_id id
    # maybe just make this false for now
    self.parser.env[id.to_sym] == :lvar # HACK: this isn't remotely right
  end

  def lvar_defined? id
    # TODO: (dyna_in_block? && dvar_defined?(id)) || local_id?(id)
    self.parser.env[id.to_sym] == :lvar
  end

  def newtok
    string_buffer.clear
  end

  def not_end?
    not is_end?
  end

  # called from process_percent
  def process_percent_quote                      # ../compare/parse30.y:8645
    c = getch # type %<type><term>...<term>

    long_hand = !!(c =~ /[QqWwIixrs]/)

    if end_of_stream? || c !~ /\p{Alnum}/ then
      term = c # TODO? PERCENT_END[c] || c

      debug 2 if c && c !~ /\p{ASCII}/
      c = "Q"
    else
      term = getch

      debug 3 if term =~ /\p{Alnum}|\P{ASCII}/
    end

    if end_of_stream? or c == RubyLexer::EOF or term == RubyLexer::EOF then
      rb_compile_error "unterminated quoted string meets end of file"
    end

    # "\0" is special to indicate beg=nnd and that no nesting?
    paren = term
    term = PERCENT_END[term]
    term, paren = paren, "\0" if term.nil? # TODO: "\0" -> nil

    text = long_hand ? "%#{c}#{paren}" : "%#{term}"

    current_line = self.lineno

    token_type, string_type =
      case c
      when "Q" then
        [:tSTRING_BEG,   STR_DQUOTE]
      when "q" then
        [:tSTRING_BEG,   STR_SQUOTE]
      when "W" then
        eat_whitespace
        [:tWORDS_BEG,    STR_DQUOTE | STR_FUNC_QWORDS]
      when "w" then
        eat_whitespace
        [:tQWORDS_BEG,   STR_SQUOTE | STR_FUNC_QWORDS]
      when "I" then
        eat_whitespace
        [:tSYMBOLS_BEG,  STR_DQUOTE | STR_FUNC_QWORDS]
      when "i" then
        eat_whitespace
        [:tQSYMBOLS_BEG, STR_SQUOTE | STR_FUNC_QWORDS]
      when "x" then
        [:tXSTRING_BEG,  STR_XQUOTE]
      when "r" then
        [:tREGEXP_BEG,   STR_REGEXP]
      when "s" then
        self.lex_state = EXPR_FNAME
        [:tSYMBEG,       STR_SSYM]
      else
        rb_compile_error "unknown type of %string. Expected [QqWwIixrs], found '#{c}'."
      end

    string string_type, term, paren

    result nil, token_type, text, current_line
  end

  def scan_variable_name                        # ../compare/parse30.y:7208
    case
    when scan(/#(?=\$(-.|[a-zA-Z_0-9~\*\$\?!@\/\\;,\.=:<>\"\&\`\'+]))/) then
      # TODO: !ISASCII
      return :tSTRING_DVAR, matched
    when scan(/#(?=\@\@?[a-zA-Z_])/) then
      # TODO: !ISASCII
      return :tSTRING_DVAR, matched
    when scan(/#[{]/) then
      self.command_start = true
      return :tSTRING_DBEG, matched
    when scan(/#/) then
      # do nothing but swallow
    end

    # if scan(/\P{ASCII}|_|\p{Alpha}/) then # TODO: fold into above DVAR cases
    #   # if (!ISASCII(c) || c == '_' || ISALPHA(c))
    #   #     return tSTRING_DVAR;
    # end

    nil
  end

  def parse_string quote                         # ../compare/parse30.y:7273
    _, func, term, paren = quote

    qwords = func =~ STR_FUNC_QWORDS
    regexp = func =~ STR_FUNC_REGEXP
    expand = func =~ STR_FUNC_EXPAND
    list   = func =~ STR_FUNC_LIST
    termx  = func =~ STR_FUNC_TERM # TODO: document wtf this means

    space = false
    term_re = regexp_cache[term]

    if termx then
      # self.nextc if qwords # delayed term

      self.lex_strterm = nil

      return result EXPR_END, regexp ? :tREGEXP_END : :tSTRING_END, term
    end

    space = true if qwords and eat_whitespace

    if list then
      debug 4
      # quote[1] -= STR_FUNC_LIST
      # space = true
    end

    # TODO: move to quote.nest!
    if string_nest == 0 && scan(term_re) then
      if qwords then
        quote[1] |= STR_FUNC_TERM

        return :tSPACE, matched
      end

      return string_term func
    end

    return result nil, :tSPACE, " " if space

    newtok

    if expand && check(/#/) then
      t = self.scan_variable_name
      return t if t

      tokadd "#"
    end

    # TODO: add string_nest, enc, base_enc ?
    lineno = self.lineno
    if tokadd_string(func, term, paren) == RubyLexer::EOF then
      if qwords then
        rb_compile_error "unterminated list meets end of file"
      end

      if regexp then
        rb_compile_error "unterminated regexp meets end of file"
      else
        rb_compile_error "unterminated string meets end of file"
      end
    end

    result nil, :tSTRING_CONTENT, string_buffer.join, lineno
  end

  def string_term func                          # ../compare/parse30.y:7254
    self.lex_strterm = nil

    return result EXPR_END, :tREGEXP_END, self.regx_options if
      func =~ STR_FUNC_REGEXP

    if func =~ STR_FUNC_LABEL && is_label_suffix? then
      self.getch
      self.lex_state = EXPR_BEG|EXPR_LABEL

      return :tLABEL_END, string_buffer.join
    end

    self.lex_state = EXPR_END

    return :tSTRING_END, [self.matched, func]
  end

  def possibly_escape_string text, check
    content = match[1]

    if text =~ check then
      content.gsub(ESC) { unescape $1 }
    else
      content.gsub(/\\\\/, "\\").gsub(/\\\'/, "'")
    end
  end

  def process_amper text
    token = if is_arg? && space_seen && !check(/\s/) then
               warning("`&' interpreted as argument prefix")
               :tAMPER
             elsif lex_state =~ EXPR_BEG|EXPR_MID then
               :tAMPER
             else
               :tAMPER2
             end

    result :arg_state, token, "&"
  end

  def process_backref text
    token = match[1].to_sym
    # TODO: can't do lineno hack w/ symbol
    result EXPR_END, :tBACK_REF, token
  end

  def process_begin text
    @comments << matched

    unless scan(/.*?\n=end( |\t|\f)*[^\n]*(\n|\z)/m) then
      @comments.clear
      rb_compile_error("embedded document meets end of file")
    end

    @comments << matched
    self.lineno += matched.count("\n") # HACK?

    nil # TODO
  end

  def process_brace_close text
    case matched
    when "}" then
      self.brace_nest -= 1
      return :tSTRING_DEND, matched if brace_nest < 0
    end

    # matching compare/parse26.y:8099
    cond.pop
    cmdarg.pop

    case matched
    when "}" then
      self.lex_state   = ruby24minus? ? EXPR_ENDARG : EXPR_END
      return :tRCURLY, matched
    when "]" then
      self.paren_nest -= 1
      self.lex_state   = ruby24minus? ? EXPR_ENDARG : EXPR_END
      return :tRBRACK, matched
    when ")" then
      self.paren_nest -= 1
      self.lex_state   = EXPR_ENDFN
      return :tRPAREN, matched
    else
      raise "Unknown bracing: #{matched.inspect}"
    end
  end

  def process_brace_open text
    # matching compare/parse23.y:8694
    self.brace_nest += 1

    if lambda_beginning? then
      self.lpar_beg = nil
      self.paren_nest -= 1 # close arg list when lambda opens body

      return expr_result(:tLAMBEG, "{")
    end

    token = case
            when lex_state =~ EXPR_LABELED then
              :tLBRACE     # hash
            when lex_state =~ EXPR_ARG_ANY|EXPR_END|EXPR_ENDFN then
              :tLCURLY     # block (primary) "{" in parse.y
            when lex_state =~ EXPR_ENDARG then
              :tLBRACE_ARG # block (expr)
            else
              :tLBRACE     # hash
            end

    state = token == :tLBRACE_ARG ? EXPR_BEG : EXPR_PAR
    self.command_start = true if token != :tLBRACE

    cond.push false
    cmdarg.push false
    result state, token, text
  end

  def process_colon1 text
    # ?: / then / when
    if is_end? || check(/\s/) then
      return result EXPR_BEG, :tCOLON, text
    end

    case
    when scan(/\'/) then
      string STR_SSYM, matched
    when scan(/\"/) then
      string STR_DSYM, matched
    end

    result EXPR_FNAME, :tSYMBEG, text
  end

  def process_colon2 text
    if is_beg? || lex_state =~ EXPR_CLASS || is_space_arg? then
      result EXPR_BEG, :tCOLON3, text
    else
      result EXPR_DOT, :tCOLON2, text
    end
  end

  def process_dots text
    tokens = ruby27plus? && is_beg? ? BTOKENS : TOKENS

    result EXPR_BEG, tokens[text], text
  end

  def process_float text
    rb_compile_error "Invalid numeric format" if text =~ /__/

    case
    when text.end_with?("ri")
      result EXPR_NUM, :tIMAGINARY, Complex(0, Rational(text.chop.chop))
    when text.end_with?("i")
      result EXPR_NUM, :tIMAGINARY, Complex(0, text.chop.to_f)
    when text.end_with?("r")
      result EXPR_NUM, :tRATIONAL,  Rational(text.chop)
    else
      result EXPR_NUM, :tFLOAT, text.to_f
    end
  end

  def process_gvar text
    if parser.class.version > 20 && text == "$-" then
      rb_compile_error "unexpected $undefined"
    end

    result EXPR_END, :tGVAR, text
  end

  def process_gvar_oddity text
    rb_compile_error "#{text.inspect} is not allowed as a global variable name"
  end

  def process_ivar text
    tok_id = text =~ /^@@/ ? :tCVAR : :tIVAR
    result EXPR_END, tok_id, text
  end

  def process_label text
    symbol = possibly_escape_string text, /^\"/

    result EXPR_LAB, :tLABEL, symbol
  end

  def process_label_or_string text
    if @was_label && text =~ /:\Z/ then
      @was_label = nil
      return process_label text
    elsif text =~ /:\Z/ then
      self.pos -= 1 # put back ":"
      text = text[0..-2]
    end

    result EXPR_END, :tSTRING, text[1..-2].gsub(/\\\\/, "\\").gsub(/\\\'/, "\'")
  end

  def process_lchevron text
    if (lex_state !~ EXPR_DOT|EXPR_CLASS &&
        !is_end? &&
        (!is_arg? || lex_state =~ EXPR_LABELED || space_seen)) then
      tok = self.heredoc_identifier
      return tok if tok
    end

    if is_after_operator? then
      self.lex_state = EXPR_ARG
    else
      self.command_start = true if lex_state =~ EXPR_CLASS
      self.lex_state = EXPR_BEG
    end

    result lex_state, :tLSHFT, "\<\<"
  end

  def process_newline_or_comment text    # ../compare/parse30.y:9126 ish
    c = matched

    if c == "#" then
      self.pos -= 1

      # TODO: handle magic comments
      while scan(/\s*\#.*(\n+|\z)/) do
        self.lineno += matched.count("\n") # TODO: maybe lines.size ?
        @comments << matched.gsub(/^ +#/, "#").gsub(/^ +$/, "")
      end

      return nil if end_of_stream?
    end

    c = (lex_state =~ EXPR_BEG|EXPR_CLASS|EXPR_FNAME|EXPR_DOT &&
         lex_state !~ EXPR_LABELED)
    # TODO: figure out what token_seen is for
    if c || self.lex_state == EXPR_LAB then # yes, == EXPR_LAB
      # ignore if !fallthrough?
      if !c && parser.in_kwarg then
        # normal newline
        self.command_start = true
        return result EXPR_BEG, :tNL, nil
      else
        maybe_pop_stack
        return # goto retry
      end
    end

    if scan(/[\ \t\r\f\v]+/) then
      self.space_seen = true
    end

    if check(/#/) then
      return # goto retry
    elsif check(/&\.|\.(?!\.)/) then # C version is a hellish obfuscated xnor
      return # goto retry
    end

    self.command_start = true

    result EXPR_BEG, :tNL, nil
  end

  def process_nthref text
    # TODO: can't do lineno hack w/ number
    result EXPR_END, :tNTH_REF, match[1].to_i
  end

  def process_paren text
    token = if is_beg? then
              :tLPAREN
            elsif !space_seen then
              # foo( ... ) => method call, no ambiguity
              :tLPAREN2
            elsif is_space_arg? then
              :tLPAREN_ARG
            elsif lex_state =~ EXPR_ENDFN && !lambda_beginning? then
              # TODO:
              # warn("parentheses after method name is interpreted as " \
              #      "an argument list, not a decomposed argument")
              :tLPAREN2
            else
              :tLPAREN2 # plain "(" in parse.y
            end

    self.paren_nest += 1

    cond.push false
    cmdarg.push false
    result EXPR_PAR, token, text
  end

  def process_percent text
    case
    when is_beg? then
      process_percent_quote
    when scan(/\=/)
      result EXPR_BEG, :tOP_ASGN, "%"
    when is_space_arg?(check(/\s/)) || (lex_state =~ EXPR_FITEM && check(/s/))
      process_percent_quote
    else
      result :arg_state, :tPERCENT, "%"
    end
  end

  def process_plus_minus text
    sign = matched
    utype, type = if sign == "+" then
                    [:tUPLUS, :tPLUS]
                  else
                    [:tUMINUS, :tMINUS]
                  end

    if is_after_operator? then
      if scan(/@/) then
        return result(EXPR_ARG, utype, "#{sign}@")
      else
        return result(EXPR_ARG, type, sign)
      end
    end

    return result(EXPR_BEG, :tOP_ASGN, sign) if scan(/\=/)

    if is_beg? || (is_arg? && space_seen && !check(/\s/)) then
      arg_ambiguous if is_arg?

      if check(/\d/) then
        return nil if utype == :tUPLUS
        return result EXPR_BEG, :tUMINUS_NUM, sign
      end

      return result EXPR_BEG, utype, sign
    end

    result EXPR_BEG, type, sign
  end

  def process_questionmark text
    if is_end? then
      return result EXPR_BEG, :tEH, "?"
    end

    if end_of_stream? then
      rb_compile_error "incomplete character syntax: parsed #{text.inspect}"
    end

    if check(/\s|\v/) then
      unless is_arg? then
        c2 = { " " => "s",
              "\n" => "n",
              "\t" => "t",
              "\v" => "v",
              "\r" => "r",
              "\f" => "f" }[matched]

        if c2 then
          warning("invalid character syntax; use ?\\" + c2)
        end
      end

      # ternary
      return result EXPR_BEG, :tEH, "?"
    elsif check(/\w(?=\w)/) then # ternary, also
      return result EXPR_BEG, :tEH, "?"
    end

    c = if scan(/\\/) then
          self.read_escape
        else
          getch
        end

    result EXPR_END, :tSTRING, c
  end

  def process_simple_string text
    replacement = text[1..-2]
    newlines =  replacement.count("\n")
    replacement.gsub!(ESC) { unescape($1).b.force_encoding Encoding::UTF_8 }

    replacement = replacement.b unless replacement.valid_encoding?

    r = result EXPR_END, :tSTRING, replacement
    self.lineno += newlines
    r
  end

  def process_slash text
    if is_beg? then
      string STR_REGEXP, matched

      return result nil, :tREGEXP_BEG, "/"
    end

    if scan(/\=/) then
      return result(EXPR_BEG, :tOP_ASGN, "/")
    end

    if is_arg? && space_seen then
      unless scan(/\s/) then
        arg_ambiguous
        string STR_REGEXP, "/"
        return result(nil, :tREGEXP_BEG, "/")
      end
    end

    result :arg_state, :tDIVIDE, "/"
  end

  def process_square_bracket text
    self.paren_nest += 1

    token = nil

    if is_after_operator? then
      case
      when scan(/\]\=/) then
        self.paren_nest -= 1 # HACK? I dunno, or bug in MRI
        return result EXPR_ARG, :tASET, "[]="
      when scan(/\]/) then
        self.paren_nest -= 1 # HACK? I dunno, or bug in MRI
        return result EXPR_ARG, :tAREF, "[]"
      else
        rb_compile_error "unexpected '['"
      end
    elsif is_beg? then
      token = :tLBRACK
    elsif is_arg? && (space_seen || lex_state =~ EXPR_LABELED) then
      token = :tLBRACK
    else
      token = :tLBRACK2
    end

    cond.push false
    cmdarg.push false
    result EXPR_PAR, token, text
  end

  def process_string_or_heredoc                  # ../compare/parse30.y:9075
    if lex_strterm[0] == :heredoc then
      self.heredoc lex_strterm
    else
      self.parse_string lex_strterm
    end
  end

  def process_symbol text
    symbol = possibly_escape_string text, /^:\"/ # stupid emacs

    result EXPR_LIT, :tSYMBOL, symbol
  end

  def process_token text
    # matching: parse_ident in compare/parse23.y:7989
    # FIX: remove: self.last_state = lex_state

    token = self.token = text
    token << matched if scan(/[\!\?](?!=)/)

    tok_id =
      case
      when token =~ /[!?]$/ then
        :tFID
      when lex_state =~ EXPR_FNAME && scan(/=(?:(?![~>=])|(?==>))/) then
        # ident=, not =~ => == or followed by =>
        # TODO test lexing of a=>b vs a==>b
        token << matched
        :tIDENTIFIER
      when token =~ /^[A-Z]/ then
        :tCONSTANT
      else
        :tIDENTIFIER
      end

    if is_label_possible? and is_label_suffix? then
      scan(/:/)
      return result EXPR_LAB, :tLABEL, token
    end

    # TODO: mb == ENC_CODERANGE_7BIT && lex_state !~ EXPR_DOT
    if lex_state !~ EXPR_DOT then
      # See if it is a reserved word.
      keyword = RubyParserStuff::Keyword.keyword token

      return process_token_keyword keyword if keyword
    end

    # matching: compare/parse30.y:9039
    state = if lex_state =~ EXPR_BEG_ANY|EXPR_ARG_ANY|EXPR_DOT then
              cmd_state ? EXPR_CMDARG : EXPR_ARG
            elsif lex_state =~ EXPR_FNAME then
              EXPR_ENDFN
            else
              EXPR_END
            end
    self.lex_state = state

    tok_id = :tIDENTIFIER if tok_id == :tCONSTANT && is_local_id(token)

    if last_state !~ EXPR_DOT|EXPR_FNAME and
        (tok_id == :tIDENTIFIER) and # not EXPR_FNAME, not attrasgn
        lvar_defined?(token) then
      state = EXPR_END|EXPR_LABEL
    end

    result state, tok_id, token
  end

  def process_token_keyword keyword
    # matching MIDDLE of parse_ident in compare/parse23.y:8046
    state = lex_state

    return result(EXPR_ENDFN, keyword.id0, token) if lex_state =~ EXPR_FNAME

    self.lex_state = keyword.state
    self.command_start = true if lex_state =~ EXPR_BEG

    case
    when keyword.id0 == :kDO then # parse26.y line 7591
      case
      when lambda_beginning? then
        self.lpar_beg = nil # lambda_beginning? == FALSE in the body of "-> do ... end"
        self.paren_nest -= 1 # TODO: question this?
        result lex_state, :kDO_LAMBDA, token
      when cond.is_in_state then
        result lex_state, :kDO_COND, token
      when cmdarg.is_in_state && state != EXPR_CMDARG then
        result lex_state, :kDO_BLOCK, token
      else
        result lex_state, :kDO, token
      end
    when state =~ EXPR_PAD then
      result lex_state, keyword.id0, token
    when keyword.id0 != keyword.id1 then
      result EXPR_PAR, keyword.id1, token
    else
      result lex_state, keyword.id1, token
    end
  end

  def process_underscore text
    self.unscan # put back "_"

    if beginning_of_line? && scan(/\__END__(\r?\n|\Z)/) then
      ss.terminate
      [RubyLexer::EOF, RubyLexer::EOF]
    elsif scan(/#{IDENT_CHAR}+/) then
      process_token matched
    end
  end

  def rb_compile_error msg
    msg += ". near line #{self.lineno}: #{self.rest[/^.*/].inspect}"
    raise RubyParser::SyntaxError, msg
  end

  def read_escape flags = nil                    # ../compare/parse30.y:6712
    case
    when scan(/\\/) then                  # Backslash
      '\\'
    when scan(/n/) then                   # newline
      "\n"
    when scan(/t/) then                   # horizontal tab
      "\t"
    when scan(/r/) then                   # carriage-return
      "\r"
    when scan(/f/) then                   # form-feed
      "\f"
    when scan(/v/) then                   # vertical tab
      "\13"
    when scan(/a/) then                   # alarm(bell)
      "\007"
    when scan(/e/) then                   # escape
      "\033"
    when scan(/[0-7]{1,3}/) then          # octal constant
      (matched.to_i(8) & 0xFF).chr.force_encoding Encoding::UTF_8
    when scan(/x([0-9a-fA-F]{1,2})/) then # hex constant
      # TODO: force encode everything to UTF-8?
      match[1].to_i(16).chr.force_encoding Encoding::UTF_8
    when scan(/b/) then                   # backspace
      "\010"
    when scan(/s/) then                   # space
      " "
    when check(/M-\\u/) then
      debug 5
    when scan(/M-\\(?=.)/) then
      c = read_escape
      c[0] = (c[0].ord | 0x80).chr
      c
    when scan(/M-(\p{ASCII})/) then
      # TODO: ISCNTRL(c) -> goto eof
      c = match[1]
      c[0] = (c[0].ord | 0x80).chr
      c
    when check(/(C-|c)\\u/) then
      debug 6
    when scan(/(C-|c)\\?\?/) then
      127.chr
    when scan(/(C-|c)\\/) then
      c = read_escape
      c[0] = (c[0].ord & 0x9f).chr
      c
    when scan(/(?:C-|c)(.)/) then
      c = match[1]
      c[0] = (c[0].ord & 0x9f).chr
      c
    when scan(/^[89]/i) then # bad octal or hex... MRI ignores them :(
      matched
    when scan(/u(\h{4})/) then
      [match[1].to_i(16)].pack("U")
    when scan(/u(\h{1,3})/) then
      debug 7
      rb_compile_error "Invalid escape character syntax"
    when scan(/u\{(\h+(?: +\h+)*)\}/) then
      match[1].split.map { |s| s.to_i(16) }.pack("U*")
    when scan(/[McCx0-9]/) || end_of_stream? then
      rb_compile_error("Invalid escape character syntax")
    else
      getch
    end.dup
  end

  def regx_options                               # ../compare/parse30.y:6914
    newtok

    options = scan(/\p{Alpha}+/) || ""

    rb_compile_error("unknown regexp options: %s" % [options]) if
      options =~ /[^ixmonesu]/

    options
  end

  def reset
    @lineno            = 1 # HACK

    self.brace_nest    = 0
    self.command_start = true
    self.comments      = []
    self.lex_state     = EXPR_NONE
    self.lex_strterm   = nil
    self.lpar_beg      = nil
    self.paren_nest    = 0
    self.space_seen    = false
    self.string_nest   = 0
    self.token         = nil
    self.string_buffer = []

    self.cond.reset
    self.cmdarg.reset
  end

  def result new_state, token, text, line = self.lineno # :nodoc:
    new_state = self.arg_state if new_state == :arg_state
    self.lex_state = new_state if new_state

    [token, [text, line]]
  end

  def ruby22_label?
    ruby22plus? and is_label_possible?
  end

  def ruby22plus?
    parser.class.version >= 22
  end

  def ruby23plus?
    parser.class.version >= 23
  end

  def ruby24minus?
    parser.class.version <= 24
  end

  def ruby27plus?
    parser.class.version >= 27
  end

  def space_vs_beginning space_type, beg_type, fallback
    if is_space_arg? check(/./m) then
      warning "`**' interpreted as argument prefix"
      space_type
    elsif is_beg? then
      beg_type
    else
      # TODO: warn_balanced("**", "argument prefix");
      fallback
    end
  end

  def string type, beg, nnd = nil
    # label = (IS_LABEL_POSSIBLE() ? str_label : 0);
    # p->lex.strterm = NEW_STRTERM(str_dquote | label, '"', 0);
    # p->lex.ptok = p->lex.pcur-1;

    type |= STR_FUNC_LABEL if is_label_possible?
    self.lex_strterm = [:strterm, type, beg, nnd || "\0"]
  end

  def tokadd c                                  # ../compare/parse30.y:6548
    string_buffer << c
  end

  def tokadd_escape                              # ../compare/parse30.y:6840
    case
    when scan(/\\\n/) then
      # just ignore
    when scan(/\\([0-7]{1,3}|x[0-9a-fA-F]{1,2})/) then
      tokadd matched
    when scan(/\\([MC]-|c)(?=\\)/) then
      tokadd matched
      self.tokadd_escape
    when scan(/\\([MC]-|c)(.)/) then
      tokadd matched

      self.tokadd_escape if check(/\\/) # recurse if continued!
    when scan(/\\[McCx]/) then # all unprocessed branches from above have failed
      rb_compile_error "Invalid escape character syntax"
    when scan(/\\(.)/m) then
      chr, = self.captures

      tokadd "\\"
      tokadd chr
    else
      rb_compile_error "Invalid escape character syntax: %p" % [self.rest.lines.first]
    end
  end

  def tokadd_string func, term, paren           # ../compare/parse30.y:7020
    qwords = func =~ STR_FUNC_QWORDS
    escape = func =~ STR_FUNC_ESCAPE
    expand = func =~ STR_FUNC_EXPAND
    regexp = func =~ STR_FUNC_REGEXP

    paren_re = regexp_cache[paren] if paren != "\0"
    term_re  = if term == "\n"
                 /\r?\n/
               else
                 regexp_cache[term]
               end

    until end_of_stream? do
      case
      when paren_re && scan(paren_re) then
        self.string_nest += 1
      when scan(term_re) then
        if self.string_nest == 0 then
          self.pos -= 1 # TODO: ss.unscan 665 errors #$ HACK: why do we depend on this so hard?
          break # leave eos loop, go parse term in caller (heredoc or parse_string)
        else
          self.lineno += matched.count("\n")
          self.string_nest -= 1
        end

      when expand && check(/#[\$\@\{]/) then
        # do nothing since we used `check`
        break # leave eos loop
      when check(/\\/) then
        case
        when scan(/\\\n/) then
          self.lineno += 1
          case
          when qwords then
            tokadd "\n"
            next
          when expand then
            next if func !~ STR_FUNC_INDENT

            if term == "\n" then
              unscan     # rollback
              scan(/\\/) # and split
              scan(/\n/) # this is `matched`
              break
            end

            tokadd "\\"
            debug 9
          else
            unscan     # rollback
            scan(/\\/) # this is `matched`
          end
        when check(/\\\\/) then
          tokadd '\\' if escape
          nextc # ignore 1st \\
          nextc # for tokadd ss.matched, below
        when scan(/\\u/) then
          unless expand then
            tokadd "\\"
            next
          end

          tokadd_utf8 term, func, regexp

          next
        else
          scan(/\\/) # eat it, we know it's there

          return RubyLexer::EOF if end_of_stream?

          if scan(/\P{ASCII}/) then
            tokadd "\\" unless expand
            tokadd self.matched
            next
          end

          case
          when regexp then
            if term !~ SIMPLE_RE_META && scan(term_re) then
              tokadd matched
              next
            end

            self.pos -= 1 # TODO: ss.unscan 15 errors
            # HACK? decide whether to eat the \\ above
            if esc = tokadd_escape && end_of_stream? then
              debug 10
            end

            next # C's continue = Ruby's next
          when expand then
            tokadd "\\" if escape
            tokadd read_escape
            next
          when qwords && scan(/\s/) then
            # ignore backslashed spaces in %w
          when !check(term_re) && !(paren_re && check(paren_re)) then
            tokadd "\\"
            next
          else
            getch # slurp it too for matched below
          end
        end # inner case for /\\/

      when scan(/\P{ASCII}/) then
        # not currently checking encoding stuff -- drops to tokadd below
      when qwords && check(/\s/) then
        break # leave eos loop
      else
        self.getch                      # TODO: optimize?
        self.lineno += 1 if self.matched == "\n"
      end # big case

      tokadd self.matched
    end # until end_of_stream?

    if self.matched then
      self.matched
    elsif end_of_stream? then
      RubyLexer::EOF
    end
  end # tokadd_string

  def tokadd_utf8 term, func, regexp_literal    # ../compare/parse30.y:6646
    tokadd "\\u" if regexp_literal

    case
    when scan(/\h{4}/) then
      codepoint = [matched.to_i(16)].pack("U")

      tokadd regexp_literal ? matched : codepoint
    when scan(/\{\s*(\h{1,6}(?:\s+\h{1,6})*)\s*\}/) then
      codepoints = match[1].split.map { |s| s.to_i 16 }.pack("U")

      if regexp_literal then
        tokadd "{"
        tokadd match[1].split.join(" ")
        tokadd "}"
      else
        tokadd codepoints
      end
    else
      rb_compile_error "unterminated Unicode escape"
    end
  end

  def unescape s
    r = ESCAPES[s]

    return r if r

    x = case s
        when /^[0-7]{1,3}/ then
          ($&.to_i(8) & 0xFF).chr
        when /^x([0-9a-fA-F]{1,2})/ then
          $1.to_i(16).chr
        when /^M-(.)/ then
          ($1[0].ord | 0x80).chr
        when /^(C-|c)(.)/ then
          ($2[0].ord & 0x9f).chr
        when /^[89a-f]/i then # bad octal or hex... ignore? that's what MRI does :(
          s
        when /^[McCx0-9]/ then
          rb_compile_error("Invalid escape character syntax")
        when /u(\h{4})/ then
          [$1.delete("{}").to_i(16)].pack("U")
        when /u(\h{1,3})/ then
          rb_compile_error("Invalid escape character syntax")
        when /u\{(\h+(?:\s+\h+)*)\}/ then
          $1.split.map { |s| s.to_i(16) }.pack("U*")
        else
          s
        end
    x
  end

  def warning s
    # do nothing for now
  end

  def was_label?
    @was_label = ruby22_label?
    true
  end

  class State
    attr_accessor :n
    attr_accessor :names

    # TODO: take a shared hash of strings for inspect/to_s
    def initialize o, names
      raise ArgumentError, "bad state: %p" % [o] unless Integer === o # TODO: remove

      self.n = o
      self.names = names
    end

    def == o
      self.equal?(o) || (o.class == self.class && o.n == self.n)
    end

    def =~ v
      (self.n & v.n) != 0
    end

    def | v
      raise ArgumentError, "Incompatible State: %p vs %p" % [self, v] unless
        self.names == v.names
      self.class.new(self.n | v.n, self.names)
    end

    def inspect
      return "Value(0)" if n.zero? # HACK?

      names.map { |v, k| k if self =~ v }.
        compact.
        join("|").
        gsub(/(?:EXPR_|STR_(?:FUNC_)?)/, "")
    end

    alias to_s inspect

    module Values
      expr_names = {}

      EXPR_NONE    = State.new    0x0, expr_names
      EXPR_BEG     = State.new    0x1, expr_names
      EXPR_END     = State.new    0x2, expr_names
      EXPR_ENDARG  = State.new    0x4, expr_names
      EXPR_ENDFN   = State.new    0x8, expr_names
      EXPR_ARG     = State.new   0x10, expr_names
      EXPR_CMDARG  = State.new   0x20, expr_names
      EXPR_MID     = State.new   0x40, expr_names
      EXPR_FNAME   = State.new   0x80, expr_names
      EXPR_DOT     = State.new  0x100, expr_names
      EXPR_CLASS   = State.new  0x200, expr_names
      EXPR_LABEL   = State.new  0x400, expr_names
      EXPR_LABELED = State.new  0x800, expr_names
      EXPR_FITEM   = State.new 0x1000, expr_names

      EXPR_BEG_ANY = EXPR_BEG | EXPR_MID    | EXPR_CLASS
      EXPR_ARG_ANY = EXPR_ARG | EXPR_CMDARG
      EXPR_END_ANY = EXPR_END | EXPR_ENDARG | EXPR_ENDFN

      # extra fake lex_state names to make things a bit cleaner

      EXPR_LAB = EXPR_ARG|EXPR_LABELED
      EXPR_LIT = EXPR_END|EXPR_ENDARG
      EXPR_PAR = EXPR_BEG|EXPR_LABEL
      EXPR_PAD = EXPR_BEG|EXPR_LABELED

      EXPR_NUM = EXPR_LIT

      expr_names.merge!(EXPR_NONE    => "EXPR_NONE",
                        EXPR_BEG     => "EXPR_BEG",
                        EXPR_END     => "EXPR_END",
                        EXPR_ENDARG  => "EXPR_ENDARG",
                        EXPR_ENDFN   => "EXPR_ENDFN",
                        EXPR_ARG     => "EXPR_ARG",
                        EXPR_CMDARG  => "EXPR_CMDARG",
                        EXPR_MID     => "EXPR_MID",
                        EXPR_FNAME   => "EXPR_FNAME",
                        EXPR_DOT     => "EXPR_DOT",
                        EXPR_CLASS   => "EXPR_CLASS",
                        EXPR_LABEL   => "EXPR_LABEL",
                        EXPR_LABELED => "EXPR_LABELED",
                        EXPR_FITEM   => "EXPR_FITEM")

      # ruby constants for strings

      str_func_names = {}

      STR_FUNC_BORING = State.new 0x00,    str_func_names
      STR_FUNC_ESCAPE = State.new 0x01,    str_func_names
      STR_FUNC_EXPAND = State.new 0x02,    str_func_names
      STR_FUNC_REGEXP = State.new 0x04,    str_func_names
      STR_FUNC_QWORDS = State.new 0x08,    str_func_names
      STR_FUNC_SYMBOL = State.new 0x10,    str_func_names
      STR_FUNC_INDENT = State.new 0x20,    str_func_names # <<-HEREDOC
      STR_FUNC_LABEL  = State.new 0x40,    str_func_names
      STR_FUNC_LIST   = State.new 0x4000,  str_func_names
      STR_FUNC_TERM   = State.new 0x8000,  str_func_names
      STR_FUNC_DEDENT = State.new 0x10000, str_func_names # <<~HEREDOC

      # TODO: check parser25.y on how they do STR_FUNC_INDENT

      STR_SQUOTE = STR_FUNC_BORING
      STR_DQUOTE = STR_FUNC_EXPAND
      STR_XQUOTE = STR_FUNC_EXPAND
      STR_REGEXP = STR_FUNC_REGEXP | STR_FUNC_ESCAPE | STR_FUNC_EXPAND
      STR_SWORD  = STR_FUNC_QWORDS | STR_FUNC_LIST
      STR_DWORD  = STR_FUNC_QWORDS | STR_FUNC_EXPAND | STR_FUNC_LIST
      STR_SSYM   = STR_FUNC_SYMBOL
      STR_DSYM   = STR_FUNC_SYMBOL | STR_FUNC_EXPAND
      STR_LABEL  = STR_FUNC_LABEL

      str_func_names.merge!(STR_FUNC_ESCAPE => "STR_FUNC_ESCAPE",
                            STR_FUNC_EXPAND => "STR_FUNC_EXPAND",
                            STR_FUNC_REGEXP => "STR_FUNC_REGEXP",
                            STR_FUNC_QWORDS => "STR_FUNC_QWORDS",
                            STR_FUNC_SYMBOL => "STR_FUNC_SYMBOL",
                            STR_FUNC_INDENT => "STR_FUNC_INDENT",
                            STR_FUNC_LABEL  => "STR_FUNC_LABEL",
                            STR_FUNC_LIST   => "STR_FUNC_LIST",
                            STR_FUNC_TERM   => "STR_FUNC_TERM",
                            STR_FUNC_DEDENT => "STR_FUNC_DEDENT",
                            STR_SQUOTE      => "STR_SQUOTE")
    end

    include Values
  end

  include State::Values
end

class RubyLexer
  module SSWrapper
    def beginning_of_line?
      ss.bol?
    end

    alias bol? beginning_of_line? # to make .rex file more readable

    def captures
      ss.captures
    end

    def check re
      maybe_pop_stack

      ss.check re
    end

    def end_of_stream?
      ss.eos?
    end

    alias eos? end_of_stream?

    def getch
      c = ss.getch
      c = ss.getch if c == "\r" && ss.peek(1) == "\n"
      c
    end

    def matched
      ss.matched
    end

    def maybe_pop_stack
      if ss.eos? && ss_stack.size > 1 then
        ss_pop
        lineno_pop
      end
    end

    def nextc
      # TODO:
      # if (UNLIKELY((p->lex.pcur == p->lex.pend) || p->eofp || RTEST(p->lex.nextline))) {
      #     if (nextline(p)) return -1;
      # }

      maybe_pop_stack

      c = ss.getch

      if c == "\n" then
        ss.unscan
        c = nil
      end

      c
    end

    def pos
      ss.pos
    end

    def pos= n
      ss.pos = n
    end

    def rest
      ss.rest
    end

    def scan re
      warn "Use nextc instead of scan(/./). From #{caller.first}" if re == /./

      maybe_pop_stack

      ss.scan re
    end

    def scanner_class # TODO: design this out of oedipus_lex. or something.
      RPStringScanner
    end

    def ss_string
      ss.string
    end

    def ss_string= s
      raise "Probably not"
      ss.string = s
    end

    def unscan
      ss.unscan
    end
  end

  include SSWrapper
end

class RubyLexer
  module SSStack
    def ss_stack_rest
      ss_stack.map(&:rest).reverse
    end

    def ss_stack
      @ss_stack ||= [@ss]
    end

    def lineno_stack
      @lineno_stack ||= []
    end

    def lineno_push n
      lineno_stack.push n
    end

    def lineno_pop
      self.lineno = lineno_stack.pop
    end

    def ss
      warn "EMPTY?!?!" if ss_stack.empty? or !ss_stack.last
      ss_stack.last
    end

    alias :match :ss # appease the alias gods

    def ss= o
      ss_stack.clear
      ss_push o
    end

    def ss_push ss
      ss_stack.push ss
    end

    def ss_pop
      ss_stack.pop
    end
  end

  prepend SSStack
end

if ENV["RP_LINENO_DEBUG"] then
  class RubyLexer
    def d o
      $stderr.puts o.inspect
    end

    alias old_lineno= lineno=

    def lineno= n
      self.old_lineno= n
      where = caller.first.split(/:/).first(2).join(":")
      $stderr.puts
      d :lineno => [n, where]
    end
  end
end

if ENV["RP_STRTERM_DEBUG"] then
  class RubyLexer
    def d o
      $stderr.puts o.inspect
    end

    alias old_lex_strterm= lex_strterm=

    def lex_strterm= o
      self.old_lex_strterm= o
      where = caller.first.split(/:/).first(2).join(":")
      $stderr.puts
      d :lex_strterm => [o, where]
    end
  end
end

require "ruby_lexer.rex"
