$: << File.expand_path("~/Work/p4/zss/src/ParseTree/dev/lib") # for me, not you.
require 'sexp'
require 'ruby_parser_extras'

class RubyLexer
  attr_accessor :command_start
  attr_accessor :cmdarg
  attr_accessor :cond
  attr_accessor :nest

  # Additional context surrounding tokens that both the lexer and
  # grammar use.
  attr_reader :lex_state

  attr_accessor :lex_strterm

  # Stream of data that yylex examines.
  attr_reader :src

  # Last token read via yylex.
  attr_accessor :token

  # Tempory buffer to build up a potential token.  Consumer takes
  # responsibility to reset this before use.
  attr_accessor :token_buffer

  # Value of last token which had a value associated with it.
  attr_accessor :yacc_value

  # What handles warnings
  attr_accessor :warnings

  # Give a name to a value.  Enebo: This should be used more.
  # HACK OMG HORRIBLE KILL ME NOW. Enebo, no. this shouldn't be used more
  EOF = nil # was 0... ugh

  # ruby constants for strings (should this be moved somewhere else?)
  STR_FUNC_PLAIN  = 0x00
  STR_FUNC_ESCAPE = 0x01 # TODO: remove and replace with REGEXP
  STR_FUNC_EXPAND = 0x02
  STR_FUNC_REGEXP = 0x04
  STR_FUNC_AWORDS  = 0x08
  STR_FUNC_SYMBOL = 0x10
  STR_FUNC_INDENT = 0x20 # <<-HEREDOC

  STR_SQUOTE = 0
  STR_DQUOTE = STR_FUNC_EXPAND
  STR_XQUOTE = STR_FUNC_EXPAND
  STR_REGEXP = STR_FUNC_REGEXP | STR_FUNC_ESCAPE | STR_FUNC_EXPAND
  STR_SSYM   = STR_FUNC_SYMBOL
  STR_DSYM   = STR_FUNC_SYMBOL | STR_FUNC_EXPAND

  # How the parser advances to the next token.
  #
  # @return true if not at end of file (EOF).

  def advance
    r = yylex
    self.token = r
    return r != RubyLexer::EOF
  end

  def arg_ambiguous
    self.warning("Ambiguous first argument. make sure.")
  end

  def comments
    c = @comments.join
    @comments.clear
    c
  end

  def heredoc here # Region has 63 lines, 1595 characters
    _, eos, func, last_line = here

    indent  = (func & STR_FUNC_INDENT) != 0
    expand  = (func & STR_FUNC_EXPAND) != 0
    eos_re  = indent ? /[ \t]*#{eos}(\r?\n|\z)/ : /#{eos}(\r?\n|\z)/
    err_msg = "can't match #{eos_re.inspect} anywhere in "

    rb_compile_error err_msg if
      src.eos?

    if src.beginning_of_line? && src.scan(eos_re) then
      src.unread_many last_line # TODO: figure out how to remove this
      self.yacc_value = t(eos)
      return :tSTRING_END
    end

    token_buffer.clear

    if expand then
      case
      when src.scan(/#[$@]/) then
        src.pos -= 1 # FIX omg stupid
        self.yacc_value = t(src.matched)
        return :tSTRING_DVAR
      when src.scan(/#[{]/) then
        self.yacc_value = t(src.matched)
        return :tSTRING_DBEG
      when src.scan(/#/) then
        token_buffer << '#'
      end

      until src.scan(eos_re) do
        c = tokadd_string func, "\n", nil, token_buffer

        rb_compile_error err_msg if
          c == RubyLexer::EOF

        if c != "\n" then
          self.yacc_value = s(:str, token_buffer.join.delete("\r"))
          return :tSTRING_CONTENT
        else
          token_buffer << src.getch # always \n
        end

        rb_compile_error err_msg if
          src.eos?
      end

      # tack on a NL after the heredoc token - FIX NL should not be needed
      src.unread_many(eos + "\n") # TODO: remove this... stupid stupid stupid
    else
      until src.check(eos_re) do
        token_buffer << src.scan(/.*(\n|\z)/)
        rb_compile_error err_msg if
          src.eos?
      end
    end

    self.lex_strterm = s(:heredoc, eos, func, last_line)
    self.yacc_value = s(:str, token_buffer.join.delete("\r"))

    return :tSTRING_CONTENT
  end

  def heredoc_identifier
    term, func = nil, STR_FUNC_PLAIN
    token_buffer.clear

    case
    when src.scan(/(-?)(['"`])(.*?)\2/) then
      term = src[2]
      unless src[1].empty? then
        func |= STR_FUNC_INDENT
      end
      func |= case term
              when "\'" then
                STR_SQUOTE
              when '"' then
                STR_DQUOTE
              else
                STR_XQUOTE
              end
      token_buffer << src[3]
    when src.scan(/-?(['"`])(?!\1*\Z)/) then
      rb_compile_error "unterminated here document identifier"
    when src.scan(/(-?)(\w+)/) then
      term = '"'
      func |= STR_DQUOTE
      unless src[1].empty? then
        func |= STR_FUNC_INDENT
      end
      token_buffer << src[2]
    else
      return nil
    end

    if src.check(/.*\n/) then
      # TODO: think about storing off the char range instead
      line = src.string[src.pos, src.matched_size]
      src.string[src.pos, src.matched_size] = ''
    else
      line = nil
    end

    self.lex_strterm = s(:heredoc, token_buffer.join, func, line)

    if term == '`' then
      self.yacc_value = t("`")
      return :tXSTRING_BEG
    else
      self.yacc_value = t("\"")
      return :tSTRING_BEG
    end
  end

  def initialize
    self.token_buffer = []
    self.cond = StackState.new(:cond)
    self.cmdarg = StackState.new(:cmdarg)
    self.nest = 0
    @comments = []

    reset
  end

  def int_with_base base
    if src.matched =~ /__/ then
      rb_compile_error "Invalid numeric format"
    end
    self.yacc_value = src.matched.to_i(base)
    return :tINTEGER
  end

  def lex_state= o
    raise "wtf?" unless Symbol === o
    @lex_state = o
  end

  ##
  #  Parse a number from the input stream.
  #
  # @param c The first character of the number.
  # @return A int constant wich represents a token.

  def parse_number c
    self.lex_state = :expr_end

    src.unscan # put number back in - stupid lexer...

    case
    when src.scan(/[+-]?0[xbd]\b/) then
      rb_compile_error "Invalid numeric format"
    when src.scan(/[+-]?0x[a-f0-9_]+/i) then
      return int_with_base(16)
    when src.scan(/[+-]?0b[01_]+/) then
      return int_with_base(2)
    when src.scan(/[+-]?0d[0-9_]+/) then
      return int_with_base(10)
    when src.scan(/[+-]?0o?[0-7_]*[89]/) then
      rb_compile_error "Illegal octal digit."
    when src.scan(/[+-]?0o?[0-7_]+|0o/) then
      return int_with_base(8)
    when src.scan(/[+-]?[\d_]+_(e|\.)/) then
      rb_compile_error "Trailing '_' in number."
    when src.scan(/[+-]?[\d_]+\.[\d_]+(e[+-]?[\d_]+)?\b|[+-]?[\d_]+e[+-]?[\d_]+\b/) then
      number = src.matched
      if number =~ /__/ then
        rb_compile_error "Invalid numeric format"
      end
      self.yacc_value = number.to_f
      return :tFLOAT
    when src.scan(/[+-]?0\b/) then
      return int_with_base(10)
    when src.scan(/[+-]?[\d_]+\b/) then
      return int_with_base(10)
    else
      rb_compile_error "Bad number format"
    end
  end

  def parse_quote
    beg, nnd, short_hand, c = nil, nil, false, nil

    if src.scan(/[a-z0-9]{1,2}/i) then # Long-hand (e.g. %Q{}).
      rb_compile_error "unknown type of %string" if
        src.matched_size == 2
      c, beg, short_hand = src.matched, src.getch, false
    else                               # Short-hand (e.g. %{, %., %!, etc)
      c, beg, short_hand = 'Q', src.getch, true
    end

    if c == RubyLexer::EOF or beg == RubyLexer::EOF then
      rb_compile_error "unterminated quoted string meets end of file"
    end

    # Figure nnd-char.  "\0" is special to indicate beg=nnd and that no nesting?
    nnd = { "(" => ")", "[" => "]", "{" => "}", "<" => ">" }[beg]
    nnd, beg = beg, "\0" if nnd.nil?

    token_type, self.yacc_value = nil, t("%#{c}#{beg}")
    token_type, string_type = case c
                              when 'Q' then
                                ch = short_hand ? nnd : c + beg
                                self.yacc_value = t("%#{ch}")
                                [:tSTRING_BEG,   STR_DQUOTE]
                              when 'q' then
                                [:tSTRING_BEG,   STR_SQUOTE]
                              when 'W' then
                                src.scan(/\s*/)
                                [:tWORDS_BEG,    STR_DQUOTE | STR_FUNC_AWORDS]
                              when 'w' then
                                src.scan(/\s*/)
                                [:tAWORDS_BEG,   STR_SQUOTE | STR_FUNC_AWORDS]
                              when 'x' then
                                [:tXSTRING_BEG,  STR_XQUOTE]
                              when 'r' then
                                [:tREGEXP_BEG,   STR_REGEXP]
                              when 's' then
                                self.lex_state  = :expr_fname
                                [:tSYMBEG,       STR_SSYM]
                              end

    rb_compile_error "Bad %string type. Expected [Qqwxr\W], found '#{c}'." if
      token_type.nil?

    self.lex_strterm = s(:strterm, string_type, nnd, beg)

    return token_type
  end

  def parse_string(quote)
    _, string_type, term, open = quote

    space = false # FIX: remove these
    func = string_type
    paren = open
    term_re = Regexp.escape term

    awords = (func & STR_FUNC_AWORDS) != 0
    regexp = (func & STR_FUNC_REGEXP) != 0
    expand = (func & STR_FUNC_EXPAND) != 0

    unless func then
      return :tSTRING_END
    end

    space = true if awords and src.scan(/\s+/)

    if self.nest == 0 && src.scan(/#{term_re}/) then
      if awords then
        quote[1] = nil
        return ' '
      elsif regexp then
        self.yacc_value = self.regx_options
        return :tREGEXP_END
      else
        self.yacc_value = t(term)
        return :tSTRING_END
      end
    end

    if space then
      return ' '
    end

    self.token_buffer.clear

    if expand
      case
      when src.scan(/#(?=[$@])/) then
        return :tSTRING_DVAR
      when src.scan(/#[{]/) then
        return :tSTRING_DBEG
      when src.scan(/#/) then
        token_buffer << '#'
      end
    end

    if tokadd_string(func, term, paren, token_buffer) == RubyLexer::EOF then
      rb_compile_error "unterminated string meets end of file"
    end

    self.yacc_value = s(:str, token_buffer.join)
    return :tSTRING_CONTENT
  end

  def rb_compile_error msg
    msg += ". near line #{src.lineno}: #{src.rest[/^.*/].inspect}"
    raise SyntaxError, msg
  end

  def read_escape
    c = case
        when src.scan(/\\/) then                   # Backslash
          '\\'
        when src.scan(/n/) then                    # newline
          "\n"
        when src.scan(/t/) then                    # horizontal tab
          "\t"
        when src.scan(/r/) then                    # carriage-return
          "\r"
        when src.scan(/f/) then                    # form-feed
          "\f"
        when src.scan(/v/) then                    # vertical tab
          "\13"
        when src.scan(/a/) then                    # alarm(bell)
          "\007"
        when src.scan(/e/) then                    # escape
          "\033"
        when src.scan(/b/) then                    # backspace
          "\010"
        when src.scan(/s/) then                    # space
          " "
        when src.scan(/[0-7]{1,3}/) then           # octal constant
          src.matched.to_i(8).chr
        when src.scan(/x([0-9a-fA-Fa-f]{2})/) then # hex constant
          src[1].to_i(16).chr
        when src.scan(/M-\\/) then
          c = self.read_escape
          c[0] |= 0x80
          c
        when src.scan(/M-(.)/) then
          c = src[1]
          c[0] |= 0x80
          c
        when src.scan(/C-\\|c\\/) then
          c = self.read_escape
          c[0] &= 0x9f
          c
        when src.scan(/C-\?|c\?/) then
          0177.chr
        when src.scan(/(C-|c)(.)/) then
          c = src[2]
          c[0] &= 0x9f
          c
        when src.scan(/[McCx0-9]/) || src.eos? then
          rb_compile_error("Invalid escape character syntax")
        else
          src.getch
        end
    c
  end

  def regx_options
    good, bad = [], []

    if src.scan(/[a-z]+/) then
      good, bad = src.matched.split(//).partition { |s| s =~ /^[ixmonesu]$/ }
    end

    unless bad.empty? then
      rb_compile_error("unknown regexp option%s - %s" %
                       [(bad.size > 1 ? "s" : ""), bad.join.inspect])
    end

    return good.join
  end

  def reset
    self.command_start = true
    self.lex_strterm = nil
    self.token = nil
    self.yacc_value = nil

    @src = nil
    @lex_state = nil
  end

  def src= src
    raise "bad src: #{src.inspect}" unless String === src
    @src = StringScanner.new src
  end

  def store_comment
    @comments.push(*self.token_buffer)
    self.token_buffer.clear
  end

  def tokadd_escape term
    case
    when src.scan(/\n/) then
      # just ignore
    when src.scan(/[0-7]{1,3}|x[0-9a-fA-F]{2}/) then
      self.token_buffer << "\\" << src.matched
    when src.scan(/([MC]-|c)\\/) then
      self.token_buffer << "\\" << src[1]
      self.tokadd_escape term
    when src.scan(/([MC]-|c)(.)/) then
      self.token_buffer << "\\" << src.matched
    when src.scan(/[McCx]/) || src.eos? then
      rb_compile_error "Invalid escape character syntax"
    else
      c = src.getch
      if (c != "\\" || c != term)
        self.token_buffer << "\\"
      end
      self.token_buffer << c
    end
  end

  def tokadd_string(func, term, paren, buffer)
    awords = (func & STR_FUNC_AWORDS) != 0
    escape = (func & STR_FUNC_ESCAPE) != 0
    expand = (func & STR_FUNC_EXPAND) != 0
    regexp = (func & STR_FUNC_REGEXP) != 0
    symbol = (func & STR_FUNC_SYMBOL) != 0

    paren_re = paren.nil? ? nil : Regexp.new(Regexp.escape(paren))
    term_re  = Regexp.new(Regexp.escape(term))

    until src.eos? do
      c = nil
      case
      when paren_re && src.scan(paren_re) then
        self.nest += 1
      when self.nest == 0 && src.scan(term_re) then
        src.pos -= 1
        break
      when src.scan(term_re) then
        self.nest -= 1
      when ((awords && src.scan(/\s/)) ||
            (expand && src.scan(/#(?=[\$\@\{])/))) then
        src.pos -= 1
        break
      when awords && src.scan(/\\\n/) then
        buffer << "\n"
        next
      when expand && src.scan(/\\\n/) then
        next
      when ((expand && src.scan(/#(?!\n)/)) ||
            src.scan(/\\\n/) ||
            (awords && src.scan(/\\\s/))) then
        # do nothing
      when src.scan(/\\\\/) then
        if escape then
          buffer << "\\"
          next
        end
      when regexp && src.scan(/\\/) then
        self.tokadd_escape term
        next
      when expand && src.scan(/\\/) then
        c = self.read_escape
      when src.scan(/\\/) then
        c = src.getch
        if c != term && !(paren && c == paren) then
          buffer << "\\"
        end
        # \\ case:
        # else if (ismbchar(c)) {
        #   int i, len = mbclen(c)-1;
        #   for (i = 0; i < len; i++) {
        #     tokadd(c);
        #     c = nextc();
        #   }
        # }
      else
        c = src.getch
        if symbol && src.scan(/\0/) then # TODO: why after getch?
          rb_compile_error "symbol cannot contain '\\0'"
        end
      end

      c = src.matched unless c
      buffer << c
    end # until
    c = src.matched unless c

    return RubyLexer::EOF if src.eos?

    return c
  end

  def warning s
    # do nothing for now
  end

  def temp_handle_strterm
    token = nil

    if lex_strterm[0] == :heredoc then
      token = self.heredoc(lex_strterm)
      if token == :tSTRING_END then
        self.lex_strterm = nil
        self.lex_state = :expr_end
      end
    else
      token = self.parse_string(lex_strterm)

      if token == :tSTRING_END || token == :tREGEXP_END then
        self.lex_strterm = nil
        self.lex_state = :expr_end
      end
    end

    return token
  end

  ##
  # Returns the next token. Also sets yy_val is needed.
  #
  # @return Description of the Returned Value
  # TODO: remove ALL sexps coming from here and move up to grammar
  # TODO: only literal values should come up from the lexer.

  def yylex # Region has 796 lines, 22436 characters
    c = ''
    space_seen = false
    command_state = false

    if lex_strterm then
      return temp_handle_strterm
    end

    command_state = self.command_start
    self.command_start = false

    last_state = lex_state

    loop do
      c = src.getch
      case c
      when /\004|\032|\000/, RubyLexer::EOF then # ^D, ^Z, EOF
        return RubyLexer::EOF
      when /\ |\t|\f|\r|\13/ then # white spaces, 13 = '\v
        space_seen = true
        next
      when /#|\n/ then
        if c == '#' then
          src.unread c # god this lexer is lame
          token_buffer.clear

          while src.scan(/\s*#.*(\n+|\z)/) do
            token_buffer << src.matched.gsub(/^ +#/, '#').gsub(/^ +$/, '')
          end

          self.store_comment

          if src.eos? then
            return RubyLexer::EOF
          end
        end
        # Replace a string of newlines with a single one

        src.scan(/\n+/)

        if (lex_state == :expr_beg   ||
            lex_state == :expr_fname ||
            lex_state == :expr_dot   ||
            lex_state == :expr_class) then
          next
        end

        self.command_start = true
        self.lex_state = :expr_beg
        return "\n"
      when '*' then
        c = src.getch
        if c == '*' then
          c = src.getch
          if c == '=' then
            self.lex_state = :expr_beg
            self.yacc_value = t("**")
            return :tOP_ASGN
          end
          src.unread c
          self.yacc_value = t("**")
          c = :tPOW
        else
          if c == '=' then
            self.lex_state = :expr_beg
            self.yacc_value = t("*")
            return :tOP_ASGN
          end
          src.unread c
          if lex_state.is_argument && space_seen && c !~ /\s/ then
            warning("`*' interpreted as argument prefix")
            c = :tSTAR
          elsif lex_state == :expr_beg || lex_state == :expr_mid then
            c = :tSTAR
          else
            c = :tSTAR2
          end
          self.yacc_value = t("*")
        end

        if lex_state == :expr_fname || lex_state == :expr_dot then
          self.lex_state = :expr_arg
        else
          self.lex_state = :expr_beg
        end

        return c
      when '!' then
        self.lex_state = :expr_beg
        if (c = src.getch) == '=' then
          self.yacc_value = t("!=")
          return :tNEQ
        end
        if c == '~' then
          self.yacc_value = t("!~")
          return :tNMATCH
        end
        src.unread(c)
        self.yacc_value = t("!")
        return :tBANG
      when '=' then

        # documentation nodes - FIX: cruby much cleaner w/ lookahead
        if src.was_begin_of_line and src.scan(/begin(?=\s)/) then
          self.token_buffer.clear
          self.token_buffer << "=begin"
          c = src.getch

          if c =~ /\s/ then
            # In case last next was the newline.
            src.unread(c)

            loop do
              c = src.getch
              token_buffer << c

              # If a line is followed by a blank line put it back.
              while c == "\n"
                c = src.getch
                token_buffer << c
              end

              if c == RubyLexer::EOF then
                rb_compile_error "embedded document meets end of file"
              end

              unless c == '=' then
                next
              end

              if src.was_begin_of_line && src.scan(/end/) then
                token_buffer << "end"
                token_buffer << src.scan(/.*\n/)
                src.unread "\n"
                break
              end
            end # loop

            self.store_comment

            next
          end
        end

        case lex_state
        when :expr_fname, :expr_dot then
          self.lex_state = :expr_arg
        else
          self.lex_state = :expr_beg
        end

        c = src.getch

        case c
        when '=' then
          c = src.getch
          if c == '=' then
            self.yacc_value = t("===")
            return :tEQQ
          end
          src.unread(c)
          self.yacc_value = t("==")
          return :tEQ
        when '~' then
          self.yacc_value = t("=~")
          return :tMATCH
        when '>' then
          self.yacc_value = t("=>")
          return :tASSOC
        end

        src.unread(c)
        self.yacc_value = t("=")
        return '='
      when '<' then
        c = src.getch
        if (c == '<' &&
            lex_state != :expr_end &&
            lex_state != :expr_dot &&
            lex_state != :expr_endarg &&
            lex_state != :expr_class &&
            (!lex_state.is_argument || space_seen)) then
          tok = self.heredoc_identifier
          if tok then
            return tok
          end
        end
        if lex_state == :expr_fname || lex_state == :expr_dot then
          self.lex_state = :expr_arg
        else
          self.lex_state = :expr_beg
        end
        if c == '=' then
          if (c = src.getch) == '>' then
            self.yacc_value = t("<=>")
            return :tCMP
          end
          src.unread c
          self.yacc_value = t("<=")
          return :tLEQ
        end
        if c == '<' then
          if (c = src.getch) == '=' then
            self.lex_state = :expr_beg
            self.yacc_value = t("\<\<")
            return :tOP_ASGN
          end
          src.unread(c)
          self.yacc_value = t("<<")
          return :tLSHFT
        end
        self.yacc_value = t("<")
        src.unread(c)
        return :tLT
      when '>' then
        if lex_state == :expr_fname || lex_state == :expr_dot then
          self.lex_state = :expr_arg
        else
          self.lex_state = :expr_beg
        end

        if (c = src.getch) == '=' then
          self.yacc_value = t(">=")
          return :tGEQ
        end
        if c == '>' then
          if (c = src.getch) == '=' then
            self.lex_state = :expr_beg
            self.yacc_value = t(">>")
            return :tOP_ASGN
          end
          src.unread c
          self.yacc_value = t(">>")
          return :tRSHFT
        end
        src.unread c
        self.yacc_value = t(">")
        return :tGT
      when '"' then
        self.lex_strterm = s(:strterm, STR_DQUOTE, '"', "\0") # TODO: question this
        self.yacc_value = t("\"")
        return :tSTRING_BEG
      when '`' then
        self.yacc_value = t("`")
        case lex_state
        when :expr_fname then
          self.lex_state = :expr_end
          return :tBACK_REF2
        when :expr_dot then
          self.lex_state = if command_state then
                             :expr_cmdarg
                           else
                             :expr_arg
                           end
          return :tBACK_REF2
        end
        self.lex_strterm = s(:strterm, STR_XQUOTE, '`', "\0")
        return :tXSTRING_BEG
      when "\'" then
        self.lex_strterm = s(:strterm, STR_SQUOTE, "\'", "\0")
        self.yacc_value = t("'")
        return :tSTRING_BEG
      when '?' then
        if lex_state == :expr_end || lex_state == :expr_endarg then
          self.lex_state = :expr_beg
          self.yacc_value = t("?")
          return '?'
        end

        c = src.getch

        if c == RubyLexer::EOF then
          rb_compile_error "incomplete character syntax"
        end

        if c =~ /\s|\v/ then
          unless lex_state.is_argument then
            c2 = case c
                 when " " then
                   's'
                 when "\n" then
                   'n'
                 when "\t" then
                   't'
                 when "\v" then
                   'v'
                 when "\r" then
                   'r'
                 when "\f" then
                   'f'
                 end

            if c2 then
              warning("invalid character syntax; use ?\\" + c2)
            end
          end

          # ternary
          src.unread c
          self.lex_state = :expr_beg
          self.yacc_value = t("?")
          return '?'
#         elsif ismbchar(c) then # ternary, also
#           rb_warn("multibyte character literal not supported yet; use ?\\" + c)
#           support.unread c
#           self.lex_state = :expr_beg
#           return '?'
        elsif c =~ /\w/ && src.check(/\w/) then
          # ternary, also
          src.unread c
          self.lex_state = :expr_beg
          self.yacc_value = t("?")
          return '?'
        elsif c == "\\" then
          c = self.read_escape
        end
        c[0] &= 0xff
        self.lex_state = :expr_end
        self.yacc_value = c[0]
        return :tINTEGER
      when '&' then
        if (c = src.getch) == '&' then
          self.lex_state = :expr_beg
          if (c = src.getch) == '=' then
            self.yacc_value = t("&&")
            self.lex_state = :expr_beg
            return :tOP_ASGN
          end
          src.unread c
          self.yacc_value = t("&&")
          return :tANDOP
        elsif c == '=' then
          self.yacc_value = t("&")
          self.lex_state = :expr_beg
          return :tOP_ASGN
        end

        src.unread c

        if lex_state.is_argument && space_seen && c !~ /\s/ then
          warning("`&' interpreted as argument prefix")
          c = :tAMPER
        elsif lex_state == :expr_beg || lex_state == :expr_mid then
          c = :tAMPER
        else
          c = :tAMPER2
        end

        if lex_state == :expr_fname || lex_state == :expr_dot then
          self.lex_state = :expr_arg
        else
          self.lex_state = :expr_beg
        end
        self.yacc_value = t("&")
        return c
      when '|' then
        if (c = src.getch) == '|' then
          self.lex_state = :expr_beg
          if (c = src.getch) == '=' then
            self.lex_state = :expr_beg
            self.yacc_value = t("||")
            return :tOP_ASGN
          end
          src.unread c
          self.yacc_value = t("||")
          return :tOROP
        end
        if c == '=' then
          self.lex_state = :expr_beg
          self.yacc_value = t("|")
          return :tOP_ASGN
        end

        if lex_state == :expr_fname || lex_state == :expr_dot then
          self.lex_state = :expr_arg
        else
          self.lex_state = :expr_beg
        end

        src.unread c
        self.yacc_value = t("|")
        return :tPIPE
      when '+' then
        c = src.getch

        if lex_state == :expr_fname || lex_state == :expr_dot then
          self.lex_state = :expr_arg
          if c == '@' then
            self.yacc_value = t("+@")
            return :tUPLUS
          end
          src.unread c
          self.yacc_value = t("+")
          return :tPLUS
        end

        if c == '=' then
          self.lex_state = :expr_beg
          self.yacc_value = t("+")
          return :tOP_ASGN
        end

        if (lex_state == :expr_beg || lex_state == :expr_mid ||
            (lex_state.is_argument && space_seen && c !~ /\s/)) then
          if lex_state.is_argument then
            arg_ambiguous
          end
          self.lex_state = :expr_beg
          if c =~ /\d/ then
            src.unscan     # HACK - these 3 lines... this lexer is so lame
            src.pos -= 1
            c = src.getch
            return parse_number(c)
          end
          src.unread c
          self.yacc_value = t("+")
          return :tUPLUS
        end
        self.lex_state = :expr_beg
        src.unread c
        self.yacc_value = t("+")
        return :tPLUS
      when '-' then
        c = src.getch

        if lex_state == :expr_fname || lex_state == :expr_dot then
          self.lex_state = :expr_arg
          if c == '@' then
            self.yacc_value = t("-@")
            return :tUMINUS
          end
          src.unread c
          self.yacc_value = t("-")
          return :tMINUS
        end
        if c == '=' then
          self.lex_state = :expr_beg
          self.yacc_value = t("-")
          return :tOP_ASGN
        end

        if (lex_state == :expr_beg || lex_state == :expr_mid ||
            (lex_state.is_argument && space_seen && c !~ /\s/)) then
          if lex_state.is_argument then
            arg_ambiguous
          end
          self.lex_state = :expr_beg
          src.unread c
          self.yacc_value = t("-")
          if c =~ /\d/ then
            return :tUMINUS_NUM
          end
          return :tUMINUS
        end
        self.lex_state = :expr_beg
        src.unread c
        self.yacc_value = t("-")
        return :tMINUS
      when '.' then
        self.lex_state = :expr_beg
        if (c = src.getch) == '.' then
          if (c = src.getch) == '.' then
            self.yacc_value = t("...")
            return :tDOT3
          end
          src.unread c
          self.yacc_value = t("..")
          return :tDOT2
        end
        src.unread c
        if c =~ /\d/ then
          rb_compile_error "no .<digit> floating literal anymore put 0 before dot"
        end
        self.lex_state = :expr_dot
        self.yacc_value = t(".")
        return :tDOT
      when /[0-9]/ then
        return parse_number(c)
      when ')' then # REFACTOR: omg this is lame... next 3 are all the same
        cond.lexpop
        cmdarg.lexpop
        self.lex_state = :expr_end
        self.yacc_value = t(")")
        return :tRPAREN
      when ']' then
        cond.lexpop
        cmdarg.lexpop
        self.lex_state = :expr_end
        self.yacc_value = t("]")
        return :tRBRACK
      when '}' then
        cond.lexpop
        cmdarg.lexpop
        self.lex_state = :expr_end
        self.yacc_value = t("end")
        return :tRCURLY
      when ':' then
        c = src.getch
        if c == ':' then
          if (lex_state == :expr_beg ||
              lex_state == :expr_mid ||
              lex_state == :expr_class ||
              (lex_state.is_argument && space_seen)) then
            self.lex_state = :expr_beg
            self.yacc_value = t("::")
            return :tCOLON3
          end

          self.lex_state = :expr_dot
          self.yacc_value = t(":")
          return :tCOLON2
        end

        if lex_state == :expr_end || lex_state == :expr_endarg || c =~ /\s/ then
          src.unread c
          self.lex_state = :expr_beg
          self.yacc_value = t(":")
          return ':'
        end

        case c
        when "\'" then
          self.lex_strterm = s(:strterm, STR_SSYM, c, "\0")
        when '"' then
          self.lex_strterm = s(:strterm, STR_DSYM, c, "\0")
        else
          src.unread c
        end

        self.lex_state = :expr_fname
        self.yacc_value = t(":")
        return :tSYMBEG
      when '/' then
        if lex_state == :expr_beg || lex_state == :expr_mid then
          self.lex_strterm = s(:strterm, STR_REGEXP, '/', "\0")
          self.yacc_value = t("/")
          return :tREGEXP_BEG
        end

        if (c = src.getch) == '=' then
          self.yacc_value = t("/")
          self.lex_state = :expr_beg
          return :tOP_ASGN
        end

        src.unread c

        if lex_state.is_argument && space_seen then
          unless c =~ /\s/ then
            arg_ambiguous
            self.lex_strterm = s(:strterm, STR_REGEXP, '/', "\0")
            self.yacc_value = t("/")
            return :tREGEXP_BEG
          end
        end

        self.lex_state = if lex_state == :expr_fname || lex_state == :expr_dot
                           :expr_arg
                         else
                           :expr_beg
                         end

        self.yacc_value = t("/")
        return :tDIVIDE
      when '^' then
        if (c = src.getch) == '=' then
          self.lex_state = :expr_beg
          self.yacc_value = t("^")
          return :tOP_ASGN
        end
        self.lex_state = if lex_state == :expr_fname || lex_state == :expr_dot
                           :expr_arg
                         else
                           :expr_beg
                         end
        src.unread c
        self.yacc_value = t("^")
        return :tCARET
      when ';' then
        self.command_start = true
        self.lex_state = :expr_beg
        self.yacc_value = t(";")
        return c
      when ',' then
        self.lex_state = :expr_beg
        self.yacc_value = t(",")
        return c
      when '~' then
        if lex_state == :expr_fname || lex_state == :expr_dot then
          if (c = src.getch) != '@' then
            src.unread c
          end
        end
        if lex_state == :expr_fname || lex_state == :expr_dot then
          self.lex_state = :expr_arg
        else
          self.lex_state = :expr_beg
        end
        self.yacc_value = t("~")
        return :tTILDE
      when '(' then
        c = :tLPAREN2
        self.command_start = true
        if lex_state == :expr_beg || lex_state == :expr_mid then
          c = :tLPAREN
        elsif space_seen then
          if lex_state == :expr_cmdarg then
            c = :tLPAREN_ARG
          elsif lex_state == :expr_arg then
            warning("don't put space before argument parentheses")
            c = :tLPAREN2
          end
        end
        cond.push false
        cmdarg.push false
        self.lex_state = :expr_beg
        self.yacc_value = t("(")
        return c
      when '[' then
        if lex_state == :expr_fname || lex_state == :expr_dot then
          self.lex_state = :expr_arg
          if (c = src.getch) == ']' then
            if src.check(/\=/) then
              c = src.getch
              self.yacc_value = t("[]=")
              return :tASET
            end
            self.yacc_value = t("[]")
            return :tAREF
          end
          rb_compile_error "unexpected '['"
        elsif lex_state == :expr_beg || lex_state == :expr_mid then
          c = :tLBRACK
        elsif lex_state.is_argument && space_seen then
          c = :tLBRACK
        end
        self.lex_state = :expr_beg
        cond.push false
        cmdarg.push false
        self.yacc_value = t("[")
        return c
      when '{' then
        c = :tLCURLY

        c = if lex_state.is_argument || lex_state == :expr_end then
              :tLCURLY      #  block (primary)
            elsif lex_state == :expr_endarg then
              :tLBRACE_ARG  #  block (expr)
            else
              :tLBRACE      #  hash
            end
        cond.push false
        cmdarg.push false
        self.lex_state = :expr_beg
        self.yacc_value = t("{")
        return c
      when "\\" then
        c = src.getch
        if c == "\n" then
          space_seen = true
          next #  skip \\n
        end
        rb_compile_error "bare backslash only allowed before newline"
      when '%' then
        if lex_state == :expr_beg || lex_state == :expr_mid then
          return parse_quote
        end

        c = src.getch
        if c == '=' then
          self.lex_state = :expr_beg
          self.yacc_value = t("%")
          return :tOP_ASGN
        end

        if lex_state.is_argument && space_seen && c !~ /\s/ then
          src.unread c
          return parse_quote
        end

        self.lex_state = case lex_state
                         when :expr_fname, :expr_dot then
                           :expr_arg
                         else
                           :expr_beg
                         end

        src.unread c
        self.yacc_value = t("%")

        return :tPERCENT
      when '$' then
        last_state = lex_state
        self.lex_state = :expr_end
        token_buffer.clear
        c = src.getch
        case c
        when '_' then #  $_: last read line string
          c = src.getch

          token_buffer << '$'
          token_buffer << '_'

          unless c =~ /\w/ then
            src.unread c
            self.yacc_value = t(token_buffer.join)
            return :tGVAR
          end
        when /[~*$?!@\/\\;,.=:<>\"]/ then
          token_buffer << '$'
          token_buffer << c
          self.yacc_value = t(token_buffer.join)
          return :tGVAR
        when '-' then
          token_buffer << '$'
          token_buffer << c
          c = src.getch
          if c =~ /\w/ then
            token_buffer << c
          else
            src.unread c
          end
          self.yacc_value = t(token_buffer.join)
          #  xxx shouldn't check if valid option variable
          return :tGVAR
        when /[\&\`\'\+]/ then
          # Explicit reference to these vars as symbols...
          if last_state == :expr_fname then
            token_buffer << '$'
            token_buffer << c
            self.yacc_value = t(token_buffer.join)
            return :tGVAR
          end

          self.yacc_value = s(:back_ref, c.to_sym)
          return :tBACK_REF
        when /[1-9]/ then
          token_buffer << '$'
          begin
            token_buffer << c
            c = src.getch
          end while c =~ /\d/
          src.unread c
          if last_state == :expr_fname then
            self.yacc_value = t(token_buffer.join)
            return :tGVAR
          else
            self.yacc_value = s(:nth_ref, token_buffer.join[1..-1].to_i)
            return :tNTH_REF
          end
        when '0' then
          token_buffer << '$'
        else
          unless c =~ /\w/ then
            src.unread c
            self.yacc_value = t("$")
            return '$'
          end
          token_buffer << '$'
        end
      when '@' then
        c = src.getch
        token_buffer.clear
        token_buffer << '@'
        if c == '@' then
          token_buffer << '@'
          c = src.getch
        end
        if c =~ /\d/ then
          if token_buffer.length == 1 then
            rb_compile_error "`@" + c + "' is not allowed as an instance variable name"
          else
            rb_compile_error "`@@" + c + "' is not allowed as a class variable name"
          end
        end
        unless c =~ /\w/ then
          src.unread c
          self.yacc_value = t("@")
          return '@'
        end
      when '_' then
        if src.was_begin_of_line && src.scan(/_END__(\n|\Z)/) then
          return RubyLexer::EOF
        end
        token_buffer.clear
      else
        unless c =~ /\w/ then
          rb_compile_error "Invalid char '#{c.inspect}' in expression"
        end
        token_buffer.clear
      end

      src.pos -= 1 # HACK
      if src.scan(/\w+/) then
        token_buffer.push(*src.matched.split(//)) # TODO: that split is tarded.
      end

      if token_buffer[0] =~ /\w/ && src.scan(/[\!\?](?!=)/) then
        token_buffer << src.matched
      end

      result = nil
      last_state = lex_state

      case token_buffer[0]
      when '$' then
        self.lex_state = :expr_end
        result = :tGVAR
      when '@' then
        self.lex_state = :expr_end
        if token_buffer[1] == '@' then
          result = :tCVAR
        else
          result = :tIVAR
        end
      else
        if token_buffer[-1] =~ /[!?]/ then
          result = :tFID
        else
          if lex_state == :expr_fname then
            # ident=, not =~ => == or followed by =>
            if src.scan(/=(?:(?![~>=])|(?==>))/) then
              result = :tIDENTIFIER
              token_buffer << src.matched
            end
          end

          if result.nil? && token_buffer[0] =~ /[A-Z]/ then
            result = :tCONSTANT
          else
            result = :tIDENTIFIER
          end
        end

        unless lex_state == :expr_dot then
          # See if it is a reserved word.
          keyword = Keyword.keyword(token_buffer.join, token_buffer.length)

          unless keyword.nil? then
            state = lex_state
            self.lex_state = keyword.state

            if state == :expr_fname then
              self.yacc_value = t(keyword.name)
            else
              self.yacc_value = t(token_buffer.join)
            end

            if keyword.id0 == :kDO then
              self.command_start = true
              if cond.is_in_state then
                return :kDO_COND
              end
              if cmdarg.is_in_state && state != :expr_cmdarg then
                return :kDO_BLOCK
              end
              if state == :expr_endarg then
                return :kDO_BLOCK
              end
              return :kDO
            end

            if state == :expr_beg then
              return keyword.id0
            end

            if keyword.id0 != keyword.id1 then
              self.lex_state = :expr_beg
            end

            return keyword.id1
          end
        end # lex_state == :expr_dot

        if (lex_state == :expr_beg ||
            lex_state == :expr_mid ||
            lex_state == :expr_dot ||
            lex_state == :expr_arg ||
            lex_state == :expr_cmdarg) then
          if command_state then
            self.lex_state = :expr_cmdarg
          else
            self.lex_state = :expr_arg
          end
        else
          self.lex_state = :expr_end
        end
      end

      # Lame: parsing logic made it into lexer in ruby...So we
      # are emulating
      # FIXME:  I believe this is much simpler now...
      # HACK
      # if (IdUtil.var_type(temp_val) == IdUtil.LOCAL_VAR &&
      #     last_state != :expr_dot &&
      #     (BlockStaticScope === scope && (scope.is_defined(temp_val) >= 0)) ||
      #     (scope.local_scope.is_defined(temp_val) >= 0)) then
      #   self.lex_state = :expr_end
      # end

      self.yacc_value = t(token_buffer.join)

      return result
    end
  end
end
