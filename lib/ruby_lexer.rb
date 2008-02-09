$: << File.expand_path("~/Work/p4/zss/src/ParseTree/dev/lib") # for me, not you.
require 'sexp'
require 'ruby_parser_extras'

class StringScanner
  def lineno
    string[0..pos].split(/\n/).size
  end

  def current_line # HAHA fuck you (HACK)
    string[0..pos][/\A.*__LINE__/m].split(/\n/).size
  end

  if ENV['TALLY'] then
    alias :old_getch :getch
    def getch
      warn({:getch => caller[0]}.inspect)
      old_getch
    end
  end

  def unread c
    return if c.nil? # UGH
    warn({:unread => caller[0]}.inspect) if ENV['TALLY']
    string[pos, 0] = c
  end

  def unread_many str
    warn({:unread_many => caller[0]}.inspect) if ENV['TALLY']
    string[pos, 0] = str
  end

  def was_begin_of_line
    pos <= 2 or string[pos-2] == ?\n
  end
end

class RubyLexer
  attr_accessor :command_start
  attr_accessor :cmdarg
  attr_accessor :cond
  attr_accessor :nest

  # Additional context surrounding tokens that both the lexer and
  # grammar use.
  attr_reader :lex_state

  def lex_state= o
    raise "wtf?" unless Symbol === o
    @lex_state = o
  end

  attr_accessor :lex_strterm

  # Stream of data that yylex examines.
  attr_reader :src

  def src= src
    raise "bad src: #{src.inspect}" unless String === src
    @src = StringScanner.new src
  end

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
  STR_FUNC_ESCAPE=0x01
  STR_FUNC_EXPAND=0x02
  STR_FUNC_REGEXP=0x04
  STR_FUNC_QWORDS=0x08
  STR_FUNC_SYMBOL=0x10
  STR_FUNC_INDENT=0x20 # <<-HEREDOC

  STR_SQUOTE = 0
  STR_DQUOTE = STR_FUNC_EXPAND
  STR_XQUOTE = STR_FUNC_EXPAND
  STR_REGEXP = STR_FUNC_REGEXP | STR_FUNC_ESCAPE | STR_FUNC_EXPAND
  STR_SSYM   = STR_FUNC_SYMBOL
  STR_DSYM   = STR_FUNC_SYMBOL | STR_FUNC_EXPAND

  def initialize
    self.token_buffer = []
    self.cond = StackState.new(:cond)
    self.cmdarg = StackState.new(:cmdarg)
    self.nest = 0
    @comments = []

    reset
  end

  def reset
    self.command_start = true
    self.lex_strterm = nil
    self.token = nil
    self.yacc_value = nil

    @src = nil
    @lex_state = nil
  end

  # How the parser advances to the next token.
  #
  # @return true if not at end of file (EOF).

  def advance
    r = yylex
    self.token = r
    return r != RubyLexer::EOF
  end

  def parse_string(quote)
    _, string_type, term, open = quote

    space = false # FIX: remove these
    func = string_type
    paren = open

    return :tSTRING_END unless func

    c = src.getch

    if (func & STR_FUNC_QWORDS) != 0 && c =~ /\s/ then
      begin
        c = src.getch
        break if c == RubyLexer::EOF # HACK UGH
      end while String === c and c =~ /\s/
      space = true
    end

    if c == term && self.nest == 0 then
      if func & STR_FUNC_QWORDS != 0 then
        quote[1] = nil
        return ' '
      end
      unless func & STR_FUNC_REGEXP != 0 then
        self.yacc_value = t(term)
        return :tSTRING_END
      end
      self.yacc_value = self.regx_options
      return :tREGEXP_END
    end

    if space then
      src.unread c
      return ' '
    end

    self.token_buffer = []

    if (func & STR_FUNC_EXPAND) != 0 && c == '#' then
      case c = src.getch
      when '$', '@' then
        src.unread c
        return :tSTRING_DVAR
      when '{' then
        return :tSTRING_DBEG
      end
      token_buffer << '#'
    end

    src.unread c

    if tokadd_string(func, term, paren, token_buffer) == RubyLexer::EOF then
      rb_compile_error "unterminated string meets end of file"
    end

    self.yacc_value = s(:str, token_buffer.join)
    return :tSTRING_CONTENT
  end

  def regx_options
    options = []
    bad = []

    while c = src.getch and c =~ /[a-z]/ do
      case c
      when /^[ixmonesu]$/ then
        options << c
      else
        bad << c
      end
    end

    src.unread c

    unless bad.empty? then
      rb_compile_error("unknown regexp option%s - %s" %
                       [(bad.size > 1 ? "s" : ""), bad.join.inspect])
    end

    return options.join
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

  def read_escape
    case
    when src.scan(/\\/) then                   # Backslash
      return '\\'
    when src.scan(/n/) then                    # newline
      return "\n"
    when src.scan(/t/) then                    # horizontal tab
      return "\t"
    when src.scan(/r/) then                    # carriage-return
      return "\r"
    when src.scan(/f/) then                    # form-feed
      return "\f"
    when src.scan(/v/) then                    # vertical tab
      return "\13"
    when src.scan(/a/) then                    # alarm(bell)
      return "\007"
    when src.scan(/e/) then                    # escape
      return "\033"
    when src.scan(/b/) then                    # backspace
      return "\010"
    when src.scan(/s/) then                    # space
      return " "
    when src.scan(/[0-7]{1,3}/) then             # octal constant
      return src.matched.to_i(8).chr
    when src.scan(/x([0-9a-fA-Fa-f]{2})/) then # hex constant
      return src[1].to_i(16).chr
    when src.scan(/M-\\/) then
      c = self.read_escape
      c[0] |= 0x80
      return c
    when src.scan(/M-(.)/) then
      c = src[1]
      c[0] |= 0x80
      return c
    when src.scan(/C-\\|c\\/) then
      c = self.read_escape
      c[0] &= 0x9f
      return c
    when src.scan(/C-\?|c\?/) then
      return 0177.chr
    when src.scan(/(C-|c)(.)/) then
      c = src[2]
      c[0] &= 0x9f
      return c
    when src.scan(/[McCx0-9]/) || src.eos? then
      rb_compile_error("Invalid escape character syntax")
    else
      return src.getch
    end
  end

  def tokadd_string(func, term, paren, buffer)
    should_expand = (func & RubyLexer::STR_FUNC_EXPAND) != 0
    until (c = src.getch) == RubyLexer::EOF do
      if c == paren then
        self.nest += 1
      elsif c == term then
        if self.nest == 0 then
          src.unread c
          break
        end
        self.nest -= 1
      elsif should_expand && c == '#' && !src.check(/\n/) then
        c2 = src.getch

        if c2 == '$' || c2 == '@' || c2 == '{' then
          src.unread c2
          src.unread c
          break
        end
        src.unread(c2)
      elsif c == "\\" then
        c = src.getch
        case c
        when "\n" then
          break if ((func & RubyLexer::STR_FUNC_QWORDS) != 0) # TODO: check break
          next  if ((func & RubyLexer::STR_FUNC_EXPAND) != 0)

          buffer << "\\"
        when "\\" then
          buffer << c if (func & RubyLexer::STR_FUNC_ESCAPE) != 0
        else
          if (func & RubyLexer::STR_FUNC_REGEXP) != 0 then
            src.unread c
            tokadd_escape term
            next
          elsif (func & RubyLexer::STR_FUNC_EXPAND) != 0 then
            src.unread c
            if (func & RubyLexer::STR_FUNC_ESCAPE) != 0 then
              buffer << "\\"
            end
            c = read_escape
          elsif (func & RubyLexer::STR_FUNC_QWORDS) != 0 && c =~ /\s/ then
            # ignore backslashed spaces in %w
          elsif c != term && !(paren && c == paren) then
            buffer << "\\"
          end
        end
        # else if (ismbchar(c)) {
        #   int i, len = mbclen(c)-1;
        #   for (i = 0; i < len; i++) {
        #     tokadd(c);
        #     c = nextc();
        #   }
        # }
      elsif (func & RubyLexer::STR_FUNC_QWORDS) != 0 && c =~ /\s/ then
        src.unread c
        break
      end

      if c == "\0" && (func & RubyLexer::STR_FUNC_SYMBOL) != 0 then
        rb_compile_error "symbol cannot contain '\\0'"
      end

      buffer << c # unless c == "\r"
    end # while

    return c
  end

  def heredoc here
    _, eos, func, last_line = here

    raise "empty heredoc token" if eos.empty?

    str     = []
    indent  = (func & RubyLexer::STR_FUNC_INDENT) != 0
    re      = indent ? /[ \t]*#{eos}(\r?\n|\z)/ : /#{eos}(\r?\n|\z)/
    err_msg = "can't match #{re.inspect} anywhere in "

    rb_compile_error err_msg if src.eos?

    if src.beginning_of_line? && src.scan(re) then
      src.unread_many last_line
      self.yacc_value = t(eos)
      return :tSTRING_END
    end

    if (func & RubyLexer::STR_FUNC_EXPAND) == 0 then
      until src.scan(re) do
        str << src.scan(/.*\n/)
        rb_compile_error err_msg if src.eos?
      end
    else
      c = src.getch
      buffer = []

      if c == "#" then
        c = src.getch
        case c
        when "$", "@" then
          src.unread c
          self.yacc_value = t("#" + c)
          return :tSTRING_DVAR
        when "{" then
          self.yacc_value = t("#" + c)
          return :tSTRING_DBEG
        end
        buffer << "#"
      end

      src.unread c

      until src.scan(re) do
        c = tokadd_string func, "\n", nil, buffer

        rb_compile_error err_msg if c == RubyLexer::EOF

        if c != "\n" then
          self.yacc_value = s(:str, buffer.join.delete("\r"))
          return :tSTRING_CONTENT
        end

        buffer << src.getch

        rb_compile_error err_msg if src.eos?
      end

      str = buffer
    end

    src.unread_many(eos + "\n")

    self.lex_strterm = s(:heredoc, eos, func, last_line)
    self.yacc_value = s(:str, str.join.delete("\r"))

    return :tSTRING_CONTENT
  end

  def parse_quote(c)
    beg, nnd = nil, nil
    short_hand = false

    # Short-hand (e.g. %{,%.,%!,... versus %Q{).
    unless c =~ /[a-z0-9]/i then
      beg, c = c, 'Q'
      short_hand = true
    else # Long-hand (e.g. %Q{}).
      short_hand = false
      beg = src.getch
      if beg =~ /[a-z0-9]/i then
        rb_compile_error "unknown type of %string"
      end
    end

    if c == RubyLexer::EOF or beg == RubyLexer::EOF then
      rb_compile_error "unterminated quoted string meets nnd of file"
    end

    # Figure nnd-char.  "\0" is special to indicate beg=nnd and that no nesting?
    nnd = case beg
          when '(' then
            ')'
          when '[' then
            ']'
          when '{' then
            '}'
          when '<' then
            '>'
          else
            nnd, beg = beg, "\0"
            nnd
          end

    string_type, token_type = STR_DQUOTE, :tSTRING_BEG
    self.yacc_value = t("%#{c}#{beg}")

    case (c)
    when 'Q' then
      self.yacc_value = t("%#{short_hand ? nnd : c + beg}")
    when 'q' then
      string_type, token_type = STR_SQUOTE, :tSTRING_BEG
    when 'W' then
      string_type, token_type = STR_DQUOTE | STR_FUNC_QWORDS, :tWORDS_BEG
      src.scan(/\s*/)
    when 'w' then
      string_type, token_type = STR_SQUOTE | STR_FUNC_QWORDS, :tQWORDS_BEG
      src.scan(/\s*/)
    when 'x' then
      string_type, token_type = STR_XQUOTE, :tXSTRING_BEG
    when 'r' then
      string_type, token_type = STR_REGEXP, :tREGEXP_BEG
    when 's' then
      string_type, token_type = STR_SSYM, :tSYMBEG
      self.lex_state = :expr_fname
    else
      rb_compile_error "Unknown type of %string. Expected 'Q', 'q', 'w', 'x', 'r' or any non letter character, but found '" + c + "'."
    end

    self.lex_strterm = s(:strterm, string_type, nnd, beg)

    return token_type
  end

  def heredoc_identifier
    term, func = nil, 0
    token_buffer.clear

    case
    when src.scan(/(-?)(['"`])(.*?)\2/) then
      term = src[2]
      func |= STR_FUNC_INDENT unless src[1].empty?
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
      func |= STR_FUNC_INDENT unless src[1].empty?
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

  def arg_ambiguous
    self.warning("Ambiguous first argument. make sure.")
  end

  def store_comment
    @comments.push(*self.token_buffer)
    self.token_buffer.clear
  end

  def comments
    c = @comments.join
    @comments.clear
    c
  end

  ##
  # Returns the next token. Also sets yy_val is needed.
  #
  # @return Description of the Returned Value
  # TODO: remove ALL sexps coming from here and move up to grammar
  # TODO: only literal values should come up from the lexer.

  def yylex
    c = ''
    space_seen = false
    command_state = false

    if lex_strterm then
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

          while src.scan(/\s*#.*\n+/) do
            token_buffer << src.matched.gsub(/^ +#/, '#').gsub(/^ +$/, '')
          end

          self.store_comment

          return RubyLexer::EOF if src.eos?
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
        if src.was_begin_of_line and src.scan(/begin/) then
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

              next unless c == '='

              if src.was_begin_of_line && src.scan(/end/) then
                token_buffer << "end"
                token_buffer << src.scan(/.*\n/)
                src.unread "\n"
                break
              end
            end

            self.store_comment

            next
          end
          src.unread(c)
        end


        if lex_state == :expr_fname || lex_state == :expr_dot then
          self.lex_state = :expr_arg
        else
          self.lex_state = :expr_beg
        end

        c = src.getch
        if c == '=' then
          c = src.getch
          if c == '=' then
            self.yacc_value = t("===")
            return :tEQQ
          end
          src.unread(c)
          self.yacc_value = t("==")
          return :tEQ
        end
        if c == '~' then
          self.yacc_value = t("=~")
          return :tMATCH
        elsif c == '>' then
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
          return tok if tok
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
        if lex_state == :expr_fname then
          self.lex_state = :expr_end
          return :tBACK_REF2
        end
        if lex_state == :expr_dot then
          if command_state then
            self.lex_state = :expr_cmdarg
          else
            self.lex_state = :expr_arg
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

        rb_compile_error "incomplete character syntax" if c == RubyLexer::EOF

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
            if src.check(/=/) then
              c = src.getch
              self.yacc_value = t("[]=")
              return :tASET
            end
            self.yacc_value = t("[]")
            return :tAREF
          end
          src.unread c
          self.yacc_value = t("[")
          return '['
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

        if lex_state.is_argument || lex_state == :expr_end then
          c = :tLCURLY      #  block (primary)
        elsif lex_state == :expr_endarg then
          c = :tLBRACE_ARG  #  block (expr)
        else
          c = :tLBRACE      #  hash
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
        src.unread c
        self.yacc_value = t("\\")
        return "\\"
      when '%' then
        if lex_state == :expr_beg || lex_state == :expr_mid then
          return parse_quote(src.getch)
        end

        c = src.getch
        if c == '=' then
          self.lex_state = :expr_beg
          self.yacc_value = t("%")
          return :tOP_ASGN
        end

        return parse_quote(c) if lex_state.is_argument && space_seen && c !~ /\s/

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

      c = src.getch # HACK
      if c =~ /\!|\?/ && token_buffer[0] =~ /\w/ && ! src.check(/=/) then
        token_buffer << c
      else
        src.unread c
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
            c = src.getch
            if c == '=' then
              c2 = src.getch

              if c2 != '~' && c2 != '>' && (c2 != '=' || (c2 == "\n" && src.check(/>/))) then
                result = :tIDENTIFIER
                token_buffer << c
                src.unread c2
              else
                src.unread c2
                src.unread c
              end
            else
              src.unread c
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
              return :kDO_COND  if cond.is_in_state
              return :kDO_BLOCK if cmdarg.is_in_state && state != :expr_cmdarg
              return :kDO_BLOCK if state == :expr_endarg
              return :kDO
            end

            return keyword.id0 if state == :expr_beg

            self.lex_state = :expr_beg unless keyword.id0 == keyword.id1

            return keyword.id1
          end
        end

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


      temp_val = token_buffer.join

      # Lame: parsing logic made it into lexer in ruby...So we
      # are emulating
      # FIXME:  I believe this is much simpler now...
# HACK
#       if (IdUtil.var_type(temp_val) == IdUtil.LOCAL_VAR &&
#           last_state != :expr_dot &&
#           (BlockStaticScope === scope && (scope.is_defined(temp_val) >= 0)) ||
#           (scope.local_scope.is_defined(temp_val) >= 0)) then
#         self.lex_state = :expr_end
#       end

      self.yacc_value = t(temp_val)

      return result
    end
  end

  def int_with_base base
    rb_compile_error "Invalid numeric format" if src.matched =~ /__/
    self.yacc_value = src.matched.to_i(base)
    return :tINTEGER
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
      rb_compile_error "Invalid numeric format" if number =~ /__/
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

  def warning s
    # do nothing for now
  end

  def rb_compile_error msg
    msg += ". near line #{src.lineno}: #{src.rest[/^.*/].inspect}"
    raise SyntaxError, msg
  end
end
