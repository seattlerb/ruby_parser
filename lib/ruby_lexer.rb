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

    c = src.read

    if (func & STR_FUNC_QWORDS) != 0 && c =~ /\s/ then
      begin
        c = src.read
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
      case c = src.read
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
      # HACK ruby_sourceline = nd_line(quote)
      raise "unterminated string meets end of file"
      return :tSTRING_END
    end

    self.yacc_value = s(:str, token_buffer.join)
    return :tSTRING_CONTENT
  end

  def regx_options
    options = []
    bad = []

    while c = src.read and c =~ /[a-z]/ do
      case c
      when /^[ixmonesu]$/ then
        options << c
      else
        bad << c
      end
    end

    src.unread c

    rb_compile_error("unknown regexp option%s - %s" %
                     [(bad.size > 1 ? "s" : ""), bad.join.inspect]) unless bad.empty?

    return options.join
  end

  def tokadd_escape term
    case c = src.read
    when "\n" then
      return false    # just ignore
    when /0-7/ then   # octal constant
      tokadd "\\"
      tokadd c

      2.times do |i|
        c = src.read
        # HACK goto eof if (c == -1)
        if c < "0" || "7" < c then
          pushback c
          break
        end
        tokadd c
      end

      return false
    when "x" then # hex constant
      tokadd "\\"
      tokadd c

      2.times do
        c = src.read
        unless c =~ /[0-9a-f]/i then # TODO error case? empty?
          src.unread c
          break
        end
        tokadd c
      end

      return false
    when "M" then
      if (c = src.read()) != "-" then
        yyerror "Invalid escape character syntax"
        pushback c
        return false
      end
      tokadd "\\"
      tokadd "M"
      tokadd "-"
      raise "not yet"
      # goto escaped;
    when "C" then
      if (c = src.read) != "-" then
        yyerror "Invalid escape character syntax"
        pushback c
        return false
      end
      tokadd "\\"
      tokadd "C"
      tokadd "-"
      raise "not yet"
      # HACK goto escaped;
    when "c" then
      tokadd "\\"
      tokadd "c"
      # HACK escaped:
      if (c = src.read) == "\\" then
        return tokadd_escape(term)
      elsif c == -1 then
        raise "no"
        # HACK goto eof
      end
      tokadd c
      return false
      # HACK eof
    when RubyLexer::EOF then
      yyerror "Invalid escape character syntax"
      return true
    else
      if (c != "\\" || c != term)
        tokadd "\\"
      end
      tokadd c
    end
    return false
  end

  def read_escape
    case c = src.read
    when "\\" then # Backslash
      return c
    when "n" then # newline
      return "\n"
    when "t" then # horizontal tab
      return "\t"
    when "r" then # carriage-return
      return "\r"
    when "f" then # form-feed
      return "\f"
    when "v" then # vertical tab
      return "\13"
    when "a" then # alarm(bell)
      return "\007"
    when 'e' then # escape
      return "\033"
    when /[0-7]/ then # octal constant
      src.unread c # TODO this seems dumb

      n = 0

      3.times do
        c = src.read
        unless c =~ /[0-7]/ then
          src.unread c
          break
        end
        n <<= 3
        n |= c[0] - ?0
      end

      return n.chr
    when "x" then # hex constant
      n = 0

      2.times do
        c = src.read.downcase
        unless c =~ /[0-9a-f]/i then
          src.unread c
          break
        end
        n <<= 4
        n |= case c[0] # TODO: I'm sure there is a better way... but I'm tired
             when ?a..?f then
               c[0] - ?a + 10
             when ?A..?F then
               c[0] - ?A + 10
             when ?0..?9 then
               c[0] - ?0
             else
               raise "wtf?: #{c.inspect}"
             end
      end

      return n.chr
    when "b" then # backspace
      return "\010"
    when "s" then # space
      return " "
    when "M" then
      c = src.read
      if c  != "-" then
        yyerror("Invalid escape character syntax")
        src.unread c
        return "\0"
      end

      c = src.read
      case c
      when "\\" then
        c = self.read_escape
        c[0] |= 0x80
        return c
      when RubyLexer::EOF then
        yyerror("Invalid escape character syntax");
        return '\0';
      else
        c[0] |= 0x80
        return c
      end
    when "C", "c" then
      if (c = src.read) != "-" then
        yyerror("Invalid escape character syntax")
        pushback(c)
        return "\0"
      end if c == "C"

      case c = src.read
      when "\\" then
        c = read_escape
      when "?" then
        return 0177
      when RubyLexer::EOF then
        yyerror("Invalid escape character syntax");
        return "\0";
      end
      c[0] &= 0x9f
      return c
    when RubyLexer::EOF then
      yyerror("Invalid escape character syntax")
      return "\0"
    else
      return c
    end
  end

  def tokadd_string(func, term, paren, buffer)
    until (c = src.read) == RubyLexer::EOF do
      if c == paren then
        self.nest += 1
      elsif c == term then
        if self.nest == 0 then
          src.unread c
          break
        end
        self.nest -= 1
      elsif (func & RubyLexer::STR_FUNC_EXPAND) != 0 && c == '#' && !src.peek(/\n/) then
        c2 = src.read

        if c2 == '$' || c2 == '@' || c2 == '{' then
          src.unread c2
          src.unread c
          break
        end
        src.unread(c2)
      elsif c == "\\" then
        c = src.read
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
        #         else if (ismbchar(c)) {
        #             int i, len = mbclen(c)-1;
        #             for (i = 0; i < len; i++) {
        #                 tokadd(c);
        #                 c = nextc();
        #             }
        #         }
      elsif (func & RubyLexer::STR_FUNC_QWORDS) != 0 && c =~ /\s/ then
        src.unread c
        break
      end

      if c == "\0" && (func & RubyLexer::STR_FUNC_SYMBOL) != 0 then
        raise SyntaxError, "symbol cannot contain '\\0'"
      end

      buffer << c # unless c == "\r"
    end # while

    return c
  end

  def heredoc here
    _, eos, func, last_line = here

    str = []
    indent = (func & RubyLexer::STR_FUNC_INDENT) != 0
    re = if indent then
           /[ \t]*#{eos}(\r?\n|\z)/
         else
           /#{eos}(\r?\n|\z)/
         end

    err_msg = "can't match #{re.inspect} anywhere in "

    raise SyntaxError, err_msg if src.eos?

    if src.begin_of_line? && src.scan(re) then
      src.unread_many last_line
      self.yacc_value = t(eos)
      return :tSTRING_END
    end

    if (func & RubyLexer::STR_FUNC_EXPAND) == 0 then
      until src.scan(re) do
        str << src.read_line
        raise SyntaxError, err_msg + src.rest.inspect if src.eos?
      end
    else
      c = src.read
      buffer = []

      if c == "#" then
        case c = src.read
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

        raise SyntaxError, err_msg + src.rest.inspect if c == RubyLexer::EOF

        if c != "\n" then
          self.yacc_value = s(:str, buffer.join.delete("\r"))
          return :tSTRING_CONTENT
        end

        buffer << src.read

        raise SyntaxError, err_msg + src.rest.inspect if src.eos?
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
      beg = src.read
      if beg =~ /[a-z0-9]/i then
        raise SyntaxError, "unknown type of %string"
      end
    end

    if c == RubyLexer::EOF or beg == RubyLexer::EOF then
      raise SyntaxError, "unterminated quoted string meets nnd of file"
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
      begin c = src.read end while c =~ /\s/
      src.unread(c)
    when 'w' then
      string_type, token_type = STR_SQUOTE | STR_FUNC_QWORDS, :tQWORDS_BEG
      begin c = src.read end while c =~ /\s/
      src.unread(c)
    when 'x' then
      string_type, token_type = STR_XQUOTE, :tXSTRING_BEG
    when 'r' then
      string_type, token_type = STR_REGEXP, :tREGEXP_BEG
    when 's' then
      string_type, token_type = STR_SSYM, :tSYMBEG
      self.lex_state = :expr_fname
    else
      raise SyntaxError, "Unknown type of %string. Expected 'Q', 'q', 'w', 'x', 'r' or any non letter character, but found '" + c + "'."
    end

    self.lex_strterm = s(:strterm, string_type, nnd, beg)

    return token_type
  end

  def heredoc_identifier
    c = src.read
    term = 42 # HACK
    func = 0

    if c == '-' then
      c = src.read
      func = STR_FUNC_INDENT
    end

    if c =~ /^['"`]$/ then
      if c == "\'" then
        func |= STR_SQUOTE
      elsif c == '"'
        func |= STR_DQUOTE
      else
        func |= STR_XQUOTE
      end

      token_buffer.clear
      term = c

      while (c = src.read) != RubyLexer::EOF && c != term
        token_buffer << c
      end

      if c == RubyLexer::EOF then
        raise SyntaxError, "unterminated here document identifier"
      end
    else
      unless c =~ /\w/ then
        src.unread c
        src.unread '-' if (func & STR_FUNC_INDENT) != 0
        return 0 # TODO: false? RubyLexer::EOF?
      end
      token_buffer.clear
      term = '"'
      func |= STR_DQUOTE
      begin
        token_buffer << c
      end while (c = src.read) != RubyLexer::EOF && c =~ /\w/
      src.unread c
    end

    line = src.read_line
    tok = token_buffer.join
    self.lex_strterm = s(:heredoc, tok, func, line)

    if term == '`' then
      self.yacc_value = t("`")
      return :tXSTRING_BEG
    end

    self.yacc_value = t("\"")
    return :tSTRING_BEG
  end

  def arg_ambiguous
    self.warning("Ambiguous first argument. make sure.")
  end

  ##
  # Read a comment up to end of line.  When found each comment will
  # get stored away into the parser result so that any interested
  # party can use them as they seem fit.  One idea is that IDE authors
  # can do distance based heuristics to associate these comments to
  # the AST node they think they belong to.
  #
  # @param c last character read from lexer source
  # @return newline or eof value

  def read_comment c
    token_buffer.clear
    token_buffer << c

    while (c = src.read) != "\n" do
      break if c == RubyLexer::EOF
      token_buffer << c
    end
    src.unread c

    # Store away each comment to parser result so IDEs can do whatever
    # they want with them.
    # HACK parser_support.result.add_comment(Node.comment(token_buffer.join))

    return c
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
      c = src.read
      case c
      when /\004|\032|\000/, RubyLexer::EOF then # ^D, ^Z, EOF
        return RubyLexer::EOF
      when /\ |\t|\f|\r|\13/ then # white spaces, 13 = '\v
        space_seen = true
        next
      when /#|\n/ then
        return 0 if c == '#' and read_comment(c) == 0 # FIX 0?
        # Replace a string of newlines with a single one
        while (c = src.read) == "\n"
          # do nothing
        end

        src.unread c

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
        c = src.read
        if c == '*' then
          c = src.read
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
        if (c = src.read) == '=' then
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
          self.token_buffer << "begin"
          c = src.read

          if c =~ /\s/ then
            # In case last next was the newline.
            src.unread(c)

            loop do
              c = src.read
              token_buffer << c

              # If a line is followed by a blank line put it back.
              while c == "\n"
                c = src.read
                token_buffer << c
              end

              if c == RubyLexer::EOF then
                raise SyntaxError, "embedded document meets end of file"
              end

              next unless c == '='

              if src.was_begin_of_line && src.scan(/end/) then
                token_buffer << "end"
                token_buffer << src.read_line
                src.unread "\n"
                break
              end
            end

            # parser_support.result.add_comment(Node.comment(token_buffer.join))
            next
          end
          src.unread(c)
        end


        if lex_state == :expr_fname || lex_state == :expr_dot then
          self.lex_state = :expr_arg
        else
          self.lex_state = :expr_beg
        end

        c = src.read
        if c == '=' then
          c = src.read
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
        c = src.read
        if (c == '<' &&
            lex_state != :expr_end &&
            lex_state != :expr_dot &&
            lex_state != :expr_endarg &&
            lex_state != :expr_class &&
            (!lex_state.is_argument || space_seen)) then
          tok = self.heredoc_identifier
          return tok unless tok == 0
        end
        if lex_state == :expr_fname || lex_state == :expr_dot then
          self.lex_state = :expr_arg
        else
          self.lex_state = :expr_beg
        end
        if c == '=' then
          if (c = src.read) == '>' then
            self.yacc_value = t("<=>")
            return :tCMP
          end
          src.unread c
          self.yacc_value = t("<=")
          return :tLEQ
        end
        if c == '<' then
          if (c = src.read) == '=' then
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

        if (c = src.read) == '=' then
          self.yacc_value = t(">=")
          return :tGEQ
        end
        if c == '>' then
          if (c = src.read) == '=' then
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

        c = src.read

        raise SyntaxError, "incomplete character syntax" if c == RubyLexer::EOF

        if c =~ /\s/ then
          unless lex_state.is_argument then
            c2 = case c
                 when ' ' then
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
                 else
                   0
                 end

            if c2 != 0 then
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
        elsif c =~ /\w/ && ! src.peek(/\n/) && self.is_next_identchar then
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
        if (c = src.read) == '&' then
          self.lex_state = :expr_beg
          if (c = src.read) == '=' then
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
        if (c = src.read) == '|' then
          self.lex_state = :expr_beg
          if (c = src.read) == '=' then
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
        c = src.read
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
          arg_ambiguous if lex_state.is_argument
          self.lex_state = :expr_beg
          src.unread c
          if c =~ /\d/ then
            c = '+'
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
        c = src.read
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
          arg_ambiguous if lex_state.is_argument
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
        if (c = src.read) == '.' then
          if (c = src.read) == '.' then
            self.yacc_value = t("...")
            return :tDOT3
          end
          src.unread c
          self.yacc_value = t("..")
          return :tDOT2
        end
        src.unread c
        if c =~ /\d/ then
          raise SyntaxError, "no .<digit> floating literal anymore put 0 before dot"
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
        c = src.read
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

        if (c = src.read) == '=' then
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

        self.lex_state = if (lex_state == :expr_fname ||
                             lex_state == :expr_dot) then
                           :expr_arg
                         else
                           :expr_beg
                         end

        self.yacc_value = t("/")
        return :tDIVIDE
      when '^' then
        if (c = src.read) == '=' then
          self.lex_state = :expr_beg
          self.yacc_value = t("^")
          return :tOP_ASGN
        end
        if lex_state == :expr_fname || self.lex_state == :expr_dot then
          self.lex_state = :expr_arg
        else
          self.lex_state = :expr_beg
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
          if (c = src.read) != '@' then
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
          if (c = src.read) == ']' then
            if src.peek(/=/) then
              c = src.read
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
        c = src.read
        if c == "\n" then
          space_seen = true
          next #  skip \\n
        end
        src.unread c
        self.yacc_value = t("\\")
        return "\\"
      when '%' then
        if lex_state == :expr_beg || lex_state == :expr_mid then
          return parse_quote(src.read)
        end

        c = src.read
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
        c = src.read
        case c
        when '_' then #  $_: last read line string
          c = src.read

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
          c = src.read
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
            c = src.read
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
        c = src.read
        token_buffer.clear
        token_buffer << '@'
        if c == '@' then
          token_buffer << '@'
          c = src.read
        end
        if c =~ /\d/ then
          if token_buffer.length == 1 then
            raise SyntaxError, "`@" + c + "' is not allowed as an instance variable name"
          else
            raise SyntaxError, "`@@" + c + "' is not allowed as a class variable name"
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
          raise SyntaxError, "Invalid char '#{c.inspect}' in expression"
        end
        token_buffer.clear
      end

      begin
        token_buffer << c
        # if ismbchar(c) then
        #   len = mbclen(c) - 1
        #   (0..len).each do
        #     c = src.read;
        #     token_buffer << c
        #   end
        # end
        c = src.read
      end while c =~ /\w/

      if c =~ /\!|\?/ && token_buffer[0] =~ /\w/ && src.peek(/./) != '=' then
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
            if (c = src.read) == '=' then
              c2 = src.read

              if c2 != '~' && c2 != '>' && (c2 != '=' || (c2 == "\n" && src.peek('>'))) then
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

  ##
  #  Parse a number from the input stream.
  #
  # @param c The first character of the number.
  # @return A int constant wich represents a token.

  def parse_number c
    self.lex_state = :expr_end

    token_buffer.clear

    if c == '-' then
      token_buffer << c
      c = src.read
    elsif c == '+' then
      # We don't append '+' since Java number parser gets confused FIX
      c = src.read
    end

    nondigit = "\0"

    if c == '0' then
      start_len = token_buffer.length
      c = src.read

      case c
      when /x/i then # hexadecimal
        c = src.read

        if c =~ /[a-f0-9]/i then
          loop do
            if c == '_' then
              break unless nondigit == "\0"
              nondigit = c
            elsif c =~ /[a-f0-9]/i then
              nondigit = "\0"
              token_buffer << c
            else
              break
            end
            c = src.read
          end
        end

        src.unread c

        if token_buffer.length == start_len then
          raise SyntaxError, "Hexadecimal number without hex-digits."
        elsif nondigit != "\0" then
          raise SyntaxError, "Trailing '_' in number."
        end
        self.yacc_value = token_buffer.join.to_i(16)
        return :tINTEGER
      when /b/i # binary
        c = src.read
        if c == '0' or c == '1' then
          loop do
            if c == '_' then
              break if nondigit != "\0"
              nondigit = c
            elsif c == '0' or c == '1' then
              nondigit = "\0"
              token_buffer << c
            else
              break
            end
            c = src.read
          end
        end

        src.unread c

        if token_buffer.length == start_len then
          raise SyntaxError, "Binary number without digits."
        elsif nondigit != "\0" then
          raise SyntaxError, "Trailing '_' in number."
        end
        self.yacc_value = token_buffer.join.to_i(2)
        return :tINTEGER
      when /d/i then # decimal
        c = src.read
        if c =~ /\d/ then
          loop do
            if c == '_' then
              break if nondigit != "\0"
              nondigit = c
            elsif c =~ /\d/ then
              nondigit = "\0"
              token_buffer << c
            else
              break
            end
            c = src.read
          end
        end

        src.unread c

        if token_buffer.length == start_len then
          raise SyntaxError, "Binary number without digits."
        elsif nondigit != "\0" then
          raise SyntaxError, "Trailing '_' in number."
        end

        self.yacc_value = token_buffer.join.to_i(10)
        return :tINTEGER
      when /o/i, /[0-7_]/ then # octal
        c = src.read if c =~ /o/i # prefixed octal - kill me
        loop do
          if c == '_' then
            break if (nondigit != "\0")
            nondigit = c
          elsif c && c >= '0' && c <= '7' then
            nondigit = "\0"
            token_buffer << c
          else
            break
          end
          c = src.read
        end
        if token_buffer.length > start_len then
          src.unread c

          if nondigit != "\0" then
            raise SyntaxError, "Trailing '_' in number."
          end

          self.yacc_value = token_buffer.join.to_i(8)
          return :tINTEGER
        end
      when /[89]/ then
        raise SyntaxError, "Illegal octal digit."
      when /[\.eE]/ then
        token_buffer << '0'
      else
        src.unread c
        self.yacc_value = 0
        return :tINTEGER
      end
    end

    seen_point = false
    seen_e = false

    loop do
      case c
      when /\d/ then
        nondigit = "\0"
        token_buffer << c
      when '.' then
        if nondigit != "\0" then
          src.unread c
          raise SyntaxError, "Trailing '_' in number."
        elsif seen_point or seen_e then
          src.unread c
          return number_token(token_buffer.join, true, nondigit)
        else
          c2 = src.read
          unless c2 =~ /\d/ then
            src.unread c2
            src.unread '.'
            if c == '_' then
              # Enebo:  c can never be antrhign but '.'
              # Why did I put this here?
            else
              self.yacc_value = token_buffer.join.to_i(10)
              return :tINTEGER
            end
          else
            token_buffer << '.'
            token_buffer << c2
            seen_point = true
            nondigit = "\0"
          end
        end
      when /e/i then
        if nondigit != "\0" then
          raise SyntaxError, "Trailing '_' in number."
        elsif seen_e then
          src.unread c
          return number_token(token_buffer.join, true, nondigit)
        else
          token_buffer << c
          seen_e = true
          nondigit = c
          c = src.read
          if c == '-' or c == '+' then
            token_buffer << c
            nondigit = c
          else
            src.unread c
          end
        end
      when '_' then #  '_' in number just ignored
        if nondigit != "\0" then
          raise SyntaxError, "Trailing '_' in number."
        end
        nondigit = c
      else
        src.unread c
        r = number_token(token_buffer.join, seen_e || seen_point, nondigit)
        return r
      end
      c = src.read
    end
  end

  # TODO: remove me
  def number_token(number, is_float, nondigit)
    if nondigit != "\0" then
      raise SyntaxError, "Trailing '_' in number."
    end

    if is_float then
      self.yacc_value = number.to_f
      return :tFLOAT
    end

    self.yacc_value = number.to_i
    return :tINTEGER
  end

  ############################################################
  # HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK

  def tokadd s # HACK
    self.token_buffer << s
  end

  def warning s
    # do nothing for now
  end

  def rb_compile_error msg
    raise msg
  end

  def is_next_identchar # TODO: ?
    c = src.read
    src.unread c

    return c != RubyLexer::EOF && c =~ /\w/
  end

  def is_next_no_case(s) # FIX: replace this whole thing with something clean
    buf = []
    old_pos = src.pos

    s.each_byte do |b|
      c = b.chr
      r = src.read
      buf << r

      if c.downcase != r.downcase then
        src.pos = old_pos
        return nil
      end
    end

    return buf.join
  end

  # END HACK
  ############################################################$

end
