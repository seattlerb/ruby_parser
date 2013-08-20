# encoding: UTF-8

class RubyLexer

  # :stopdoc:
  RUBY19 = "".respond_to? :encoding

  IDENT_CHAR_RE = if RUBY19 then
                    /[\w\u0080-\u{10ffff}]/u
                  else
                    /[\w\x80-\xFF]/n
                  end

  IDENT_RE = /^#{IDENT_CHAR_RE}+/o

  attr_accessor :command_start
  attr_accessor :cmdarg
  attr_accessor :cond
  attr_accessor :tern # TODO: rename ternary damnit... wtf
  attr_accessor :string_nest

  ESC_RE = /\\((?>[0-7]{1,3}|x[0-9a-fA-F]{1,2}|M-[^\\]|(C-|c)[^\\]|[^0-7xMCc]))/u
  SIMPLE_STRING_RE = /(#{ESC_RE}|#(#{ESC_RE}|[^\{\#\@\$\"\\])|[^\"\\\#])*/o
  SIMPLE_SSTRING_RE = /(\\.|[^\'])*/
  # :startdoc:

  ##
  # What version of ruby to parse. 18 and 19 are the only valid values
  # currently supported.

  attr_accessor :version

  # Additional context surrounding tokens that both the lexer and
  # grammar use.
  attr_reader :lex_state

  attr_accessor :lex_strterm

  attr_accessor :parser # HACK for very end of lexer... *sigh*

  # Stream of data that yylex examines.
  attr_reader :src

  # Last token read via yylex.
  attr_accessor :token

  attr_accessor :string_buffer

  # Value of last token which had a value associated with it.
  attr_accessor :yacc_value

  # What handles warnings
  attr_accessor :warnings

  attr_accessor :space_seen
  attr_accessor :paren_nest
  attr_accessor :brace_nest
  attr_accessor :lpar_beg

  EOF = :eof_haha!

  # ruby constants for strings (should this be moved somewhere else?)

  # :stopdoc:
  STR_FUNC_BORING = 0x00
  STR_FUNC_ESCAPE = 0x01 # TODO: remove and replace with REGEXP
  STR_FUNC_EXPAND = 0x02
  STR_FUNC_REGEXP = 0x04
  STR_FUNC_QWORDS = 0x08
  STR_FUNC_SYMBOL = 0x10
  STR_FUNC_INDENT = 0x20 # <<-HEREDOC

  STR_SQUOTE = STR_FUNC_BORING
  STR_DQUOTE = STR_FUNC_BORING | STR_FUNC_EXPAND
  STR_XQUOTE = STR_FUNC_BORING | STR_FUNC_EXPAND
  STR_REGEXP = STR_FUNC_REGEXP | STR_FUNC_ESCAPE | STR_FUNC_EXPAND
  STR_SSYM   = STR_FUNC_SYMBOL
  STR_DSYM   = STR_FUNC_SYMBOL | STR_FUNC_EXPAND

  TOKENS = {
    "!"   => :tBANG,
    "!="  => :tNEQ,
    # "!@"  => :tUBANG,
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
  # :startdoc:

  # How the parser advances to the next token.
  #
  # @return true if not at end of file (EOF).

  def advance
    r = yylex
    self.token = r

    raise "yylex returned nil, near #{src.rest[0,10].inspect}" unless r

    return RubyLexer::EOF != r
  end

  def arg_ambiguous
    self.warning("Ambiguous first argument. make sure.")
  end

  def comments
    c = @comments.join
    @comments.clear
    c
  end


  def heredoc here # 63 lines
    _, eos, func, last_line = here

    indent  = (func & STR_FUNC_INDENT) != 0 ? "[ \t]*" : nil
    expand  = (func & STR_FUNC_EXPAND) != 0
    eos_re  = /#{indent}#{Regexp.escape eos}(\r*\n|\z)/
    err_msg = "can't match #{eos_re.inspect} anywhere in "

    rb_compile_error err_msg if
      src.eos?

    if src.beginning_of_line? && src.scan(eos_re) then
      src.unread_many last_line # TODO: figure out how to remove this
      self.yacc_value = eos
      return :tSTRING_END
    end

    self.string_buffer = []

    if expand then
      case
      when src.scan(/#[$@]/) then
        src.pos -= 1 # FIX omg stupid
        self.yacc_value = src.matched
        return :tSTRING_DVAR
      when src.scan(/#[{]/) then
        self.yacc_value = src.matched
        return :tSTRING_DBEG
      when src.scan(/#/) then
        string_buffer << '#'
      end

      begin
        c = tokadd_string func, "\n", nil

        rb_compile_error err_msg if
          c == RubyLexer::EOF

        if c != "\n" then
          self.yacc_value = string_buffer.join.delete("\r")
          return :tSTRING_CONTENT
        else
          string_buffer << src.scan(/\n/)
        end

        rb_compile_error err_msg if
          src.eos?
      end until src.check(eos_re)
    else
      until src.check(eos_re) do
        string_buffer << src.scan(/.*(\n|\z)/)
        rb_compile_error err_msg if
          src.eos?
      end
    end

    self.lex_strterm = [:heredoc, eos, func, last_line]
    self.yacc_value = string_buffer.join.delete("\r")

    return :tSTRING_CONTENT
  end

  def heredoc_identifier # 51 lines
    term, func = nil, STR_FUNC_BORING
    self.string_buffer = []

    case
    when src.scan(/(-?)([\'\"\`])(.*?)\2/) then
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
      string_buffer << src[3]
    when src.scan(/-?([\'\"\`])(?!\1*\Z)/) then
      rb_compile_error "unterminated here document identifier"
    when src.scan(/(-?)(#{IDENT_CHAR_RE}+)/) then
      term = '"'
      func |= STR_DQUOTE
      unless src[1].empty? then
        func |= STR_FUNC_INDENT
      end
      string_buffer << src[2]
    else
      return nil
    end

    if src.scan(/.*\n/) then
      # TODO: think about storing off the char range instead
      line = src.matched
      src.extra_lines_added += 1
    else
      line = nil
    end

    self.lex_strterm = [:heredoc, string_buffer.join, func, line]

    if term == '`' then
      self.yacc_value = "`"
      return :tXSTRING_BEG
    else
      self.yacc_value = "\""
      return :tSTRING_BEG
    end
  end

  def in_lex_state?(*states)
    states.include? lex_state
  end

  def initialize v = 18
    self.version = v
    self.cond   = RubyParserStuff::StackState.new(:cond)
    self.cmdarg = RubyParserStuff::StackState.new(:cmdarg)
    self.tern   = RubyParserStuff::StackState.new(:tern)
    self.string_nest = 0
    self.paren_nest = 0
    self.brace_nest = 0
    self.lpar_beg = nil

    @comments = []

    reset
  end

  def int_with_base base
    rb_compile_error "Invalid numeric format" if src.matched =~ /__/

    self.yacc_value = src.matched.to_i(base)
    return :tINTEGER
  end

  def lex_state= o
    # warn "wtf lex_state = #{o.inspect} from #{caller.first}"
    raise "wtf\?" unless Symbol === o
    @lex_state = o
  end

  attr_writer :lineno
  def lineno
    @lineno ||= src.lineno
  end

  ##
  #  Parse a number from the input stream.
  #
  # @param c The first character of the number.
  # @return A int constant wich represents a token.

  def parse_number
    self.lex_state = :expr_end

    case
    when src.scan(/[+-]?0[xXbBdD]\b/) then
      rb_compile_error "Invalid numeric format"
    when src.scan(/[+-]?(?:(?:[1-9][\d_]*|0)(?!\.\d)\b|0[Dd][0-9_]+)/) then
      int_with_base(10)
    when src.scan(/[+-]?0x[a-f0-9_]+/i) then
      int_with_base(16)
    when src.scan(/[+-]?0[Bb][01_]+/) then
      int_with_base(2)
    when src.scan(/[+-]?0[Oo]?[0-7_]*[89]/) then
      rb_compile_error "Illegal octal digit."
    when src.scan(/[+-]?0[Oo]?[0-7_]+|0[Oo]/) then
      int_with_base(8)
    when src.scan(/[+-]?[\d_]+_(e|\.)/) then
      rb_compile_error "Trailing '_' in number."
    when src.scan(/[+-]?[\d_]+\.[\d_]+(e[+-]?[\d_]+)?\b|[+-]?[\d_]+e[+-]?[\d_]+\b/i) then
      number = src.matched
      if number =~ /__/ then
        rb_compile_error "Invalid numeric format"
      end
      self.yacc_value = number.to_f
      :tFLOAT
    when src.scan(/[+-]?[0-9_]+(?![e])/) then
      int_with_base(10)
    else
      rb_compile_error "Bad number format"
    end
  end

  def parse_quote # 58 lines
    beg, nnd, short_hand, c = nil, nil, false, nil

    if src.scan(/[a-z0-9]{1,2}/i) then # Long-hand (e.g. %Q{}).
      rb_compile_error "unknown type of %string" if src.matched_size == 2
      c, beg, short_hand = src.matched, src.getch, false
    else                               # Short-hand (e.g. %{, %., %!, etc)
      c, beg, short_hand = 'Q', src.getch, true
    end

    if src.eos? or c == RubyLexer::EOF or beg == RubyLexer::EOF then
      rb_compile_error "unterminated quoted string meets end of file"
    end

    # Figure nnd-char.  "\0" is special to indicate beg=nnd and that no nesting?
    nnd = { "(" => ")", "[" => "]", "{" => "}", "<" => ">" }[beg]
    nnd, beg = beg, "\0" if nnd.nil?

    token_type, self.yacc_value = nil, "%#{c}#{beg}"
    token_type, string_type = case c
                              when 'Q' then
                                ch = short_hand ? nnd : c + beg
                                self.yacc_value = "%#{ch}"
                                [:tSTRING_BEG,   STR_DQUOTE]
                              when 'q' then
                                [:tSTRING_BEG,   STR_SQUOTE]
                              when 'W' then
                                src.scan(/\s*/)
                                [:tWORDS_BEG,    STR_DQUOTE | STR_FUNC_QWORDS]
                              when 'w' then
                                src.scan(/\s*/)
                                [:tQWORDS_BEG,   STR_SQUOTE | STR_FUNC_QWORDS]
                              when 'x' then
                                [:tXSTRING_BEG,  STR_XQUOTE]
                              when 'r' then
                                [:tREGEXP_BEG,   STR_REGEXP]
                              when 's' then
                                self.lex_state  = :expr_fname
                                [:tSYMBEG,       STR_SSYM]
                              when 'I' then
                                [:tSYMBOLS_BEG, STR_DQUOTE | STR_FUNC_QWORDS]
                              when 'i' then
                                [:tQSYMBOLS_BEG, STR_SQUOTE | STR_FUNC_QWORDS]
                              end

    rb_compile_error "Bad %string type. Expected [Qq\Wwxrs], found '#{c}'." if
      token_type.nil?

    self.lex_strterm = [:strterm, string_type, nnd, beg]

    return token_type
  end

  def parse_string(quote) # 65 lines
    _, string_type, term, open = quote

    space = false # FIX: remove these
    func = string_type
    paren = open
    term_re = @@regexp_cache[term]

    qwords = (func & STR_FUNC_QWORDS) != 0
    regexp = (func & STR_FUNC_REGEXP) != 0
    expand = (func & STR_FUNC_EXPAND) != 0

    unless func then # FIX: impossible, prolly needs == 0
      self.lineno = nil
      return :tSTRING_END
    end

    space = true if qwords and src.scan(/\s+/)

    if self.string_nest == 0 && src.scan(/#{term_re}/) then
      if qwords then
        quote[1] = nil # TODO: make struct
        return :tSPACE
      elsif regexp then
        self.yacc_value = self.regx_options
        self.lineno = nil
        return :tREGEXP_END
      else
        self.yacc_value = term
        self.lineno = nil
        return :tSTRING_END
      end
    end

    if space then
      return :tSPACE
    end

    self.string_buffer = []

    if expand
      case
      when src.scan(/#(?=[$@])/) then
        return :tSTRING_DVAR
      when src.scan(/#[{]/) then
        return :tSTRING_DBEG
      when src.scan(/#/) then
        string_buffer << '#'
      end
    end

    if tokadd_string(func, term, paren) == RubyLexer::EOF then
      rb_compile_error "unterminated string meets end of file"
    end

    self.yacc_value = string_buffer.join

    return :tSTRING_CONTENT
  end

  def rb_compile_error msg
    msg += ". near line #{self.lineno}: #{src.rest[/^.*/].inspect}"
    raise RubyParser::SyntaxError, msg
  end

  def read_escape # 51 lines
    case
    when src.scan(/\\/) then                  # Backslash
      '\\'
    when src.scan(/n/) then                   # newline
      "\n"
    when src.scan(/t/) then                   # horizontal tab
      "\t"
    when src.scan(/r/) then                   # carriage-return
      "\r"
    when src.scan(/f/) then                   # form-feed
      "\f"
    when src.scan(/v/) then                   # vertical tab
      "\13"
    when src.scan(/a/) then                   # alarm(bell)
      "\007"
    when src.scan(/e/) then                   # escape
      "\033"
    when src.scan(/b/) then                   # backspace
      "\010"
    when src.scan(/s/) then                   # space
      " "
    when src.scan(/[0-7]{1,3}/) then          # octal constant
      (src.matched.to_i(8) & 0xFF).chr
    when src.scan(/x([0-9a-fA-F]{1,2})/) then # hex constant
      src[1].to_i(16).chr
    when src.check(/M-\\[\\MCc]/) then
      src.scan(/M-\\/) # eat it
      c = self.read_escape
      c[0] = (c[0].ord | 0x80).chr
      c
    when src.scan(/M-(.)/) then
      c = src[1]
      c[0] = (c[0].ord | 0x80).chr
      c
    when src.check(/(C-|c)\\[\\MCc]/) then
      src.scan(/(C-|c)\\/) # eat it
      c = self.read_escape
      c[0] = (c[0].ord & 0x9f).chr
      c
    when src.scan(/C-\?|c\?/) then
      127.chr
    when src.scan(/(C-|c)(.)/) then
      c = src[2]
      c[0] = (c[0].ord & 0x9f).chr
      c
    when src.scan(/^[89]/i) then # bad octal or hex... MRI ignores them :(
      src.matched
    when src.scan(/[McCx0-9]/) || src.eos? then
      rb_compile_error("Invalid escape character syntax")
    else
      src.getch
    end
  end

  def regx_options # 15 lines
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
    self.lex_strterm   = nil
    self.token         = nil
    self.yacc_value    = nil

    @src       = nil
    @lex_state = nil
  end

  def ruby18
    Ruby18Parser === parser
  end

  def ruby19
    Ruby19Parser === parser
  end

  def src= src
    raise "bad src: #{src.inspect}" unless String === src
    @src = RPStringScanner.new(src)
  end

  def tokadd_escape term # 20 lines
    case
    when src.scan(/\\\n/) then
      # just ignore
    when src.scan(/\\([0-7]{1,3}|x[0-9a-fA-F]{1,2})/) then
      self.string_buffer << src.matched
    when src.scan(/\\([MC]-|c)(?=\\)/) then
      self.string_buffer << src.matched
      self.tokadd_escape term
    when src.scan(/\\([MC]-|c)(.)/) then
      self.string_buffer << src.matched
    when src.scan(/\\[McCx]/) then
      rb_compile_error "Invalid escape character syntax"
    when src.scan(/\\(.)/m) then
      self.string_buffer << src.matched
    else
      rb_compile_error "Invalid escape character syntax"
    end
  end

  @@regexp_cache = Hash.new { |h,k| h[k] = Regexp.new(Regexp.escape(k)) }
  @@regexp_cache[nil] = nil

  def tokadd_string(func, term, paren) # 105 lines
    qwords = (func & STR_FUNC_QWORDS) != 0
    escape = (func & STR_FUNC_ESCAPE) != 0
    expand = (func & STR_FUNC_EXPAND) != 0
    regexp = (func & STR_FUNC_REGEXP) != 0
    symbol = (func & STR_FUNC_SYMBOL) != 0

    paren_re = @@regexp_cache[paren]
    term_re  = @@regexp_cache[term]

    until src.eos? do
      c = nil
      handled = true

      case
      when paren_re && src.scan(paren_re) then
        self.string_nest += 1
      when src.scan(term_re) then
        if self.string_nest == 0 then
          src.pos -= 1
          break
        else
          self.string_nest -= 1
        end
      when expand && src.scan(/#(?=[\$\@\{])/) then
        src.pos -= 1
        break
      when qwords && src.scan(/\s/) then
        src.pos -= 1
        break
      when expand && src.scan(/#(?!\n)/) then
        # do nothing
      when src.check(/\\/) then
        case
        when qwords && src.scan(/\\\n/) then
          string_buffer << "\n"
          next
        when qwords && src.scan(/\\\s/) then
          c = ' '
        when expand && src.scan(/\\\n/) then
          next
        when regexp && src.check(/\\/) then
          self.tokadd_escape term
          next
        when expand && src.scan(/\\/) then
          c = self.read_escape
        when src.scan(/\\\n/) then
          # do nothing
        when src.scan(/\\\\/) then
          string_buffer << '\\' if escape
          c = '\\'
        when src.scan(/\\/) then
          unless src.scan(term_re) || paren.nil? || src.scan(paren_re) then
            string_buffer << "\\"
          end
        else
          handled = false
        end # inner /\\/ case
      else
        handled = false
      end # top case

      unless handled then
        t = Regexp.escape term
        x = Regexp.escape(paren) if paren && paren != "\000"
        re = if qwords then
               if RUBY19 then
                 /[^#{t}#{x}\#\0\\\s]+|./ # |. to pick up whatever
               else
                 /[^#{t}#{x}\#\0\\\s\v]+|./ # argh. 1.8's \s doesn't pick up \v
               end
             else
               /[^#{t}#{x}\#\0\\]+|./
             end

        src.scan re
        c = src.matched

        rb_compile_error "symbol cannot contain '\\0'" if symbol && c =~ /\0/
      end # unless handled

      c ||= src.matched
      string_buffer << c
    end # until

    c ||= src.matched
    c = RubyLexer::EOF if src.eos?

    return c
  end

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
        else
          s
        end
    x.force_encoding "UTF-8" if RUBY19
    x
  end

  def warning s
    # do nothing for now
  end

  def result lex_state, token, text # :nodoc:
    lex_state = self.arg_state if lex_state == :arg_state
    self.lex_state = lex_state if lex_state
    self.yacc_value = text
    token
  end

  def expr_result token, text
    cond.push false
    cmdarg.push false
    result :expr_beg, token, text
  end

  def arg_state
    in_arg_state? ? :expr_arg : :expr_beg
  end

  def in_arg_state?
    in_lex_state? :expr_fname, :expr_dot
  end

  def space_vs_beginning space_type, beg_type, fallback
    if is_space_arg? src.check(/./m) then
      warning "`**' interpreted as argument prefix"
      space_type
    elsif is_beg? then
      beg_type
    else
      # TODO: warn_balanced("**", "argument prefix");
      fallback
    end
  end

  ##
  # Returns the next token. Also sets yy_val is needed.
  #
  # @return Description of the Returned Value

  def yylex # 461 lines
    c = ''
    self.space_seen = false
    command_state = false
    src = self.src

    self.token = nil
    self.yacc_value = nil

    return yylex_string if lex_strterm

    command_state = self.command_start
    self.command_start = false

    loop do # START OF CASE
      if src.scan(/[\ \t\r\f\v]/) then # \s - \n + \v
        self.space_seen = true
        next
      elsif src.check(/[^a-zA-Z]/) then
        if src.scan(/\n|\#/) then
          self.lineno = nil
          c = src.matched
          if c == '#' then
            src.pos -= 1

            while src.scan(/\s*#.*(\n+|\z)/) do
              @comments << src.matched.gsub(/^ +#/, '#').gsub(/^ +$/, '')
            end

            return RubyLexer::EOF if src.eos?
          end

          # Replace a string of newlines with a single one
          src.scan(/\n+/)

          next if in_lex_state?(:expr_beg, :expr_value, :expr_class,
                                :expr_fname, :expr_dot)

          if src.scan(/([\ \t\r\f\v]*)\./) then
            self.space_seen = true unless src[1].empty?

            src.pos -= 1
            next unless src.check(/\.\./)
          end

          self.command_start = true

          return result(:expr_beg, :tNL, nil)
        elsif src.scan(/[\]\)\}]/) then
          if src.matched == "}" then
            self.brace_nest -= 1
          else
            self.paren_nest -= 1
          end

          cond.lexpop
          cmdarg.lexpop
          tern.lexpop

          text  = src.matched
          state = text == ")" ? :expr_endfn : :expr_endarg
          token = {
            ")" => :tRPAREN,
            "]" => :tRBRACK,
            "}" => :tRCURLY
          }[text]

          return result(state, token, text)
        elsif src.scan(/\!/) then
          if in_arg_state? then
            return result(:expr_arg, :tUBANG, "!@") if src.scan(/@/)
          end

          text = src.scan(/[=~]/) ? "!#{src.matched}" : "!"

          return result(arg_state, TOKENS[text], text)
        elsif src.scan(/\.\.\.?|,|![=~]?/) then
          return result(:expr_beg, TOKENS[src.matched], src.matched)
        elsif src.check(/\./) then
          if src.scan(/\.\d/) then
            rb_compile_error "no .<digit> floating literal anymore put 0 before dot"
          elsif src.scan(/\./) then
            return result(:expr_dot, :tDOT, ".")
          end
        elsif src.scan(/\(/) then
          token = if ruby18 then
                     yylex_paren18
                   else
                     yylex_paren19
                   end

          self.paren_nest += 1

          return expr_result(token, "(")
        elsif src.check(/\=/) then
          if src.scan(/\=\=\=|\=\=|\=~|\=>|\=(?!begin\b)/) then
            tok = src.matched
            return result(:arg_state, TOKENS[tok], tok)
          elsif src.scan(/\=begin(?=\s)/) then
            @comments << src.matched

            unless src.scan(/.*?\n=end( |\t|\f)*[^\n]*(\n|\z)/m) then
              @comments.clear
              rb_compile_error("embedded document meets end of file")
            end

            @comments << src.matched

            next
          else
            raise "you shouldn't be able to get here"
          end
        elsif src.scan(/\"(#{SIMPLE_STRING_RE})\"/o) then
          string = src.matched[1..-2].gsub(ESC_RE) { unescape $1 }
          return result(:expr_end, :tSTRING, string)
        elsif src.scan(/\"/) then # FALLBACK
          self.lex_strterm = [:strterm, STR_DQUOTE, '"', "\0"] # TODO: question this
          return result(nil, :tSTRING_BEG, '"')
        elsif src.scan(/\@\@?#{IDENT_CHAR_RE}+/o) then
          self.token = src.matched

          rb_compile_error "`#{self.token}` is not allowed as a variable name" if
            self.token =~ /\@\d/

          tok_id = src.matched =~ /^@@/ ? :tCVAR : :tIVAR
          return result(:expr_end, tok_id, self.token)
        elsif src.scan(/\:\:/) then
          if is_beg? || in_lex_state?(:expr_class) || is_space_arg? then
            return result(:expr_beg, :tCOLON3, "::")
          end

          return result(:expr_dot, :tCOLON2, "::")
        elsif ! is_end? && src.scan(/:([a-zA-Z_]#{IDENT_CHAR_RE}*(?:[?!]|=(?==>)|=(?![=>]))?)/) then
          # scanning shortcut to symbols
          return result(:expr_end, :tSYMBOL, src[1])
        elsif src.scan(/\:/) then
          # ?: / then / when
          if is_end? || src.check(/\s/) then
            # TODO warn_balanced(":", "symbol literal");
            return result(:expr_beg, :tCOLON, ":")
          end

          case
          when src.scan(/\'/) then
            self.lex_strterm = [:strterm, STR_SSYM, src.matched, "\0"]
          when src.scan(/\"/) then
            self.lex_strterm = [:strterm, STR_DSYM, src.matched, "\0"]
          end

          return result(:expr_fname, :tSYMBEG, ":")
        elsif src.check(/[0-9]/) then
          return parse_number
        elsif src.scan(/\[/) then
          self.paren_nest += 1

          token = nil

          if in_lex_state? :expr_fname, :expr_dot then
            case
            when src.scan(/\]\=/) then
              self.paren_nest -= 1 # HACK? I dunno, or bug in MRI
              return result(:expr_arg, :tASET, "[]=")
            when src.scan(/\]/) then
              self.paren_nest -= 1 # HACK? I dunno, or bug in MRI
              return result(:expr_arg, :tAREF, "[]")
            else
              rb_compile_error "unexpected '['"
            end
          elsif is_beg? then
            self.tern.push false
            token = :tLBRACK
          elsif is_arg? && space_seen then
            self.tern.push false
            token = :tLBRACK
          else
            token = :tLBRACK2
          end

          return expr_result(token, "[")
        elsif src.scan(/\'#{SIMPLE_SSTRING_RE}\'/) then
          text = src.matched[1..-2].gsub(/\\\\/, "\\").gsub(/\\'/, "'") # "
          return result(:expr_end, :tSTRING, text)
        elsif src.check(/\|/) then
          if src.scan(/\|\|\=/) then
            return result(:expr_beg, :tOP_ASGN, "||")
          elsif src.scan(/\|\|/) then
            return result(:expr_beg, :tOROP, "||")
          elsif src.scan(/\|\=/) then
            return result(:expr_beg, :tOP_ASGN, "|")
          elsif src.scan(/\|/) then
            return result(:arg_state, :tPIPE, "|")
          end
        elsif src.scan(/\{/) then
          self.brace_nest += 1
          if lpar_beg && lpar_beg == paren_nest then
            self.lpar_beg = nil
            self.paren_nest -= 1

            return expr_result(:tLAMBEG, "{")
          end

          token = if is_arg? || in_lex_state?(:expr_end, :expr_endfn) then
                     :tLCURLY      #  block (primary)
                   elsif in_lex_state?(:expr_endarg) then
                     :tLBRACE_ARG  #  block (expr)
                   else
                     self.tern.push false
                     :tLBRACE      #  hash
                   end

          self.command_start = true unless token == :tLBRACE

          return expr_result(token, "{")
        elsif src.scan(/->/) then
          return result(:expr_endfn, :tLAMBDA, nil)
        elsif src.scan(/[+-]/) then
          sign = src.matched
          utype, type = if sign == "+" then
                          [:tUPLUS, :tPLUS]
                        else
                          [:tUMINUS, :tMINUS]
                        end

          if in_arg_state? then
            if src.scan(/@/) then
              return result(:expr_arg, utype, "#{sign}@")
            else
              return result(:expr_arg, type, sign)
            end
          end

          return result(:expr_beg, :tOP_ASGN, sign) if src.scan(/\=/)

          if (is_beg? || (is_arg? && space_seen && !src.check(/\s/))) then
            arg_ambiguous if is_arg?

            if src.check(/\d/) then
              return self.parse_number if utype == :tUPLUS
              return result(:expr_beg, :tUMINUS_NUM, sign)
            end

            return result(:expr_beg, utype, sign)
          end

          return result(:expr_beg, type, sign)
        elsif src.check(/\*/) then
          if src.scan(/\*\*=/) then
            return result(:expr_beg, :tOP_ASGN, "**")
          elsif src.scan(/\*\*/) then
            token = space_vs_beginning :tDSTAR, :tDSTAR, :tPOW

            return result(:arg_state, token, "**")
          elsif src.scan(/\*\=/) then
            return result(:expr_beg, :tOP_ASGN, "*")
          elsif src.scan(/\*/) then
            token = space_vs_beginning :tSTAR, :tSTAR, :tSTAR2

            return result(:arg_state, token, "*")
          end
        elsif src.check(/\</) then
          if src.scan(/\<\=\>/) then
            return result(:arg_state, :tCMP, "<=>")
          elsif src.scan(/\<\=/) then
            return result(:arg_state, :tLEQ, "<=")
          elsif src.scan(/\<\<\=/) then
            return result(:arg_state, :tOP_ASGN, "<<")
          elsif src.scan(/\<\</) then
            if (!in_lex_state?(:expr_dot, :expr_class) &&
                !is_end? &&
                (!is_arg? || space_seen)) then
              tok = self.heredoc_identifier
              return tok if tok
            end

            return result(:arg_state, :tLSHFT, "\<\<")
          elsif src.scan(/\</) then
            return result(:arg_state, :tLT, "<")
          end
        elsif src.check(/\>/) then
          if src.scan(/\>\=/) then
            return result(:arg_state, :tGEQ, ">=")
          elsif src.scan(/\>\>=/) then
            return result(:arg_state, :tOP_ASGN, ">>")
          elsif src.scan(/\>\>/) then
            return result(:arg_state, :tRSHFT, ">>")
          elsif src.scan(/\>/) then
            return result(:arg_state, :tGT, ">")
          end
        elsif src.scan(/\`/) then
          case lex_state
          when :expr_fname then
            return result(:expr_end, :tBACK_REF2, "`")
          when :expr_dot then
            state = command_state ? :expr_cmdarg : :expr_arg
            return result(state, :tBACK_REF2, "`")
          else
            self.lex_strterm = [:strterm, STR_XQUOTE, '`', "\0"]
            return result(nil, :tXSTRING_BEG, "`")
          end
        elsif src.scan(/\?/) then
          if is_end? then
            state = ruby18 ? :expr_beg : :expr_value # HACK?
            self.tern.push true
            return result(state, :tEH, "?")
          end

          if src.eos? then
            rb_compile_error "incomplete character syntax"
          end

          if src.check(/\s|\v/) then
            unless is_arg? then
              c2 = { " " => 's',
                    "\n" => 'n',
                    "\t" => 't',
                    "\v" => 'v',
                    "\r" => 'r',
                    "\f" => 'f' }[src.matched]

              if c2 then
                warning("invalid character syntax; use ?\\" + c2)
              end
            end

            # ternary
            state = ruby18 ? :expr_beg : :expr_value # HACK?
            self.tern.push true
            return result(state, :tEH, "?")
          elsif src.check(/\w(?=\w)/) then # ternary, also
            self.tern.push true
            return result(:expr_beg, :tEH, "?")
          end

          c = if src.scan(/\\/) then
                self.read_escape
              else
                src.getch
              end

          if version == 18 then
            return result(:expr_end, :tINTEGER, c[0].ord & 0xff)
          else
            return result(:expr_end, :tSTRING, c)
          end
        elsif src.check(/\&/) then
          if src.scan(/\&\&\=/) then
            return result(:expr_beg, :tOP_ASGN, "&&")
          elsif src.scan(/\&\&/) then
            return result(:expr_beg, :tANDOP, "&&")
          elsif src.scan(/\&\=/) then
            return result(:expr_beg, :tOP_ASGN, "&")
          elsif src.scan(/&/) then
            token = if is_arg? && space_seen && !src.check(/\s/) then
                       warning("`&' interpreted as argument prefix")
                       :tAMPER
                     elsif in_lex_state? :expr_beg, :expr_mid then
                       :tAMPER
                     else
                       :tAMPER2
                     end

            return result(:arg_state, token, "&")
          end
        elsif src.scan(/\//) then
          if is_beg? then
            self.lex_strterm = [:strterm, STR_REGEXP, '/', "\0"]
            return result(nil, :tREGEXP_BEG, "/")
          end

          if src.scan(/\=/) then
            return result(:expr_beg, :tOP_ASGN, "/")
          end

          if is_arg? && space_seen then
            unless src.scan(/\s/) then
              arg_ambiguous
              self.lex_strterm = [:strterm, STR_REGEXP, '/', "\0"]
              return result(nil, :tREGEXP_BEG, "/")
            end
          end

          return result(:arg_state, :tDIVIDE, "/")
        elsif src.scan(/\^=/) then
          return result(:expr_beg, :tOP_ASGN, "^")
        elsif src.scan(/\^/) then
          return result(:arg_state, :tCARET, "^")
        elsif src.scan(/\;/) then
          self.command_start = true
          return result(:expr_beg, :tSEMI, ";")
        elsif src.scan(/\~/) then
          src.scan(/@/) if in_lex_state? :expr_fname, :expr_dot
          return result(:arg_state, :tTILDE, "~")
        elsif src.scan(/\\/) then
          if src.scan(/\r?\n/) then
            self.lineno = nil
            self.space_seen = true
            next
          end
          rb_compile_error "bare backslash only allowed before newline"
        elsif src.scan(/\%/) then
          return parse_quote if is_beg?

          return result(:expr_beg, :tOP_ASGN, "%") if src.scan(/\=/)

          return parse_quote if is_arg? && space_seen && ! src.check(/\s/)

          return result(:arg_state, :tPERCENT, "%")
        elsif src.check(/\$/) then
          if src.scan(/(\$_)(\w+)/) then
            self.token = src.matched
            return result(:expr_end, :tGVAR, src.matched)
          elsif src.scan(/\$_/) then
            return result(:expr_end, :tGVAR, src.matched)
          elsif src.scan(/\$[~*$?!@\/\\;,.=:<>\"]|\$-\w?/) then
            return result(:expr_end, :tGVAR, src.matched)
          elsif src.scan(/\$([\&\`\'\+])/) then
            # Explicit reference to these vars as symbols...
            if lex_state == :expr_fname then
              return result(:expr_end, :tGVAR, src.matched)
            else
              return result(:expr_end, :tBACK_REF, src[1].to_sym)
            end
          elsif src.scan(/\$([1-9]\d*)/) then
            if lex_state == :expr_fname then
              return result(:expr_end, :tGVAR, src.matched)
            else
              return result(:expr_end, :tNTH_REF, src[1].to_i)
            end
          elsif src.scan(/\$0/) then
            return result(:expr_end, :tGVAR, src.matched)
          elsif src.scan(/\$\W|\$\z/) then # TODO: remove?
            return result(:expr_end, "$", "$") # FIX: "$"??
          elsif src.scan(/\$\w+/)
            return result(:expr_end, :tGVAR, src.matched)
          end
        elsif src.check(/\_/) then
          if src.beginning_of_line? && src.scan(/\__END__(\r?\n|\Z)/) then
            self.lineno = nil
            return RubyLexer::EOF
          elsif src.scan(/\_\w*/) then
            self.token = src.matched
            return process_token command_state
          end
        end
      end # END OF CASE

      if src.scan(/\004|\032|\000/) || src.eos? then # ^D, ^Z, EOF
        return RubyLexer::EOF
      else # alpha check
        rb_compile_error "Invalid char #{src.rest[0].chr} in expression" unless
          src.check IDENT_RE
      end

      self.token = src.matched if self.src.scan IDENT_RE

      return process_token command_state
    end
  end

  def yylex_paren18
    self.command_start = true
    token = :tLPAREN2

    if in_lex_state? :expr_beg, :expr_mid then
      token = :tLPAREN
    elsif space_seen then
      if in_lex_state? :expr_cmdarg then
        token = :tLPAREN_ARG
      elsif in_lex_state? :expr_arg then
        self.tern.push false
        warning "don't put space before argument parentheses"
      end
    else
      self.tern.push false
    end

    token
  end

  def yylex_paren19
    if is_beg? then
      :tLPAREN
    elsif is_space_arg? then
      :tLPAREN_ARG
    else
      :tLPAREN2 # plain '(' in parse.y
    end
  end

  def is_arg?
    in_lex_state? :expr_arg, :expr_cmdarg
  end

  def is_end?
    in_lex_state? :expr_end, :expr_endarg, :expr_endfn
  end

  def is_beg?
    in_lex_state? :expr_beg, :expr_value, :expr_mid, :expr_class
  end

  # TODO #define IS_AFTER_OPERATOR() IS_lex_state(EXPR_FNAME | EXPR_DOT)

  def is_space_arg? c = "x"
    is_arg? and space_seen and c !~ /\s/
  end

  def is_label_possible? command_state
    (in_lex_state?(:expr_beg) && !command_state) || is_arg?
  end

  def process_token command_state
    token << src.matched if src.scan(/[\!\?](?!=)/)

    tok_id =
      case
      when token =~ /[!?]$/ then
        :tFID
      when in_lex_state?(:expr_fname) && src.scan(/=(?:(?![~>=])|(?==>))/) then
        # ident=, not =~ => == or followed by =>
        # TODO test lexing of a=>b vs a==>b
        token << src.matched
        :tIDENTIFIER
      when token =~ /^[A-Z]/ then
        :tCONSTANT
      else
        :tIDENTIFIER
      end

    if !ruby18 and is_label_possible?(command_state) and src.scan(/:(?!:)/) then
      return result(:expr_beg, :tLABEL, [token, src.lineno]) # HACK: array?
    end

    unless in_lex_state? :expr_dot then
      # See if it is a reserved word.
      keyword = if ruby18 then # REFACTOR need 18/19 lexer subclasses
                  RubyParserStuff::Keyword.keyword18 token
                else
                  RubyParserStuff::Keyword.keyword19 token
                end

      return process_token_keyword keyword if keyword
    end # unless in_lex_state? :expr_dot

    # TODO:
    # if (mb == ENC_CODERANGE_7BIT && lex_state != EXPR_DOT) {

    state =
      if is_beg? || is_arg? || in_lex_state?(:expr_dot) then
        if !self.in_arg_state? && self.parser.env[token.to_sym] == :lvar then
          :expr_end
        elsif command_state then
          :expr_cmdarg
        else
          :expr_arg
        end
      elsif !ruby18 && in_lex_state?(:expr_fname) then
        :expr_endfn
      else
        :expr_end
      end

    return result(state, tok_id, token)
  end

  def process_token_keyword keyword
    state = keyword.state
    value = [token, src.lineno]

    self.command_start = true if state == :expr_beg and lex_state != :expr_fname

    case
    when lex_state == :expr_fname then
      result(state, keyword.id0, keyword.name)
    when keyword.id0 == :kDO then
      case
      when lpar_beg && lpar_beg == paren_nest then
        self.lpar_beg = nil
        self.paren_nest -= 1
        result(state, :kDO_LAMBDA, value)
      when cond.is_in_state then
        result(state, :kDO_COND, value)
      when cmdarg.is_in_state && lex_state != :expr_cmdarg then
        result(state, :kDO_BLOCK, value)
      when in_lex_state?(:expr_beg, :expr_endarg) then
        result(state, :kDO_BLOCK, value)
      else
        result(state, :kDO, value)
      end
    when in_lex_state?(:expr_beg, :expr_value) then
      result(state, keyword.id0, value)
    when keyword.id0 != keyword.id1 then
      result(:expr_beg, keyword.id1, value)
    else
      result(state, keyword.id1, value)
    end
  end

  def yylex_string # 23 lines
    token = if lex_strterm[0] == :heredoc then
              self.heredoc lex_strterm
            else
              self.parse_string lex_strterm
            end

    if token == :tSTRING_END || token == :tREGEXP_END then
      self.lineno      = nil
      self.lex_strterm = nil
      self.lex_state   = :expr_end
    end

    return token
  end
end
