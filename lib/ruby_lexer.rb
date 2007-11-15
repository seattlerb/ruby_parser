require 'pp'
require 'stringio'
require 'racc/parser'

$: << File.expand_path("~/Work/p4/zss/src/ParseTree/dev/lib")
require 'sexp'

def d s
  warn s.inspect
end

class RubyParser < Racc::Parser
  VERSION = '1.0.0'

  attr_reader :warnings
  attr_accessor :lexer, :in_def, :in_single
  attr_reader :env

  alias :old_yyerror :yyerror
  def yyerror msg=nil
    warn msg if msg
    old_yyerror
  end

  def parse(str)
    raise "bad val: #{str.inspect}" unless String === str

    self.lexer.src = StringIO.new(str)

    @yydebug = ENV.has_key? 'DEBUG'

    do_parse
  end

  def do_parse
    _racc_do_parse_rb(_racc_setup, false)
  end

  def yyparse(recv, mid)
    _racc_yyparse_rb(recv, mid, _racc_setup, true)
  end

  def on_error( error_token_id, error_value, value_stack )
    p :error => [ error_token_id, error_value, value_stack ]
    raise "boom"
  end if ENV["DEBUG"]

  def next_token
    if self.lexer.advance then
      [self.lexer.token, self.lexer.yacc_value]
    else
      return [false, '$end']
    end
  end

  def initialize
    super
    self.lexer = RubyLexer.new
    self.in_def = false
    self.in_single = 0
    @env = Environment.new
  end

  alias :is_in_def :in_def # HACK
  alias :is_in_single :in_single # HACK

  def current_scope # HACK
    nil
  end

  def assignable(lhs, value = nil)
    id = lhs.value.to_sym
    id = id.value.to_sym if Token === id

    raise SyntaxError, "Can't change the value of #{id}" if
      id.to_s =~ /^(?:self|nil|true|false|__LINE__|__FILE__)$/

    result = case id.to_s
             when /^@@/ then
               asgn = in_def or in_single > 0
               s((asgn ? :cvasgn : :cvdecl), id)
             when /^@/ then
               s(:iasgn, id.to_sym)
             when /^\$/ then
               s(:gasgn, id.to_sym)
             when /^[A-Z]/ then
               s(:cdecl, id.to_sym)
             else
               if env.dynamic? then
                 if env.dasgn_curr? id then
                   s(:dasgn_curr, id)
                 else
                   s(:dasgn, id)
                 end
               else
                 s(:lasgn, id)
               end
             end

    self.env[id.to_sym] = self.env.dynamic? ? :dvar : :lvar

    result << value if value

    return result
  end

  def warnings= warnings
    @warnings = warnings

    self.lexer.warnings = warnings
  end

  def arg_add(node1, node2)
    return s(:array, node2) unless node1
    return node1 << node2 if node1[0] == :array
    return s(:argspush, node1, node2)
  end

  def node_assign(lhs, rhs)
    return nil unless lhs

    # value_expr(rhs);

    case lhs[0]
    when :gasgn, :iasgn, :lasgn, :dasgn, :dasgn_curr,
      :masgn, :cdecl, :cvdecl, :cvasgn then
      lhs << rhs
    when :attrasgn, :call then
      args = lhs.array(true) || lhs.argscat(true) || lhs.splat(true) # FIX: fragile
      lhs << arg_add(args, rhs)
    end

    lhs
  end

  def gettable(id)
    id = id.value.to_sym if Token === id  # HACK
    id = id.last.to_sym  if Sexp === id   # HACK
    id = id.to_sym       if String === id # HACK

    return s(:self)  if id == :self
    return s(:nil)   if id == :nil
    return s(:true)  if id == :true
    return s(:false) if id == :false
    raise "not yet"  if id == "__FILE__" # NEW_STR(rb_str_new2(ruby_sourcefile)) 
    raise "not yet"  if id == "__LINE__" # NEW_LIT(INT2FIX(ruby_sourceline))

    result = case id.to_s
             when /^@@/ then
               s(:cvar, id)
             when /^@/ then
               s(:ivar, id)
             when /^\$/ then
               s(:gvar, id)
             when /^[A-Z]/ then
               s(:const, id)
             else
               if env.dynamic? and :dvar == env[id] then
                 s(:dvar, id)
               elsif env[id] then
                 s(:lvar, id)
               else
                 s(:vcall, id)
               end
             end

    return result if result

    raise "identifier #{id.inspect} is not valid"
  end

  def block_append(head, tail)
    # NODE *nnd, *h = head;
    nnd = nil
    h = head

    return head unless tail

     # again:
    return tail unless h

    case h[0]
    when :newline then
      h = h.last
      # goto again # HACK
    when :lit, :str then
      parser_warning(h, "unused literal ignored")
      return tail
    when :block then
      nnd = h.last
      break;
    else
      h = nnd = s(:block, head)
      head = nnd;
    end

    tail = s(:block, tail) unless tail[0] == :block

# HACK
#     nnd->nd_next = tail;
#     h->nd_nnd = tail->nd_nnd;
    return head;
  end

  def new_yield(node)
    if node then
      raise SyntaxException, "Block argument should not be given." if
        node.node_type == :block_pass

      node = node.last if node.node_type == :array and node.size == 2
    end

    return s(:yield, node)
  end

  def new_fcall m, a
    if a and a[0] == :block_pass then
      args = a.array(true) || a.argscat(true) || a.splat(true)
      call = s(:fcall, m)
      call << args if args
      a << call
      return a
    end

    return s(:fcall, m, a)
  end

  def arg_blk_pass node1, node2
    if node2 then
      node2.insert 1, node1
      return node2
    else
      node1
    end
  end

  def get_match_node lhs, rhs
    if lhs.first == :lit and Regexp === lhs.last then
      return s(:match2, lhs, rhs)
    elsif rhs.first == :lit and Regexp === rhs.last then
      return s(:match3, rhs, lhs)
    end

    return s(:call, lhs, :"=~", s(:array, rhs))
  end

  def cond node
    return nil if node.nil?

    case node.first
    when :dregex then
      return s(:match2, node, s(:gvar, "$_".to_sym))
    when :regex then
      return s(:match, node)
    when :lit then
      if Regexp === node.last then
        return s(:match, node)
      else
        return node
      end
    when :and then
      return s(:and, cond0(node[1]), cond0(node[2]))
    when :or then
      return s(:or,  cond0(node[1]), cond0(node[2]))
    when :dot2 then
      return node if node[1][0] == :lit
      label = "flip#{node.hash}"
      env[label] = self.env.dynamic? ? :dvar : :lvar
      return s(:flip2, node[1], node[2])
    when :dot3 then
      return node if node[1][0] == :lit
      label = "flip#{node.hash}"
      env[label] = self.env.dynamic? ? :dvar : :lvar
      return s(:flip3, node[1], node[2])
    else
      return node
    end
  end

  ############################################################
  # HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK

  def support
    self
  end

  def push_local_scope dyn = false
    self.env.extend dyn
  end

  def pop_local_scope
    self.env.unextend
  end

  def append_to_block head, tail
    return head if tail.nil?
    return tail if head.nil?

    head = s(:block, head) unless head.first == :block
    head << tail
  end

  def new_super args, operation
    # HACK ((BlockPassNode) args).getArgsNode
    return s(:super, args) if args && args.first == :block_pass

    return s(:super, args)
  end

  def aryset receiver, index
    s(:attrasgn, receiver, :"[]=", index)
  end

  def arg_concat node1, node2
    return node2.nil? ? node1 : s(:argscat, node1, node2)
  end

  def list_append list, item
    return s(:array, item) unless list
    list.push(*item[1..-1])
    list
  end

  def literal_concat head, tail
    return tail unless head
    return head unless tail

    htype = head[0]

    if htype == :evstr then
      node = s(:dstr, '')
      head = list_append(node, head);
    end

    case tail[0]
    when :str then
      if htype == :str
        head[-1] += tail[-1]
      else
        head << tail
      end
    when :dstr then
      if htype == :str then
        head[-1] += tail[-1]
      else
        head << s(:str, tail[-1])
      end
    when :evstr then
      head[0] = :dstr if htype == :str
      head = list_append(head, tail);
    end

    return head;
  end

  # END HACK
  ############################################################$

end

class Environment

  attr_reader :env

  def initialize dyn = false
    @dyn = []
    @env = []
    self.extend
  end

  def [] k
    self.all[k]
  end

  def []= k, v
    raise "no" if v == true
    self.current[k] = v
  end

  def has_key? k
    self.all.has_key? k
  end

  def all
    @env.reverse.inject { |env, scope| env.merge scope }
  end

  def current
    @env.first
  end

  def dynamic?
    @dyn.any?
  end

  def dasgn_curr? name
    (! has_key?(name) && @dyn.first) || current.has_key?(name)
  end

  def extend dyn = false
    @dyn.unshift dyn
    @env.unshift({})
  end

  def unextend
    @dyn.shift
    @env.shift
    raise "You went too far unextending env" if @env.empty?
  end

  def scope
    self.extend
    begin
      yield
    ensure
      self.unextend
    end
  end
end

class StringBuffer # TODO: remove
  def initialize(n = nil)
    @s = []
  end

  def << s
    @s << s
  end
  alias :append :<<

  def to_s
    @s.join
  end

  def [](i)
    self.to_s[i].chr
  end
  alias :char_at :[]

  def size
    @s.size
  end
  alias :length :size

  def clear
    @s.clear
  end
end

class LexState
  attr_accessor :debug

  def initialize debug
    self.debug = debug
  end

  EXPR_BEG    = LexState.new "EXPR_BEG"
  EXPR_END    = LexState.new "EXPR_END"
  EXPR_ARG    = LexState.new "EXPR_ARG"
  EXPR_CMDARG = LexState.new "EXPR_CMDARG"
  EXPR_ENDARG = LexState.new "EXPR_ENDARG"
  EXPR_MID    = LexState.new "EXPR_MID"
  EXPR_FNAME  = LexState.new "EXPR_FNAME"
  EXPR_DOT    = LexState.new "EXPR_DOT"
  EXPR_CLASS  = LexState.new "EXPR_CLASS"

  def is_expr_beg
    return self == EXPR_BEG
  end

  def is_expr_end
    return self == EXPR_END
  end

  def is_expr_arg
    return self == EXPR_ARG
  end

  def is_expr_cmd_arg
    return self == EXPR_CMDARG
  end

  def is_expr_end_arg
    return self == EXPR_ENDARG
  end

  def is_expr_mid
    return self == EXPR_MID
  end

  def is_expr_fname
    return self == EXPR_FNAME
  end

  def is_expr_dot
    return self == EXPR_DOT
  end

  def is_expr_class
    return self == EXPR_CLASS
  end

  def is_argument
    return self == EXPR_ARG || self == EXPR_CMDARG
  end

  def to_s
    return debug
  end
end

class SyntaxException < SyntaxError
  def initialize(msg)
    super(msg)
  end
end

class StackState # TODO: nuke this fucker
  attr_reader :stack

  def inspect
    "StackState(#{@name}, #{@stack.inspect})"
  end

  def initialize(name)
    @name = name
    @stack = [false]
  end

  def pop
    raise if @stack.size == 0
    self.stack.pop
  end

  def lexpop
    raise if @stack.size == 0
    a = self.stack.pop
    b = self.stack.pop
    self.stack.push(a || b)
  end

  def push val
    raise if val != true and val != false
    self.stack.push val
  end

  def is_in_state
    self.stack.last
  end
end

class StringIO # HACK: everything in here is a hack
  alias :old_read :read
  def read
    c = self.getc
    if c and c != 0 then
      c.chr
    else
      ::RubyLexer::EOF
    end
  end

  def peek expected
    c = self.read
    self.unread c
    c == expected
  end

  def unread(c)
    self.ungetc c[0] if c
  end
end

class Tokens
  def self.method_missing meth, *args
    meth
  end
end

class Keyword
  class KWtable
    attr_accessor :name, :id, :state
    def initialize(name, id=[], state=nil)
      @name = name
      @id = id
      @state = state
    end

    def id0
      self.id.first
    end

    def id1
      self.id.last
    end
  end

  TOTAL_KEYWORDS  = 40
  MIN_WORD_LENGTH =  2
  MAX_WORD_LENGTH =  8
  MIN_HASH_VALUE  =  6
  MAX_HASH_VALUE  = 55
  # maximum key range = 50, duplicates = 0

  EXPR_BEG    = LexState::EXPR_BEG    # ignore newline, +/- is a sign.
  EXPR_END    = LexState::EXPR_END    # newline significant, +/- is a operator.
  EXPR_ARG    = LexState::EXPR_ARG    # newline significant, +/- is a operator.
  EXPR_CMDARG = LexState::EXPR_CMDARG # newline significant, +/- is a operator.
  EXPR_ENDARG = LexState::EXPR_ENDARG # newline significant, +/- is a operator.
  EXPR_MID    = LexState::EXPR_MID    # newline significant, +/- is a operator.
  EXPR_FNAME  = LexState::EXPR_FNAME  # ignore newline, no reserved words.
  EXPR_DOT    = LexState::EXPR_DOT    # right after . or ::, no reserved words.
  EXPR_CLASS  = LexState::EXPR_CLASS  # immediate after class, no here document.

  # TODO: remove this and inline below
  K_CLASS          = Tokens.kCLASS
  K_MODULE         = Tokens.kMODULE
  K_DEF            = Tokens.kDEF
  K_UNDEF          = Tokens.kUNDEF
  K_BEGIN          = Tokens.kBEGIN
  K_RESCUE         = Tokens.kRESCUE
  K_ENSURE         = Tokens.kENSURE
  K_END            = Tokens.kEND
  K_IF             = Tokens.kIF
  K_UNLESS         = Tokens.kUNLESS
  K_THEN           = Tokens.kTHEN
  K_ELSIF          = Tokens.kELSIF
  K_ELSE           = Tokens.kELSE
  K_CASE           = Tokens.kCASE
  K_WHEN           = Tokens.kWHEN
  K_WHILE          = Tokens.kWHILE
  K_UNTIL          = Tokens.kUNTIL
  K_FOR            = Tokens.kFOR
  K_BREAK          = Tokens.kBREAK
  K_NEXT           = Tokens.kNEXT
  K_REDO           = Tokens.kREDO
  K_RETRY          = Tokens.kRETRY
  K_IN             = Tokens.kIN
  K_DO             = Tokens.kDO
  K_DO_COND        = Tokens.kDO_COND
  K_DO_BLOCK       = Tokens.kDO_BLOCK
  K_RETURN         = Tokens.kRETURN
  K_YIELD          = Tokens.kYIELD
  K_SUPER          = Tokens.kSUPER
  K_SELF           = Tokens.kSELF
  K_NIL            = Tokens.kNIL
  K_TRUE           = Tokens.kTRUE
  K_FALSE          = Tokens.kFALSE
  K_AND            = Tokens.kAND
  K_OR             = Tokens.kOR
  K_NOT            = Tokens.kNOT
  K_IF_MOD         = Tokens.kIF_MOD
  K_UNLESS_MOD     = Tokens.kUNLESS_MOD
  K_WHILE_MOD      = Tokens.kWHILE_MOD
  K_UNTIL_MOD      = Tokens.kUNTIL_MOD
  K_RESCUE_MOD     = Tokens.kRESCUE_MOD
  K_ALIAS          = Tokens.kALIAS
  K_DEFINED        = Tokens.kDEFINED
  K_lBEGIN         = Tokens.klBEGIN
  K_lEND           = Tokens.klEND
  K___LINE__       = Tokens.k__LINE__
  K___FILE__       = Tokens.k__FILE__
  T_IDENTIFIER     = Tokens.tIDENTIFIER
  T_FID            = Tokens.tFID
  T_GVAR           = Tokens.tGVAR
  T_IVAR           = Tokens.tIVAR
  T_CONSTANT       = Tokens.tCONSTANT
  T_CVAR           = Tokens.tCVAR
  T_INTEGER        = Tokens.tINTEGER
  T_FLOAT          = Tokens.tFLOAT
  T_STRING_CONTENT = Tokens.tSTRING_CONTENT
  T_NTH_REF        = Tokens.tNTH_REF
  T_BACK_REF       = Tokens.tBACK_REF
  T_REGEXP_END     = Tokens.tREGEXP_END
  T_UPLUS          = Tokens.tUPLUS
  T_UMINUS         = Tokens.tUMINUS
  T_POW            = Tokens.tPOW
  T_CMP            = Tokens.tCMP
  T_EQ             = Tokens.tEQ
  T_EQQ            = Tokens.tEQQ
  T_NEQ            = Tokens.tNEQ
  T_GEQ            = Tokens.tGEQ
  T_LEQ            = Tokens.tLEQ
  T_ANDOP          = Tokens.tANDOP
  T_OROP           = Tokens.tOROP
  T_MATCH          = Tokens.tMATCH
  T_NMATCH         = Tokens.tNMATCH
  T_DOT2           = Tokens.tDOT2
  T_DOT3           = Tokens.tDOT3
  T_AREF           = Tokens.tAREF
  T_ASET           = Tokens.tASET
  T_LSHFT          = Tokens.tLSHFT
  T_RSHFT          = Tokens.tRSHFT
  T_COLON2         = Tokens.tCOLON2
  T_COLON3         = Tokens.tCOLON3
  T_OP_ASGN        = Tokens.tOP_ASGN
  T_ASSOC          = Tokens.tASSOC
  T_LPAREN         = Tokens.tLPAREN
  T_LPAREN_ARG     = Tokens.tLPAREN_ARG
  T_RPAREN         = Tokens.tRPAREN
  T_LBRACK         = Tokens.tLBRACK
  T_LBRACE         = Tokens.tLBRACE
  T_LBRACE_ARG     = Tokens.tLBRACE_ARG
  T_STAR           = Tokens.tSTAR
  T_AMPER          = Tokens.tAMPER
  T_SYMBEG         = Tokens.tSYMBEG
  T_STRING_BEG     = Tokens.tSTRING_BEG
  T_XSTRING_BEG    = Tokens.tXSTRING_BEG
  T_REGEXP_BEG     = Tokens.tREGEXP_BEG
  T_WORDS_BEG      = Tokens.tWORDS_BEG
  T_QWORDS_BEG     = Tokens.tQWORDS_BEG
  T_STRING_DBEG    = Tokens.tSTRING_DBEG
  T_STRING_DVAR    = Tokens.tSTRING_DVAR
  T_STRING_END     = Tokens.tSTRING_END
  T_LOWEST         = Tokens.tLOWEST
  T_UMINUS_NUM     = Tokens.tUMINUS_NUM
  T_LAST_TOKEN     = Tokens.tLAST_TOKEN

  def self.hash(str, len)
    asso_values = [
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 11, 56, 56, 36, 56,  1, 37,
                   31,  1, 56, 56, 56, 56, 29, 56,  1, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56,  1, 56, 32,  1,  2,
                   1,   1,  4, 23, 56, 17, 56, 20,  9,  2,
                   9,  26, 14, 56,  5,  1,  1, 16, 56, 21,
                   20,  9, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
                   56, 56, 56, 56, 56, 56
                  ]
    hval = len;

    case hval
    when 2,1 then
      hval += asso_values[str[0]];
    else
      hval += asso_values[str[2]];
      hval += asso_values[str[0]];
    end

    hval += asso_values[str[len - 1]];
    return hval
  end

  def self.keyword(str, len = str.size)
    wordlist = [
                [""], [""], [""], [""], [""], [""],
                ["end",       [K_END, K_END], EXPR_END],
                ["else",      [K_ELSE, K_ELSE], EXPR_BEG],
                ["case",      [K_CASE, K_CASE], EXPR_BEG],
                ["ensure",    [K_ENSURE, K_ENSURE], EXPR_BEG],
                ["module",    [K_MODULE, K_MODULE], EXPR_BEG],
                ["elsif",     [K_ELSIF, K_ELSIF], EXPR_BEG],
                ["def",       [K_DEF, K_DEF], EXPR_FNAME],
                ["rescue",    [K_RESCUE, K_RESCUE_MOD], EXPR_MID],
                ["not",       [K_NOT, K_NOT], EXPR_BEG],
                ["then",      [K_THEN, K_THEN], EXPR_BEG],
                ["yield",     [K_YIELD, K_YIELD], EXPR_ARG],
                ["for",       [K_FOR, K_FOR], EXPR_BEG],
                ["self",      [K_SELF, K_SELF], EXPR_END],
                ["false",     [K_FALSE, K_FALSE], EXPR_END],
                ["retry",     [K_RETRY, K_RETRY], EXPR_END],
                ["return",    [K_RETURN, K_RETURN], EXPR_MID],
                ["true",      [K_TRUE, K_TRUE], EXPR_END],
                ["if",        [K_IF, K_IF_MOD], EXPR_BEG],
                ["defined?",  [K_DEFINED, K_DEFINED], EXPR_ARG],
                ["super",     [K_SUPER, K_SUPER], EXPR_ARG],
                ["undef",     [K_UNDEF, K_UNDEF], EXPR_FNAME],
                ["break",     [K_BREAK, K_BREAK], EXPR_MID],
                ["in",        [K_IN, K_IN], EXPR_BEG],
                ["do",        [K_DO, K_DO], EXPR_BEG],
                ["nil",       [K_NIL, K_NIL], EXPR_END],
                ["until",     [K_UNTIL, K_UNTIL_MOD], EXPR_BEG],
                ["unless",    [K_UNLESS, K_UNLESS_MOD], EXPR_BEG],
                ["or",        [K_OR, K_OR], EXPR_BEG],
                ["next",      [K_NEXT, K_NEXT], EXPR_MID],
                ["when",      [K_WHEN, K_WHEN], EXPR_BEG],
                ["redo",      [K_REDO, K_REDO], EXPR_END],
                ["and",       [K_AND, K_AND], EXPR_BEG],
                ["begin",     [K_BEGIN, K_BEGIN], EXPR_BEG],
                ["__LINE__",  [K___LINE__, K___LINE__], EXPR_END],
                ["class",     [K_CLASS, K_CLASS], EXPR_CLASS],
                ["__FILE__",  [K___FILE__, K___FILE__], EXPR_END],
                ["END",       [K_lEND, K_lEND], EXPR_END],
                ["BEGIN",     [K_lBEGIN, K_lBEGIN], EXPR_END],
                ["while",     [K_WHILE, K_WHILE_MOD], EXPR_BEG],
                [""], [""], [""], [""], [""], [""], [""], [""], [""],
                [""],
                ["alias", [K_ALIAS, K_ALIAS], EXPR_FNAME]
               ].map { |args| KWtable.new(*args) }

    if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH) then
      key = hash(str, len)
      if (key <= MAX_HASH_VALUE && key >= 0) then
        s = wordlist[key].name;
        return wordlist[key] if str == s
      end
    end

    return nil
  end
end

############################################################
# HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK

class Sexp
  def value
    raise "multi item sexp" if size > 2
    last
  end

  def node_type
    first
  end
end

def bitch
  c = caller
  m = c[0].split.last
  warn "bitch: you shouldn't be doing #{m}: from #{c[1]}"
end

class NilClass
  def method_missing msg, *args # HACK
    c = caller
    warn "called #{msg} on nil: from #{c[0]}"
    nil
  end
end

class Token
  attr_accessor :args
  def initialize(token)
    @args = Array(token)
  end

  def value # TODO: eventually phase this out (or make it official)
    self.args.first
  end

  def first # HACK
    self.args.first
  end

  def inspect
    "t(#{args.join.inspect})"
  end

  def to_sym
    self.value.to_sym
  end
end

# END HACK
############################################################

class RubyLexer
  attr_accessor :command_start
  attr_accessor :cmdarg
  attr_accessor :cond
  attr_accessor :nest

  # Additional context surrounding tokens that both the lexer and
  # grammar use.
  attr_reader :lex_state

  def lex_state= o
    raise "wtf?" if o.nil?
    raise "wtf?" if Fixnum === o
    @lex_state = o
  end

  attr_accessor :lex_strterm

  # Used for tiny smidgen of grammar in lexer
  attr_accessor :parser_support # TODO: remove

  # Stream of data that yylex examines.
  attr_accessor :src

  # Last token read via yylex.
  attr_accessor :token

  # Tempory buffer to build up a potential token.  Consumer takes
  # responsibility to reset this before use.
  attr_accessor :token_buffer

  # Value of last token which had a value associated with it.
  attr_accessor :yacc_value

  # What handles warnings
  attr_accessor :warnings

  # TODO: remove all of these
  alias :source= :src=
  alias :str_term :lex_strterm
  alias :str_term= :lex_strterm=
  alias :state :lex_state
  alias :state= :lex_state=
  alias :value :yacc_value
  alias :value= :yacc_value=
  alias :getCmdArgumentState :cmdarg

  # Give a name to a value.  Enebo: This should be used more.
  EOF = 0

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
    self.parser_support = nil
    self.token_buffer = StringBuffer.new 60
    self.cond = StackState.new(:cond)
    self.cmdarg = StackState.new(:cmdarg)
    self.nest = 0

    reset
  end

  def reset
    self.token = 0
    self.yacc_value = nil
    self.src = nil
    @lex_state = nil
    self.lex_strterm = nil
    self.command_start = true
  end

  # How the parser advances to the next token.
  #
  # @return true if not at end of file (EOF).

  def advance
    r = yylex
    self.token = r
    return r != EOF
  end

  def is_next_identchar # TODO: ?
    c = src.read
    src.unread c

    return c != EOF && c =~ /\w/
  end

  def nextc # HACK
    src.read
  end

  def pushback c # HACK
    src.ungetc c[0]
  end

  def parse_string(quote)
    _, string_type, term, open, nest = quote

    space = false
    func = string_type
    paren = open

    return Tokens.tSTRING_END unless func

    c = nextc

    if (func & STR_FUNC_QWORDS) != 0 && c =~ /\s/ then
      begin
        c = nextc
        break if c == RubyLexer::EOF # HACK UGH
      end while String === c and c =~ /\s/
      space = true
    end

    if c == term && ! nest then
      if func & STR_FUNC_QWORDS != 0 then
        quote[1] = nil
        return ' '
      end
      return Tokens.tSTRING_END unless func & STR_FUNC_REGEXP != 0
      # HACK yylval.num = self.regx_options
      return Tokens.tREGEXP_END
    end

    if space then
      pushback(c)
      return ' '
    end

    token_buffer = []

    if (func & STR_FUNC_EXPAND) != 0 && c == '#' then
      case c = nextc
      when '$', '@' then
        pushback(c)
        return Tokens.tSTRING_DVAR
      when '{' then
        return Tokens.tSTRING_DBEG
      end
      token_buffer << '#'
    end

    pushback(c)

    if (tokadd_string(func, term, paren, token_buffer) == -1) then
      # ruby_sourceline = nd_line(quote);
      # HACK rb_compile_error("unterminated string meets end of file");
      return Tokens.tSTRING_END;
    end
    
    self.yacc_value = s(:str, token_buffer.join)
    return Tokens.tSTRING_CONTENT
  end

  def tokadd_string(func, term, paren, buffer)
    while ((c = src.read()) != RubyLexer::EOF)
      if (paren != '\0' && c == paren) then
        self.nest += 1;
      elsif (c == term) then
        if (nest == 0) then
          src.unread(c);
          break
        end
        self.nest -= 1
      elsif ((func & RubyLexer::STR_FUNC_EXPAND) != 0 && c == '#' && !src.peek('\n')) then
        c2 = src.read();

        if (c2 == '$' || c2 == '@' || c2 == '{') then
          src.unread(c2);
          src.unread(c);
          break
        end
        src.unread(c2);
      elsif (c == '\\') then
        c = src.read();
        case c
        when '\n' then
          break if ((func & RubyLexer::STR_FUNC_QWORDS) != 0) # TODO: check break
          next  if ((func & RubyLexer::STR_FUNC_EXPAND) != 0)

          buffer << '\\'
        when '\\' then
          buffer << c if ((func & RubyLexer::STR_FUNC_ESCAPE) != 0)
        else
          if ((func & RubyLexer::STR_FUNC_REGEXP) != 0) then
            src.unread(c);
            parseEscapeIntoBuffer(src, buffer);
            continue;
          elsif ((func & RubyLexer::STR_FUNC_EXPAND) != 0) then
            src.unread(c);
            if ((func & RubyLexer::STR_FUNC_ESCAPE) != 0) then
              buffer << '\\'
            end
            c = src.readEscape();
          elsif ((func & RubyLexer::STR_FUNC_QWORDS) != 0 &&
                 Character.isWhitespace(c)) then
            # ignore backslashed spaces in %w
          elsif (c != term && !(paren != '\0' && c == paren)) then
            buffer << '\\'
          end
        end
      elsif ((func & RubyLexer::STR_FUNC_QWORDS) != 0 &&
             Character.isWhitespace(c)) then
        src.unread(c);
      end
      if (c == '\0' && (func & RubyLexer::STR_FUNC_SYMBOL) != 0) then
        throw new SyntaxException(src.getPosition(), "symbol cannot contain '\\0'");
      end
      buffer << c
    end # while
    return c;
  end

  # static int
  # here_document(here)
  #     NODE *here;
  # {
  #     int c, func, indent = 0;
  #     char *eos, *p, *pend;
  #     long len;
  #     VALUE str = 0;

  #     eos = RSTRING(here->nd_lit)->ptr;
  #     len = RSTRING(here->nd_lit)->len - 1;
  #     indent = (func = *eos++) & STR_FUNC_INDENT;

  #     if ((c = nextc()) == -1) then
  #       error:
  #         rb_compile_error("can't find string \"%s\" anywhere before EOF", eos);
  #         heredoc_restore(lex_strterm);
  #         lex_strterm = nil
  #         return 0;
  #     end
  #     if (was_bol() && whole_match_p(eos, len, indent)) then
  #         heredoc_restore(lex_strterm);
  #         return tSTRING_END;
  #     end

  #     if (!(func & STR_FUNC_EXPAND)) then
  #         begin
  #             p = RSTRING(lex_lastline)->ptr;
  #             pend = lex_pend;
  #             if (pend > p) then
  #                 switch (pend[-1]) then
  #                   case '\n':
  #                     if (--pend == p || pend[-1] != '\r') then
  #                         pend++;
  #                         break;
  #                     end
  #                   case '\r':
  #                     --pend;
  #                 end
  #             end
  #             if (str)
  #                 rb_str_cat(str, p, pend - p);
  #             else
  #                 str = rb_str_new(p, pend - p);
  #             if (pend < lex_pend) rb_str_cat(str, "\n", 1);
  #             lex_p = lex_pend;
  #             if (nextc() == -1) then
  #                 if (str) dispose_string(str);
  #                 goto error;
  #             end
  #         end while (!whole_match_p(eos, len, indent));
  #     end
  #     else then
  #         newtok();
  #         if (c == '#') then
  #             switch (c = nextc()) then
  #               case '$':
  #               case '@':
  #                 pushback(c);
  #                 return tSTRING_DVAR;
  #               case '{':
  #                 return tSTRING_DBEG;
  #             end
  #             tokadd('#');
  #         end
  #         begin
  #             pushback(c);
  #             if ((c = tokadd_string(func, '\n', 0, NULL)) == -1) goto error;
  #             if (c != '\n') then
  #                 yylval.node = NEW_STR(rb_str_new(tok(), toklen()));
  #                 return tSTRING_CONTENT;
  #             end
  #             tokadd(nextc());
  #             if ((c = nextc()) == -1) goto error;
  #         end while (!whole_match_p(eos, len, indent));
  #         str = rb_str_new(tok(), toklen());
  #     end
  #     heredoc_restore(lex_strterm);
  #     lex_strterm = NEW_STRTERM(nil, 0, 0);
  #     yylval.node = NEW_STR(str);
  #     return tSTRING_CONTENT;
  # end

  ##
  # Do the next characters from the source match provided String in a
  # case insensitive manner. If so, then consume those characters and
  # that string. Otherwise, consume none of them and return null.
  #
  # TODO: teach comments English
  #
  # @param s to be matched against
  # @return string if string matches, null otherwise

  def is_next_no_case(s) # TODO: ?
    buf = StringBuffer.new

    s.each_byte do |b|
      c = b.chr
      r = src.read
      buf.append(r)

      if c.downcase != r.downcase then
        src.unread_many(buf)
        return null
      end
    end

    return buf.to_s
  end

  ##
  # @param c the character to test
  # @return true if character is a hex value (0-9a-f)

  def is_hex_char(c) # TODO: ?
    c =~ /[a-f0-9]/i
  end

  ##
  # @param c the character to test
  # @return true if character is an octal value (0-7)

  def is_oct_char(c) # TODO: ?
    c =~ /[0-7]/
  end

  ##
  # @param c is character to be compared
  # @return whether c is an identifier or not

  def is_identifier_char(c) # TODO: ?
    c =~ /\w/
  end

  ##
  # What type/kind of quote are we dealing with?
  #
  # @param c first character the the quote construct
  # @return a token that specifies the quote type

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
        raise SyntaxException("unknown type of %string")
      end
    end

    if c == EOF or beg == EOF then
      raise SyntaxException("unterminated quoted string meets nnd of file")
    end

    # Figure nnd-char.  '\0' is special to indicate beg=nnd and that no nesting?
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
            nnd, beg = beg, '\0'
            nnd
          end

    string_type, token_type = STR_DQUOTE, Tokens.tSTRING_BEG
    self.yacc_value = Token.new("%#{c}#{beg}")

    case (c)
    when 'Q' then
      self.yacc_value = Token.new("%#{short_hand ? nnd : c + beg}")
    when 'q' then
      string_type, token_type = STR_SQUOTE, Tokens.tSTRING_BEG
    when 'W' then
      string_type, token_type = STR_DQUOTE | STR_FUNC_QWORDS, Tokens.tWORDS_BEG
      begin c = src.read end while c =~ /\s/
      src.unread(c)
    when 'w' then
      string_type, token_type = STR_SQUOTE | STR_FUNC_QWORDS, Tokens.tQWORDS_BEG
      begin c = src.read end while c =~ /\s/
      src.unread(c)
    when 'x' then
      string_type, token_type = STR_XQUOTE, Tokens.tXSTRING_BEG
    when 'r' then
      string_type, token_type = STR_REGEXP, Tokens.tREGEXP_BEG
    when 's' then
      string_type, token_type = STR_SSYM, Tokens.tSYMBEG
      self.lex_state = LexState::EXPR_FNAME
    else
      raise SyntaxException("Unknown type of %string. Expected 'Q', 'q', 'w', 'x', 'r' or any non letter character, but found '" + c + "'.")
    end

    self.lex_strterm = s(:strterm, string_type, nnd, beg)

    return token_type
  end

  def here_document_identifier
    c = src.read
    term = 42
    func = 0

    if c == '-' then
      c = src.read
      func = STR_FUNC_INDENT
    end

    if c == '\'' || c == '"' || c == '`' then
      if c == '\'' then
        func |= STR_SQUOTE
      elsif c == '"'
        func |= STR_DQUOTE
      else
        func |= STR_XQUOTE
      end

      token_buffer.clear
      term = c

      while (c = src.read) != EOF && c != term
        token_buffer.append c
      end

      if c == EOF then
        raise SyntaxException("unterminated here document identifier")
      end
    else
      unless is_identifier_char c then
        src.unread c
        src.unread '-' if (func & STR_FUNC_INDENT) != 0
        return 0
      end
      token_buffer.clear
      term = '"'
      func |= STR_DQUOTE
      begin
        token_buffer.append c
      end while (c = src.read) != EOF && is_identifier_char(c)
      src.unread c
    end

    line = src.read_line + '\n'
    tok = token_buffer.to_s
    self.lex_strterm = s(:heredoc, tok, func, line)

    if term == '`' then
      self.yacc_value = Token.new("`")
      return Tokens.tXSTRING_BEG
    end

    self.yacc_value = Token.new("\"")
    return Tokens.tSTRING_BEG
  end

  def arg_ambiguous
    self.warnings.warning("Ambiguous first argument. make sure.")
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
    token_buffer.append c

    while (c = src.read) != "\n" do
      break if c == EOF
      token_buffer.append c
    end
    src.unread c

    # Store away each comment to parser result so IDEs can do whatever
    # they want with them.
    # HACK parser_support.result.add_comment(Node.comment(token_buffer.to_s))

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
        token = here_document(lex_strterm)
        if token == Tokens.tSTRING_END then
          self.lex_strterm = nil
          self.lex_state = LexState::EXPR_END
        end
      else
        token = parse_string(lex_strterm)
        if (token == Tokens.tSTRING_END || token == Tokens.tREGEXP_END) then
          self.lex_strterm = nil
          self.lex_state = LexState::EXPR_END
        end
      end

      return token
    end

    command_state = command_start
    self.command_start = false

    last_state = lex_state

    loop do
      c = src.read
      case c
      when /\004|\032|\000/, 0 then # ^D, ^Z, EOF
        return 0
      when /\ |\t|\f|\r|\13/ then # white spaces, 13 = '\v
        space_seen = true
        next
      when /#|\n/ then
        return 0 if c == '#' and read_comment(c) == 0
        # Replace a string of newlines with a single one
        while ((c = src.read) == "\n")
          # do nothing
        end

        src.unread c

        if (lex_state == LexState::EXPR_BEG   ||
            lex_state == LexState::EXPR_FNAME ||
            lex_state == LexState::EXPR_DOT   ||
            lex_state == LexState::EXPR_CLASS) then
          next
        end

        self.command_start = true
        self.lex_state = LexState::EXPR_BEG
        return "\n"
      when '*' then
        c = src.read
        if c == '*' then
          c = src.read
          if c == '=' then
            self.lex_state = LexState::EXPR_BEG
            self.yacc_value = Token.new("**")
            return Tokens.tOP_ASGN
          end
          src.unread c
          self.yacc_value = Token.new("**")
          c = Tokens.tPOW
        else
          if c == '=' then
            self.lex_state = LexState::EXPR_BEG
            self.yacc_value = Token.new("*")
            return Tokens.tOP_ASGN
          end
          src.unread c
          if lex_state.is_argument && space_seen && c !~ /\s/ then
            warnings.warning("`*' interpreted as argument prefix")
            c = Tokens.tSTAR
          elsif (lex_state == LexState::EXPR_BEG ||
                 lex_state == LexState::EXPR_MID) then
            c = Tokens.tSTAR
          else
            c = Tokens.tSTAR2
          end
          self.yacc_value = Token.new("*")
        end

        if (lex_state == LexState::EXPR_FNAME ||
            lex_state == LexState::EXPR_DOT) then
          self.lex_state = LexState::EXPR_ARG
        else
          self.lex_state = LexState::EXPR_BEG
        end

        return c
      when '!' then
        self.lex_state = LexState::EXPR_BEG
        if ((c = src.read) == '=') then
          self.yacc_value = Token.new("!=")
          return Tokens.tNEQ
        end
        if (c == '~') then
          self.yacc_value = Token.new("!~")
          return Tokens.tNMATCH
        end
        src.unread(c)
        self.yacc_value = Token.new("!")
        return Tokens.tBANG
      when '=' then
        # documentation nodes
#         if (src.was_begin_of_line) then
#           equal_label = is_next_no_case("begin")
#           unless equal_label.nil? then
#             token_buffer.clear
#             token_buffer.append(equal_label)
#             c = src.read

#             if c =~ /\s/ then
#               # In case last next was the newline.
#               src.unread(c)
#               loop do
#                 c = src.read
#                 token_buffer.append(c)

#                 # If a line is followed by a blank line put it back.
#                 while c == "\n"
#                   c = src.read
#                   token_buffer.append(c)
#                 end

#                 if (c == EOF) then
#                   raise SyntaxException("embedded document meets end of file")
#                 end

#                 next unless c == '='

#                 if (src.was_begin_of_line && (equal_label = is_next_no_case("end")) != null) then
#                   token_buffer.append(equal_label)
#                   token_buffer.append(src.read_line)
#                   src.unread("\n")
#                   break
#                 end
#               end

#               parser_support.result.add_comment(Node.comment(token_buffer.to_s))
#               next
#             end
#             src.unread(c)
#           end
#         end

        if (lex_state == LexState::EXPR_FNAME || lex_state == LexState::EXPR_DOT) then
          self.lex_state = LexState::EXPR_ARG
        else
          self.lex_state = LexState::EXPR_BEG
        end

        c = src.read
        if (c == '=') then
          c = src.read
          if (c == '=') then
            self.yacc_value = Token.new("===")
            return Tokens.tEQQ
          end
          src.unread(c)
          self.yacc_value = Token.new("==")
          return Tokens.tEQ
        end
        if (c == '~') then
          self.yacc_value = Token.new("=~")
          return Tokens.tMATCH
        elsif (c == '>') then
          self.yacc_value = Token.new("=>")
          return Tokens.tASSOC
        end
        src.unread(c)
        self.yacc_value = Token.new("=")
        return '='
      when '<' then
        c = src.read
        if (c == '<' &&
            lex_state != LexState::EXPR_END &&
            lex_state != LexState::EXPR_DOT &&
            lex_state != LexState::EXPR_ENDARG &&
            lex_state != LexState::EXPR_CLASS &&
            (!lex_state.is_argument || space_seen)) then
          tok = here_document_identifier
          return tok unless tok == 0
        end
        if (lex_state == LexState::EXPR_FNAME ||
            lex_state == LexState::EXPR_DOT) then
          self.lex_state = LexState::EXPR_ARG
        else
          self.lex_state = LexState::EXPR_BEG
        end
        if (c == '=') then
          if ((c = src.read) == '>') then
            self.yacc_value = Token.new("<=>")
            return Tokens.tCMP
          end
          src.unread c
          self.yacc_value = Token.new("<=")
          return Tokens.tLEQ
        end
        if (c == '<') then
          if ((c = src.read) == '=') then
            self.lex_state = LexState::EXPR_BEG
            self.yacc_value = Token.new("\<\<")
            return Tokens.tOP_ASGN
          end
          src.unread(c)
          self.yacc_value = Token.new("<<")
          return Tokens.tLSHFT
        end
        self.yacc_value = Token.new("<")
        src.unread(c)
        return Tokens.tLT
      when '>' then
        if (lex_state == LexState::EXPR_FNAME ||
            lex_state == LexState::EXPR_DOT) then
          self.lex_state = LexState::EXPR_ARG
        else
          self.lex_state = LexState::EXPR_BEG
        end

        if ((c = src.read) == '=') then
          self.yacc_value = Token.new(">=")
          return Tokens.tGEQ
        end
        if (c == '>') then
          if ((c = src.read) == '=') then
            self.lex_state = LexState::EXPR_BEG
            self.yacc_value = Token.new(">>")
            return Tokens.tOP_ASGN
          end
          src.unread c
          self.yacc_value = Token.new(">>")
          return Tokens.tRSHFT
        end
        src.unread c
        self.yacc_value = Token.new(">")
        return Tokens.tGT
      when '"' then
        self.lex_strterm = s(:strterm, STR_DQUOTE, '"', "\0") # TODO: question this
        self.yacc_value = Token.new("\"")
        return Tokens.tSTRING_BEG
      when '`' then
        self.yacc_value = Token.new("`")
        if (lex_state == LexState::EXPR_FNAME) then
          self.lex_state = LexState::EXPR_END
          return Tokens.tBACK_REF2
        end
        if (lex_state == LexState::EXPR_DOT) then
          if (command_state) then
            self.lex_state = LexState::EXPR_CMDARG
          else
            self.lex_state = LexState::EXPR_ARG
          end
          return Tokens.tBACK_REF2
        end
        self.lex_strterm = s(:strterm, STR_XQUOTE, '`', "\0")
        return Tokens.tXSTRING_BEG
      when '\'' then
        self.lex_strterm = s(:strterm, STR_SQUOTE, '\'', "\0")
        self.yacc_value = Token.new("'")
        return Tokens.tSTRING_BEG
      when '?' then
        if (lex_state == LexState::EXPR_END ||
            lex_state == LexState::EXPR_ENDARG) then
          self.lex_state = LexState::EXPR_BEG
          self.yacc_value = Token.new("?")
          return '?'
        end

        c = src.read

        if (c == EOF) then
          raise SyntaxException("incomplete character syntax")
        end

        if c =~ /\s/ then
          if (!lex_state.is_argument) then
            c2 = 0
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
                 end

            if (c2 != 0) then
              warnings.warn("invalid character syntax; use ?\\" + c2)
            end
          end
          src.unread c
          self.lex_state = LexState::EXPR_BEG
          self.yacc_value = Token.new("?")
          return '?'
#         elsif (ismbchar(c)) then
#           rb_warn("multibyte character literal not supported yet; use ?\\" + c)
#           support.unread c
#           self.lex_state = LexState::EXPR_BEG
#           return '?'
        elsif c =~ /\w/ && ! src.peek("\n") && is_next_identchar then
          src.unread c
          self.lex_state = LexState::EXPR_BEG
          self.yacc_value = Token.new("?")
          return '?'
        elsif (c == "\\") then
          c = src.read_escape
        end
        c &= 0xff
        self.lex_state = LexState::EXPR_END
        self.yacc_value = c.to_i
        return Tokens.tINTEGER
      when '&' then
        if ((c = src.read) == '&') then
          self.lex_state = LexState::EXPR_BEG
          if ((c = src.read) == '=') then
            self.yacc_value = Token.new("&&")
            self.lex_state = LexState::EXPR_BEG
            return Tokens.tOP_ASGN
          end
          src.unread c
          self.yacc_value = Token.new("&&")
          return Tokens.tANDOP
        elsif (c == '=') then
          self.yacc_value = Token.new("&")
          self.lex_state = LexState::EXPR_BEG
          return Tokens.tOP_ASGN
        end

        src.unread c

        if lex_state.is_argument && space_seen && c !~ /\s/ then
          warnings.warning("`&' interpreted as argument prefix")
          c = Tokens.tAMPER
        elsif (lex_state == LexState::EXPR_BEG || lex_state == LexState::EXPR_MID) then
          c = Tokens.tAMPER
        else
          c = Tokens.tAMPER2
        end

        if (lex_state == LexState::EXPR_FNAME ||
            lex_state == LexState::EXPR_DOT) then
          self.lex_state = LexState::EXPR_ARG
        else
          self.lex_state = LexState::EXPR_BEG
        end
        self.yacc_value = Token.new("&")
        return c
      when '|' then
        if ((c = src.read) == '|') then
          self.lex_state = LexState::EXPR_BEG
          if ((c = src.read) == '=') then
            self.lex_state = LexState::EXPR_BEG
            self.yacc_value = Token.new("||")
            return Tokens.tOP_ASGN
          end
          src.unread c
          self.yacc_value = Token.new("||")
          return Tokens.tOROP
        end
        if (c == '=') then
          self.lex_state = LexState::EXPR_BEG
          self.yacc_value = Token.new("|")
          return Tokens.tOP_ASGN
        end
        if (lex_state == LexState::EXPR_FNAME ||
            lex_state == LexState::EXPR_DOT) then
          self.lex_state = LexState::EXPR_ARG
        else
          self.lex_state = LexState::EXPR_BEG
        end
        src.unread c
        self.yacc_value = Token.new("|")
        return Tokens.tPIPE
      when '+' then
        c = src.read
        if (lex_state == LexState::EXPR_FNAME ||
            lex_state == LexState::EXPR_DOT) then
          self.lex_state = LexState::EXPR_ARG
          if (c == '@') then
            self.yacc_value = Token.new("+@")
            return Tokens.tUPLUS
          end
          src.unread c
          self.yacc_value = Token.new("+")
          return Tokens.tPLUS
        end

        if (c == '=') then
          self.lex_state = LexState::EXPR_BEG
          self.yacc_value = Token.new("+")
          return Tokens.tOP_ASGN
        end

        if (lex_state == LexState::EXPR_BEG || lex_state == LexState::EXPR_MID ||
            (lex_state.is_argument && space_seen && c !~ /\s/)) then
          arg_ambiguous if lex_state.is_argument
          self.lex_state = LexState::EXPR_BEG
          src.unread c
          if c =~ /\d/ then
            c = '+'
            return parse_number(c)
          end
          self.yacc_value = Token.new("+")
          return Tokens.tUPLUS
        end
        self.lex_state = LexState::EXPR_BEG
        src.unread c
        self.yacc_value = Token.new("+")
        return Tokens.tPLUS
      when '-' then
        c = src.read
        if (lex_state == LexState::EXPR_FNAME || lex_state == LexState::EXPR_DOT) then
          self.lex_state = LexState::EXPR_ARG
          if (c == '@') then
            self.yacc_value = Token.new("-@")
            return Tokens.tUMINUS
          end
          src.unread c
          self.yacc_value = Token.new("-")
          return Tokens.tMINUS
        end
        if (c == '=') then
          self.lex_state = LexState::EXPR_BEG
          self.yacc_value = Token.new("-")
          return Tokens.tOP_ASGN
        end
        if (lex_state == LexState::EXPR_BEG || lex_state == LexState::EXPR_MID ||
            (lex_state.is_argument && space_seen && c !~ /\s/)) then
          arg_ambiguous if lex_state.is_argument
          self.lex_state = LexState::EXPR_BEG
          src.unread c
          self.yacc_value = Token.new("-")
          if c =~ /\d/ then
            return Tokens.tUMINUS_NUM
          end
          return Tokens.tUMINUS
        end
        self.lex_state = LexState::EXPR_BEG
        src.unread c
        self.yacc_value = Token.new("-")
        return Tokens.tMINUS
      when '.' then
        self.lex_state = LexState::EXPR_BEG
        if ((c = src.read) == '.') then
          if ((c = src.read) == '.') then
            self.yacc_value = Token.new("...")
            return Tokens.tDOT3
          end
          src.unread c
          self.yacc_value = Token.new("..")
          return Tokens.tDOT2
        end
        src.unread c
        if c =~ /\d/ then
          raise SyntaxException("no .<digit> floating literal anymore put 0 before dot")
        end
        self.lex_state = LexState::EXPR_DOT
        self.yacc_value = Token.new(".")
        return Tokens.tDOT
      when /[0-9]/ then
        return parse_number(c)
      when ')' then # REFACTOR: omg this is lame... next 3 are all the same
        cond.lexpop
        cmdarg.lexpop
        self.lex_state = LexState::EXPR_END
        self.yacc_value = Token.new(")")
        return Tokens.tRPAREN
      when ']' then
        cond.lexpop
        cmdarg.lexpop
        self.lex_state = LexState::EXPR_END
        self.yacc_value = Token.new("]")
        return Tokens.tRBRACK
      when '}' then
        cond.lexpop
        cmdarg.lexpop
        self.lex_state = LexState::EXPR_END
        self.yacc_value = Token.new("end")
        return Tokens.tRCURLY
      when ':' then
        c = src.read
        if (c == ':') then
          if (lex_state == LexState::EXPR_BEG ||
              lex_state == LexState::EXPR_MID ||
              lex_state == LexState::EXPR_CLASS ||
              (lex_state.is_argument && space_seen)) then
            self.lex_state = LexState::EXPR_BEG
            self.yacc_value = Token.new("::")
            return Tokens.tCOLON3
          end
          self.lex_state = LexState::EXPR_DOT
          self.yacc_value = Token.new(":")
          return Tokens.tCOLON2
        end
        if (lex_state == LexState::EXPR_END ||
            lex_state == LexState::EXPR_ENDARG || c =~ /\s/) then
          src.unread c
          self.lex_state = LexState::EXPR_BEG
          self.yacc_value = Token.new(":")
          return ':'
        end
        case c
        when '\'' then
          self.lex_strterm = s(:strterm, STR_SSYM, c, "\0")
        when '"' then
          self.lex_strterm = s(:strterm, STR_DSYM, c, "\0")
        else
          src.unread c
        end
        self.lex_state = LexState::EXPR_FNAME
        self.yacc_value = Token.new(":")
        return Tokens.tSYMBEG
      when '/' then
        if (lex_state == LexState::EXPR_BEG ||
            lex_state == LexState::EXPR_MID) then
          self.lex_strterm = s(:strterm, STR_REGEXP, '/', "\0")
          self.yacc_value = Token.new("/")
          return Tokens.tREGEXP_BEG
        end

        if ((c = src.read) == '=') then
          self.yacc_value = Token.new("/")
          self.lex_state = LexState::EXPR_BEG
          return Tokens.tOP_ASGN
        end
        src.unread c
        if (lex_state.is_argument && space_seen) then
          unless c =~ /\s/ then
            arg_ambiguous
            self.lex_strterm = s(:strterm, STR_REGEXP, '/', "\0")
            self.yacc_value = Token.new("/")
            return Tokens.tREGEXP_BEG
          end
        end
        if (lex_state == LexState::EXPR_FNAME ||
            lex_state == LexState::EXPR_DOT) then
          self.lex_state = LexState::EXPR_ARG
        else
          self.lex_state = LexState::EXPR_BEG
        end
        self.yacc_value = Token.new("/")
        return Tokens.tDIVIDE
      when '^' then
        if ((c = src.read) == '=') then
          self.lex_state = LexState::EXPR_BEG
          self.yacc_value = Token.new("^")
          return Tokens.tOP_ASGN
        end
        if (lex_state == LexState::EXPR_FNAME ||
            self.lex_state == LexState::EXPR_DOT) then
          self.lex_state = LexState::EXPR_ARG
        else
          self.lex_state = LexState::EXPR_BEG
        end
        src.unread c
        self.yacc_value = Token.new("^")
        return Tokens.tCARET
      when ';' then
        self.command_start = true
      when ',' then
        self.lex_state = LexState::EXPR_BEG
        self.yacc_value = Token.new(",")
        return c
      when '~' then
        if (lex_state == LexState::EXPR_FNAME ||
            lex_state == LexState::EXPR_DOT) then
          if ((c = src.read) != '@') then
            src.unread c
          end
        end
        if (lex_state == LexState::EXPR_FNAME ||
            lex_state == LexState::EXPR_DOT) then
          self.lex_state = LexState::EXPR_ARG
        else
          self.lex_state = LexState::EXPR_BEG
        end
        self.yacc_value = Token.new("~")
        return Tokens.tTILDE
      when '(' then
        c = Tokens.tLPAREN2
        self.command_start = true
        if lex_state == LexState::EXPR_BEG || lex_state == LexState::EXPR_MID then
          c = Tokens.tLPAREN
        elsif space_seen then
          if lex_state == LexState::EXPR_CMDARG then
            c = Tokens.tLPAREN_ARG
          elsif lex_state == LexState::EXPR_ARG then
            warnings.warn("don't put space before argument parentheses")
            c = Tokens.tLPAREN2
          end
        end
        cond.push false
        cmdarg.push false
        self.lex_state = LexState::EXPR_BEG
        self.yacc_value = Token.new("(")
        return c
      when '[' then
        if (lex_state == LexState::EXPR_FNAME ||
            lex_state == LexState::EXPR_DOT) then
          self.lex_state = LexState::EXPR_ARG
          if ((c = src.read) == ']') then
            if (src.peek('=')) then
              c = src.read
              self.yacc_value = Token.new("[]=")
              return Tokens.tASET
            end
            self.yacc_value = Token.new("[]")
            return Tokens.tAREF
          end
          src.unread c
          self.yacc_value = Token.new("[")
          return '['
        elsif (lex_state == LexState::EXPR_BEG ||
               lex_state == LexState::EXPR_MID) then
          c = Tokens.tLBRACK
        elsif (lex_state.is_argument && space_seen) then
          c = Tokens.tLBRACK
        end
        self.lex_state = LexState::EXPR_BEG
        cond.push false
        cmdarg.push false
        self.yacc_value = Token.new("[")
        return c
      when '{' then
        c = Tokens.tLCURLY

        if (lex_state.is_argument || lex_state == LexState::EXPR_END) then
          c = Tokens.tLCURLY      #  block (primary)
        elsif (lex_state == LexState::EXPR_ENDARG) then
          c = Tokens.tLBRACE_ARG  #  block (expr)
        else
          c = Tokens.tLBRACE      #  hash
        end
        cond.push false
        cmdarg.push false
        self.lex_state = LexState::EXPR_BEG
        self.yacc_value = Token.new("{")
        return c
      when "\\" then
        c = src.read
        if (c == "\n") then
          space_seen = true
          next #  skip \\n
        end
        src.unread c
        self.yacc_value = Token.new("\\")
        return "\\"
      when '%' then
        if (lex_state == LexState::EXPR_BEG ||
            lex_state == LexState::EXPR_MID) then
          return parse_quote(src.read)
        end

        c = src.read
        if c == '=' then
          self.lex_state = LexState::EXPR_BEG
          self.yacc_value = Token.new("%")
          return Tokens.tOP_ASGN
        end

        return parse_quote(c) if lex_state.is_argument && space_seen && c !~ /\s/

        self.lex_state = case lex_state
                         when LexState::EXPR_FNAME, LexState::EXPR_DOT then
                           LexState::EXPR_ARG
                         else
                           LexState::EXPR_BEG
                         end

        src.unread c
        self.yacc_value = Token.new("%")

        return Tokens.tPERCENT
      when '$' then
        last_state = lex_state
        self.lex_state = LexState::EXPR_END
        token_buffer.clear
        c = src.read
        case c
        when '_' then #  $_: last read line string
            c = src.read
          if (is_identifier_char(c)) then
            token_buffer.append('$')
            token_buffer.append('_')
            break
          end
          src.unread c
          c = '_'
        when /[~*$?!@\/\\;,.=:<>\"]/ then
          token_buffer.append('$')
          token_buffer.append(c)
          self.yacc_value = Token.new(token_buffer.to_s)
          return Tokens.tGVAR
        when '-' then
            token_buffer.append '$'
          token_buffer.append c
          c = src.read
          if (is_identifier_char(c)) then
            token_buffer.append c
          else
            src.unread c
          end
          self.yacc_value = Token.new(token_buffer.to_s)
          #  xxx shouldn't check if valid option variable
          return Tokens.tGVAR
        when /[\&\`\'\+]/ then
          # Explicit reference to these vars as symbols...
          if (last_state == LexState::EXPR_FNAME) then
            token_buffer.append '$'
            token_buffer.append c
            self.yacc_value = Token.new(token_buffer.to_s)
            return Tokens.tGVAR
          end

          self.yacc_value = s(:back_ref, c.to_sym)
          return Tokens.tBACK_REF
        when /[1-9]/ then
          token_buffer.append('$');
          begin
            token_buffer.append(c);
            c = src.read;
          end while c =~ /\d/
          src.unread c
          if (last_state == LexState::EXPR_FNAME) then
            self.yacc_value = Token.new(token_buffer.to_s);
            return Tokens.tGVAR;
          else
            self.yacc_value = s(:nth_ref, token_buffer.to_s[1..-1].to_i)
            return Tokens.tNTH_REF;
          end
        when '0' then
          token_buffer.append('$');
        else
          if (!is_identifier_char(c)) then
            src.unread c
            self.yacc_value = Token.new("$");
            return '$';
          end
          token_buffer.append('$');
        end
      when '@' then
        c = src.read;
        token_buffer.clear
        token_buffer.append('@');
        if (c == '@') then
            token_buffer.append('@');
            c = src.read;
        end
        if c =~ /\d/ then
          if (token_buffer.length == 1) then
            raise SyntaxException("`@" + c + "' is not allowed as an instance variable name");
          else
            raise SyntaxException("`@@" + c + "' is not allowed as a class variable name");
          end
        end
        if (!is_identifier_char(c)) then
          src.unread c
          self.yacc_value = Token.new("@");
          return '@';
        end
      when '_' then
        if (src.was_begin_of_line && src.match_string("_END__\n", false)) then
          parser_support.result.set_end_seen(true);
          return 0;
        end
        token_buffer.clear
      else
        unless is_identifier_char c then
          raise SyntaxException("Invalid char `\\#{c}' in expression")
        end
        token_buffer.clear
      end

      begin
        token_buffer.append(c);
#         if (ismbchar(c)) then
#           len = mbclen(c) - 1
#           (0..len).each do
#             c = src.read;
#             token_buffer.append(c);
#           end
#         end
        c = src.read;
      end while is_identifier_char c

      peek = src.read;
      if c =~ /\!|\?/ &&
          is_identifier_char(token_buffer.char_at(0)) &&
          peek != '=' then
        src.unread(peek);
        token_buffer.append(c);
      else
        src.unread(peek);
        src.unread c
      end

      result = 0
      last_state = lex_state

      case token_buffer.char_at 0
      when '$' then
        self.lex_state = LexState::EXPR_END
        result = Tokens.tGVAR
      when '@' then
        self.lex_state = LexState::EXPR_END
        if token_buffer.char_at(1) == '@' then
          result = Tokens.tCVAR
        else
          result = Tokens.tIVAR
        end
      else
        last = token_buffer.char_at(-1)
        if (last == '!' || last == '?') then
          result = Tokens.tFID;
        else
          if (lex_state == LexState::EXPR_FNAME) then
            if (c = src.read) == '=' then
              c2 = src.read;

              if c2 != '~' && c2 != '>' && (c2 != '=' || (c2 == "\n" && src.peek('>'))) then
                result = Tokens.tIDENTIFIER;
                token_buffer.append(c);
                src.unread(c2);
              else
                src.unread(c2);
                src.unread c
              end
            else
              src.unread c
            end
          end
          if result == 0 && token_buffer.char_at(0) =~ /[A-Z]/ then
            result = Tokens.tCONSTANT;
          else
            result = Tokens.tIDENTIFIER;
          end
        end

        unless lex_state == LexState::EXPR_DOT then
          # See if it is a reserved word.
          keyword = Keyword.keyword(token_buffer.to_s, token_buffer.length);

          unless keyword.nil? then
            state = lex_state
            self.lex_state = keyword.state

            if state.is_expr_fname then
              self.yacc_value = Token.new(keyword.name);
            else
              self.yacc_value = Token.new(token_buffer.to_s);
            end

            if keyword.id0 == :kDO then # HACK Tokens.kDO then
              return Tokens.kDO_COND if cond.is_in_state
              return Tokens.kDO_BLOCK if cmdarg.is_in_state &&
                state != LexState::EXPR_CMDARG
              return Tokens.kDO_BLOCK if state == LexState::EXPR_ENDARG
              return Tokens.kDO
            end
            
            return keyword.id0 if state == LexState::EXPR_BEG

            self.lex_state = LexState::EXPR_BEG unless keyword.id0 == keyword.id1

            return keyword.id1;
          end
        end

        if (lex_state == LexState::EXPR_BEG ||
            lex_state == LexState::EXPR_MID ||
            lex_state == LexState::EXPR_DOT ||
            lex_state == LexState::EXPR_ARG ||
            lex_state == LexState::EXPR_CMDARG) then
          if command_state then
            self.lex_state = LexState::EXPR_CMDARG;
          else
            self.lex_state = LexState::EXPR_ARG;
          end
        else
          self.lex_state = LexState::EXPR_END;
        end
      end

      temp_val = token_buffer.to_s

      # Lame: parsing logic made it into lexer in ruby...So we
      # are emulating
      # FIXME:  I believe this is much simpler now...
# HACK
#      scope = parser_support.current_scope
#       if (IdUtil.var_type(temp_val) == IdUtil.LOCAL_VAR &&
#           last_state != LexState::EXPR_DOT &&
#           (BlockStaticScope === scope && (scope.is_defined(temp_val) >= 0)) ||
#           (scope.local_scope.is_defined(temp_val) >= 0)) then
#         self.lex_state = LexState::EXPR_END
#       end

      self.yacc_value = Token.new(temp_val);

      return result;
    end
  end

  ##
  #  Parse a number from the input stream.
  #
  # @param c The first character of the number.
  # @return A int constant wich represents a token.

  def parse_number c
    self.lex_state = LexState::EXPR_END

    token_buffer.clear

    if c == '-' then
      token_buffer.append c
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

        if is_hex_char c then
          loop do
            if c == '_' then
              break unless nondigit == "\0"
              nondigit = c
            elsif is_hex_char c then
              nondigit = "\0"
              token_buffer.append c
            else
              break
            end
            c = src.read
          end
        end

        src.unread c

        if token_buffer.length == start_len then
          raise SyntaxException("Hexadecimal number without hex-digits.")
        elsif nondigit != "\0" then
          raise SyntaxException("Trailing '_' in number.")
        end
        self.yacc_value = token_buffer.to_s.to_i(16)
        return Tokens.tINTEGER
      when /b/i # binary
        c = src.read
        if c == '0' or c == '1' then
          loop do
            if c == '_' then
              break if nondigit != "\0"
              nondigit = c
            elsif c == '0' or c == '1' then
              nondigit = "\0"
              token_buffer.append c
            else
              break
            end
            c = src.read
          end
        end

        src.unread c

        if token_buffer.length == start_len then
          raise SyntaxException("Binary number without digits.")
        elsif nondigit != "\0" then
          raise SyntaxException("Trailing '_' in number.")
        end
        self.yacc_value = token_buffer.to_s.to_i(2)
        return Tokens.tINTEGER
      when /d/i then # decimal
        c = src.read
        if c =~ /\d/ then
          loop do
            if c == '_' then
              break if nondigit != "\0"
              nondigit = c
            elsif c =~ /\d/ then
              nondigit = "\0"
              token_buffer.append c
            else
              break
            end
            c = src.read
          end
        end

        src.unread c

        if token_buffer.length == start_len then
          raise SyntaxException("Binary number without digits.")
        elsif nondigit != "\0" then
          raise SyntaxException("Trailing '_' in number.")
        end

        self.yacc_value = token_buffer.to_s.to_i(10)
        return Tokens.tINTEGER
      when /[0-7_]/ then # octal
        loop do
          if (c == '_') then
            break if (nondigit != "\0")
            nondigit = c
          elsif (c >= '0' && c <= '7') then
            nondigit = "\0"
            token_buffer.append c
          else
            break
          end
          c = src.read
        end
        if token_buffer.length > start_len then
          src.unread c

          if nondigit != "\0" then
            raise SyntaxException("Trailing '_' in number.")
          end

          self.yacc_value = token_buffer.to_s.to_i(8)
          return Tokens.tINTEGER
        end
      when /[89]/ then
          raise SyntaxException("Illegal octal digit.")
      when /[\.eE]/ then
        token_buffer.append '0'
      else
        src.unread c
        self.yacc_value = 0
        return Tokens.tINTEGER
      end
    end

    seen_point = false
    seen_e = false

    loop do
      case c
      when /\d/ then
        nondigit = "\0"
        token_buffer.append c
      when '.' then
        if (nondigit != "\0") then
          src.unread c
          raise SyntaxException("Trailing '_' in number.")
        elsif seen_point or seen_e then
          src.unread c
          return number_token(token_buffer.to_s, true, nondigit)
        else
          c2 = src.read
          unless c2 =~ /\d/ then
            src.unread c2
            src.unread '.'
            if (c == '_') then
              # Enebo:  c can never be antrhign but '.'
              # Why did I put this here?
            else
              self.yacc_value = token_buffer.to_s.to_i(10)
              return Tokens.tINTEGER
            end
          else
            token_buffer.append '.'
            token_buffer.append c2
            seen_point = true
            nondigit = "\0"
          end
        end
      when /e/i then
        if (nondigit != "\0") then
          raise SyntaxException("Trailing '_' in number.")
        elsif (seen_e) then
          src.unread c
          return number_token(token_buffer.to_s, true, nondigit)
        else
          token_buffer.append c
          seen_e = true
          nondigit = c
          c = src.read
          if c == '-' or c == '+' then
            token_buffer.append c
            nondigit = c
          else
            src.unread c
          end
        end
      when '_' then #  '_' in number just ignored
        if nondigit != "\0" then
          raise SyntaxException("Trailing '_' in number.")
        end
        nondigit = c
      else
        src.unread c
        r = number_token(token_buffer.to_s, seen_e || seen_point, nondigit)
        return r
      end
      c = src.read
    end
  end

  # TODO: remove me
  def number_token(number, is_float, nondigit)
    if (nondigit != "\0") then
      raise SyntaxException("Trailing '_' in number.")
    end

    if (is_float) then
      d = number.to_f
      self.yacc_value = d
      return Tokens.tFLOAT
    end

    self.yacc_value = number.to_i
    return Tokens.tINTEGER
  end
end
