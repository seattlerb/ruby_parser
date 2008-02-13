require 'stringio'
require 'racc/parser'
require 'sexp'
require 'strscan'

class StringScanner
  def lineno
    string[0..pos].split(/\n/).size
  end

  def current_line # HAHA fuck you (HACK)
    string[0..pos][/\A.*__LINE__/m].split(/\n/).size
  end

#   if ENV['TALLY'] then
#     alias :old_getch :getch
#     def getch
#       warn({:getch => caller[0]}.inspect)
#       old_getch
#     end
#   end

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

class RubyParser < Racc::Parser
  VERSION = '1.0.0'

  attr_accessor :lexer, :in_def, :in_single, :file
  attr_reader :env, :comments

  def initialize
    super
    self.lexer = RubyLexer.new
    self.in_def = false
    self.in_single = 0
    @env = Environment.new
    @comments = []
  end

  def parse(str, file = "(string)")
    raise "bad val: #{str.inspect}" unless String === str

    self.file = file
    self.lexer.src = str

    @yydebug = ENV.has_key? 'DEBUG'

    do_parse
  end

  def do_parse
    _racc_do_parse_rb(_racc_setup, false)
  end

  def next_token
    if self.lexer.advance then
      [self.lexer.token, self.lexer.yacc_value]
    else
      return [false, '$end']
    end
  end

  def assignable(lhs, value = nil)
    id = lhs.to_sym
    id = id.to_sym if Token === id

    raise SyntaxError, "Can't change the value of #{id}" if
      id.to_s =~ /^(?:self|nil|true|false|__LINE__|__FILE__)$/

    result = case id.to_s
             when /^@@/ then
               asgn = in_def || in_single > 0
               s((asgn ? :cvasgn : :cvdecl), id)
             when /^@/ then
               s(:iasgn, id)
             when /^\$/ then
               s(:gasgn, id)
             when /^[A-Z]/ then
               s(:cdecl, id)
             else
               case self.env[id]
               when :lvar then
                 s(:lasgn, id)
               when :dvar, nil then
                 if self.env.current[id] == :dvar then
                   s(:dasgn_curr, id)
                 elsif self.env[id] == :dvar then
                   self.env.use(id)
                   s(:dasgn, id)
                 elsif ! self.env.dynamic? then
                   s(:lasgn, id)
                 else
                   s(:dasgn_curr, id)
                 end
               else
                 raise "wtf? unknown type: #{self.env[id]}"
               end
             end

    self.env[id] = (self.env.dynamic? ? :dvar : :lvar) unless self.env[id]

    result << value if value

    return result
  end

  def arg_add(node1, node2)
    return s(:array, node2) unless node1
    return node1 << node2 if node1[0] == :array
    return s(:argspush, node1, node2)
  end

  def node_assign(lhs, rhs)
    return nil unless lhs

    rhs = value_expr rhs

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
    id = id.to_sym      if Token  === id # HACK
    id = id.last.to_sym if Sexp   === id # HACK
    id = id.to_sym      if String === id # HACK

    return s(:self)                  if id == :self
    return s(:nil)                   if id == :nil
    return s(:true)                  if id == :true
    return s(:false)                 if id == :false
    return s(:str, self.file)        if id == :"__FILE__"
    return s(:lit, lexer.current_line) if id == :"__LINE__"

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
               type = env[id]
               if type then
                 s(type, id)
               elsif env.dynamic? and :dvar == env[id] then
                 s(:dvar, id)
               else
                 s(:vcall, id)
               end
             end

    return result if result

    raise "identifier #{id.inspect} is not valid"
  end

  def block_append(head, tail, strip_tail_block=false)
    return head unless tail
    return tail unless head

    case head[0]
    when :lit, :str then
      return tail
    end

    head = remove_begin(head)
    head = s(:block, head) unless head[0] == :block

    if strip_tail_block and Sexp === tail and tail[0] == :block then
      head.push(*tail.values)
    else
      head << tail
    end
  end

  def new_yield(node)
    if node then
      raise SyntaxError, "Block argument should not be given." if
        node.node_type == :block_pass

      node = node.last if node.node_type == :array and node.size == 2
    end

    return s(:yield, node)
  end

  def logop(type, left, right)
    left = value_expr left

    if left and left[0] == type and not left.paren then
      node, second = left, nil

      while (second = node[2]) && second[0] == type and not second.paren do
        node = second
      end

      node[2] = s(type, second, right)

      return left
    end

    return s(type, left, right)
  end

  def new_call recv, meth, args = nil # REFACTOR - merge with fcall
    if args && args[0] == :block_pass then
      new_args = args.array(true) || args.argscat(true) || args.splat(true)
      call = s(:call, recv, meth)
      call << new_args if new_args
      args << call

      return args
    end
    result = s(:call, recv, meth)
    result << args if args
    result
  end

  def new_fcall meth, args
    if args and args[0] == :block_pass then
      new_args = args.array(true) || args.argscat(true) || args.splat(true)
      call = s(:fcall, meth)
      call << new_args if new_args
      args << call
      return args
    end

    r = s(:fcall, meth)
    r << args if args and args != s(:array)
    r
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
    if lhs then
      case lhs[0]
      when :dregx, :dregx_once then
        return s(:match2, lhs, rhs)
      when :lit then
        return s(:match2, lhs, rhs) if Regexp === lhs.last
      end
    end

    if rhs then
      case rhs[0]
      when :dregx, :dregx_once then
        return s(:match3, rhs, lhs)
      when :lit then
        return s(:match3, rhs, lhs) if Regexp === rhs.last
      end
    end

    return s(:call, lhs, :"=~", s(:array, rhs))
  end

  def cond node
    return nil if node.nil?
    node = value_expr node

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
      return s(:and, cond(node[1]), cond(node[2]))
    when :or then
      return s(:or,  cond(node[1]), cond(node[2]))
    when :dot2 then
      label = "flip#{node.hash}"
      env[label] = self.env.dynamic? ? :dvar : :lvar
      return s(:flip2, node[1], node[2])
    when :dot3 then
      label = "flip#{node.hash}"
      env[label] = self.env.dynamic? ? :dvar : :lvar
      return s(:flip3, node[1], node[2])
    else
      return node
    end
  end

  def append_to_block head, tail # FIX: wtf is this?!? switch to block_append
    return head if tail.nil?
    return tail if head.nil?

    head = s(:block, head) unless head.first == :block
    head << tail
  end

  def new_super args
    if args && args.first == :block_pass then
      t, body, bp = args
      result = s(t, bp, s(:super, body))
    else
      result = s(:super)
      result << args if args and args != s(:array)
    end
    result
  end

  def aryset receiver, index
    s(:attrasgn, receiver, :"[]=", index)
  end

  def arg_concat node1, node2
    return node2.nil? ? node1 : s(:argscat, node1, node2)
  end

  def list_append list, item # TODO: nuke me *sigh*
    return s(:array, item) unless list
    list << item
  end

  def literal_concat head, tail
    return tail unless head
    return head unless tail

    htype, ttype = head[0], tail[0]

    head = s(:dstr, '', head) if htype == :evstr

    case ttype
    when :str then
      if htype == :str
        head[-1] << tail[-1]
      elsif htype == :dstr and head.size == 2 then
        head[-1] << tail[-1]
      else
        head << tail
      end
    when :dstr then
      if htype == :str then
        tail[1] = head[-1] + tail[1]
        head = tail
      else
        tail[0] = :array
        tail[1] = s(:str, tail[1])
        tail.delete_at 1 if tail[1] == s(:str, '')

        head.push(*tail[1..-1])
      end
    when :evstr then
      head[0] = :dstr if htype == :str
      if head.size == 2 and tail[1][0] == :str then
        head[-1] << tail[1][-1]
        head[0] = :str if head.size == 2 # HACK ?
      else
        head.push(tail)
      end
    end

    return head
  end

  def remove_begin node
    node = node[-1] if node and node[0] == :begin and node.size == 2
    node
  end

  def ret_args node
    if node then
      if node[0] == :block_pass then
        raise SyntaxError, "block argument should not be given"
      end

      node = node.last if node[0] == :array && node.size == 2
      node = s(:svalue, node) if node[0] == :splat and not node.paren # HACK matz wraps ONE of the FOUR splats in a newline to distinguish. I use paren for now. ugh
    end

    node
  end

  def value_expr node # HACK
    node = remove_begin node
    node[2] = value_expr(node[2]) if node and node[0] == :if
    node
  end

  def void_stmts node
    return nil unless node
    return node unless node[0] == :block

    node[1..-2] = node[1..-2].map { |n| remove_begin(n) }
    node
  end

  ############################################################
  # HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK

  def dyna_init body, known_vars = []
    var = nil
    vars = self.env.dynamic.keys - known_vars

    vars.each do |id|
      if self.env.used? id then
        var = s(:dasgn_curr, id, var).compact
      end
    end

    self.block_append(var, body, body && body[0] == :block)
  end

  def warning s
    # do nothing for now
  end

  # END HACK
  ############################################################$
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

  def self.hash_keyword(str, len)
    hval = len

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

    case hval
    when 2, 1 then
      hval += asso_values[str[0]]
    else
      hval += asso_values[str[2]]
      hval += asso_values[str[0]]
    end

    hval += asso_values[str[len - 1]]
    return hval
  end

  ##
  # :expr_beg    = ignore newline, +/- is a sign.
  # :expr_end    = newline significant, +/- is a operator.
  # :expr_arg    = newline significant, +/- is a operator.
  # :expr_cmdarg = newline significant, +/- is a operator.
  # :expr_endarg = newline significant, +/- is a operator.
  # :expr_mid    = newline significant, +/- is a operator.
  # :expr_fname  = ignore newline, no reserved words.
  # :expr_dot    = right after . or ::, no reserved words.
  # :expr_class  = immediate after class, no here document.

  def self.keyword(str, len = str.size)
    wordlist = [
                [""], [""], [""], [""], [""], [""],
                ["end",      [:kEND,      :kEND        ], :expr_end   ],
                ["else",     [:kELSE,     :kELSE       ], :expr_beg   ],
                ["case",     [:kCASE,     :kCASE       ], :expr_beg   ],
                ["ensure",   [:kENSURE,   :kENSURE     ], :expr_beg   ],
                ["module",   [:kMODULE,   :kMODULE     ], :expr_beg   ],
                ["elsif",    [:kELSIF,    :kELSIF      ], :expr_beg   ],
                ["def",      [:kDEF,      :kDEF        ], :expr_fname ],
                ["rescue",   [:kRESCUE,   :kRESCUE_MOD ], :expr_mid   ],
                ["not",      [:kNOT,      :kNOT        ], :expr_beg   ],
                ["then",     [:kTHEN,     :kTHEN       ], :expr_beg   ],
                ["yield",    [:kYIELD,    :kYIELD      ], :expr_arg   ],
                ["for",      [:kFOR,      :kFOR        ], :expr_beg   ],
                ["self",     [:kSELF,     :kSELF       ], :expr_end   ],
                ["false",    [:kFALSE,    :kFALSE      ], :expr_end   ],
                ["retry",    [:kRETRY,    :kRETRY      ], :expr_end   ],
                ["return",   [:kRETURN,   :kRETURN     ], :expr_mid   ],
                ["true",     [:kTRUE,     :kTRUE       ], :expr_end   ],
                ["if",       [:kIF,       :kIF_MOD     ], :expr_beg   ],
                ["defined?", [:kDEFINED,  :kDEFINED    ], :expr_arg   ],
                ["super",    [:kSUPER,    :kSUPER      ], :expr_arg   ],
                ["undef",    [:kUNDEF,    :kUNDEF      ], :expr_fname ],
                ["break",    [:kBREAK,    :kBREAK      ], :expr_mid   ],
                ["in",       [:kIN,       :kIN         ], :expr_beg   ],
                ["do",       [:kDO,       :kDO         ], :expr_beg   ],
                ["nil",      [:kNIL,      :kNIL        ], :expr_end   ],
                ["until",    [:kUNTIL,    :kUNTIL_MOD  ], :expr_beg   ],
                ["unless",   [:kUNLESS,   :kUNLESS_MOD ], :expr_beg   ],
                ["or",       [:kOR,       :kOR         ], :expr_beg   ],
                ["next",     [:kNEXT,     :kNEXT       ], :expr_mid   ],
                ["when",     [:kWHEN,     :kWHEN       ], :expr_beg   ],
                ["redo",     [:kREDO,     :kREDO       ], :expr_end   ],
                ["and",      [:kAND,      :kAND        ], :expr_beg   ],
                ["begin",    [:kBEGIN,    :kBEGIN      ], :expr_beg   ],
                ["__LINE__", [:k__LINE__, :k__LINE__   ], :expr_end   ],
                ["class",    [:kCLASS,    :kCLASS      ], :expr_class ],
                ["__FILE__", [:k__FILE__, :k__FILE__   ], :expr_end   ],
                ["END",      [:klEND,     :klEND       ], :expr_end   ],
                ["BEGIN",    [:klBEGIN,   :klBEGIN     ], :expr_end   ],
                ["while",    [:kWHILE,    :kWHILE_MOD  ], :expr_beg   ],
                [""], [""], [""], [""], [""], [""], [""], [""], [""],
                [""],
                ["alias",    [:kALIAS,    :kALIAS      ], :expr_fname ],
               ].map { |args| KWtable.new(*args) }

    if len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH then
      key = hash_keyword(str, len)
      if key <= MAX_HASH_VALUE && key >= 0 then
        s = wordlist[key].name
        return wordlist[key] if str == s
      end
    end

    return nil
  end
end

class Environment
  attr_reader :env, :dyn
  attr_accessor :init

  def initialize dyn = false
    @dyn = []
    @env = []
    @use = []
    @init = false
    self.extend
  end

  def use id
    @env.each_with_index do |env, i|
      if env[id] then
        @use[i][id] = true
      end
    end
  end

  def used? id
    idx = @dyn.index false # REFACTOR
    u = @use[0...idx].reverse.inject { |env, scope| env.merge scope } || {}
    u[id]
  end

  def [] k
    self.all[k]
  end

  def []= k, v
    raise "no" if v == true
    self.current[k] = v
  end

  def all
    idx = @dyn.index false
    @env[0..idx].reverse.inject { |env, scope| env.merge scope }
  end

  def dynamic
    idx = @dyn.index false
    @env[0...idx].reverse.inject { |env, scope| env.merge scope } || {}
  end

  def current
    @env.first
  end

  def dynamic?
    @dyn[0] != false
  end

  def extend dyn = false
    @dyn.unshift dyn
    @env.unshift({})
    @use.unshift({})
   end

  def unextend
    @dyn.shift
    @env.shift
    @use.shift
    raise "You went too far unextending env" if @env.empty?
  end
end

class StackState
  attr_reader :stack

  def inspect
    "StackState(#{@name}, #{@stack.inspect})"
  end

  def initialize(name)
    @name = name
    @stack = [false]
  end

  def pop
    # raise "#{@name} empty" if @stack.size <= 1
    r = @stack.pop
    @stack.push false if @stack.size == 0
    r
  end

  def lexpop
    raise if @stack.size == 0
    a = @stack.pop
    b = @stack.pop
    @stack.push(a || b)
  end

  def push val
    raise if val != true and val != false
    @stack.push val
  end

  def is_in_state
    @stack.last
  end
end

def t str
  Token.new str
end

class Token # TODO: nuke this and use sexps
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

  def == o
    Token === o and self.args == o.args
  end
end

############################################################
# HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK

class Symbol
  def is_argument # TODO: phase this out
    return self == :expr_arg || self == :expr_cmdarg
  end
end

class Sexp
  attr_writer :paren
  attr_accessor :comments

  def paren
    @paren ||= false
  end

  def value
    raise "multi item sexp" if size > 2
    last
  end

  def values
    self[1..-1]
  end

  def node_type
    first
  end
end

# END HACK
############################################################


