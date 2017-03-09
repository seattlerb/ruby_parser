#!/usr/bin/ruby -w

good = false

rules = Hash.new { |h,k| h[k] = [] }
rule = nil
order = []

def munge s
  renames = [
             "'='",             "tEQL",
             "'!'",             "tBANG",
             "'%'",             "tPERCENT",
             "'&'",             "tAMPER2",
             "'('",             "tLPAREN2",
             "')'",             "tRPAREN",
             "'*'",             "tSTAR2",
             "'+'",             "tPLUS",
             "','",             "tCOMMA",
             "'-'",             "tMINUS",
             "'.'",             "tDOT",
             "'/'",             "tDIVIDE",
             "';'",             "tSEMI",
             "':'",             "tCOLON",
             "'<'",             "tLT",
             "'>'",             "tGT",
             "'?'",             "tEH",
             "'['",             "tLBRACK",
             "'\\n'",           "tNL",
             "']'",             "tRBRACK",
             "'^'",             "tCARET",
             "'`'",             "tBACK_REF2",
             "'{'",             "tLCURLY",
             "'|'",             "tPIPE",
             "'}'",             "tRCURLY",
             "'~'",             "tTILDE",
             '"["',             "tLBRACK",

             # 2.0 changes?
             '"<=>"',            "tCMP",
             '"=="',             "tEQ",
             '"==="',            "tEQQ",
             '"!~"',             "tNMATCH",
             '"=~"',             "tMATCH",
             '">="',             "tGEQ",
             '"<="',             "tLEQ",
             '"!="',             "tNEQ",
             '"<<"',             "tLSHFT",
             '">>"',             "tRSHFT",
             '"*"',              "tSTAR",

             '".."',             "tDOT2",

             '"&"',              "tAMPER",
             '"&&"',             "tANDOP",
             '"||"',             "tOROP",

             '"..."',            "tDOT3",
             '"**"',             "tPOW",
             '"unary+"',         "tUPLUS",
             '"unary-"',         "tUMINUS",
             '"[]"',             "tAREF",
             '"[]="',            "tASET",
             '"::"',             "tCOLON2",
             '"{ arg"',          "tLBRACE_ARG",
             '"( arg"',          "tLPAREN_ARG",
             '"("',              "tLPAREN",
             'rparen',           "tRPAREN",
             '"{"',              "tLBRACE",
             '"=>"',             "tASSOC",
             '"->"',             "tLAMBDA",
             '":: at EXPR_BEG"', "tCOLON3",
             '"**arg"',          "tDSTAR",
             '","',              "tCOMMA",

             # other

             'tLBRACK2',        "tLBRACK", # HACK

             "' '",             "tSPACE", # needs to be later to avoid bad hits

             "/* empty */",     "none",
             /^\s*$/,           "none",
             "keyword_BEGIN",   "klBEGIN",
             "keyword_END",     "klEND",
             /keyword_(\w+)/,   proc { "k#{$1.upcase}" },
             /\bk_([a-z_]+)/,   proc { "k#{$1.upcase}" },
             /modifier_(\w+)/,  proc { "k#{$1.upcase}_MOD" },
             "kVARIABLE",       "keyword_variable", # ugh

             /@(\d+)\s+/,       "",
            ]

  renames.each_slice(2) do |(a, b)|
    if Proc === b then
      s.gsub!(a, &b)
    else
      s.gsub!(a, b)
    end
  end

  s.strip
end

ARGF.each_line do |line|
  next unless good or line =~ /^-* ?Grammar|\$accept : /

  case line.strip
  when /^$/ then
  when /^(\d+) (\$?\w+): (.*)/ then    # yacc
    rule = $2
    order << rule unless rules.has_key? rule
    rules[rule] << munge($3)
  when /^(\d+) \s+\| (.*)/ then        # yacc
    rules[rule] << munge($2)
  when /^(\d+) (@\d+): (.*)/ then      # yacc
    rule = $2
    order << rule unless rules.has_key? rule
    rules[rule] << munge($3)
  when /^rule (\d+) (@?\w+):(.*)/ then # racc
    rule = $2
    order << rule unless rules.has_key? rule
    rules[rule] << munge($3)
  when /\$accept/ then                 # byacc?
    good = true
  when /Grammar/ then                  # both
    good = true
  when /^-+ Symbols/ then              # racc
    break
  when /^Terminals/ then               # yacc
    break
  when /^\cL/ then                     # byacc
    break
  else
    warn "unparsed: #{$.}: #{line.chomp}"
  end
end

require 'yaml'

order.each do |k|
  next if k =~ /@/
  puts
  puts "#{k}:"
  puts rules[k].map { |r| "    #{r}" }.join "\n"
end
