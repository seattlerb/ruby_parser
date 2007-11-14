# -*- racc -*-

class RubyParser

token kCLASS kMODULE kDEF kUNDEF kBEGIN kRESCUE kENSURE kEND kIF kUNLESS
      kTHEN kELSIF kELSE kCASE kWHEN kWHILE kUNTIL kFOR kBREAK kNEXT
      kREDO kRETRY kIN kDO kDO_COND kDO_BLOCK kRETURN kYIELD kSUPER
      kSELF kNIL kTRUE kFALSE kAND kOR kNOT kIF_MOD kUNLESS_MOD kWHILE_MOD
      kUNTIL_MOD kRESCUE_MOD kALIAS kDEFINED klBEGIN klEND k__LINE__
      k__FILE__ tIDENTIFIER tFID tGVAR tIVAR tCONSTANT tCVAR tNTH_REF
      tBACK_REF tSTRING_CONTENT tINTEGER tFLOAT tREGEXP_END tUPLUS
      tUMINUS tUMINUS_NUM tPOW tCMP tEQ tEQQ tNEQ tGEQ tLEQ tANDOP
      tOROP tMATCH tNMATCH tDOT tDOT2 tDOT3 tAREF tASET tLSHFT tRSHFT
      tCOLON2 tCOLON3 tOP_ASGN tASSOC tLPAREN tLPAREN2 tRPAREN tLPAREN_ARG
      tLBRACK tRBRACK tLBRACE tLBRACE_ARG tSTAR tSTAR2 tAMPER tAMPER2
      tTILDE tPERCENT tDIVIDE tPLUS tMINUS tLT tGT tPIPE tBANG tCARET
      tLCURLY tRCURLY tBACK_REF2 tSYMBEG tSTRING_BEG tXSTRING_BEG tREGEXP_BEG
      tWORDS_BEG tQWORDS_BEG tSTRING_DBEG tSTRING_DVAR tSTRING_END
      tLAST_TOKEN

preclow
  nonassoc tLOWEST
  nonassoc tLBRACE_ARG
  nonassoc kIF_MOD kUNLESS_MOD kWHILE_MOD kUNTIL_MOD
  left     kOR kAND
  right    kNOT
  nonassoc kDEFINED
  right    '=' tOP_ASGN
  left     kRESCUE_MOD
  right    '?' ':'
  nonassoc tDOT2 tDOT3
  left     tOROP
  left     tANDOP
  nonassoc tCMP tEQ tEQQ tNEQ tMATCH tNMATCH
  left     tGT tGEQ tLT tLEQ
  left     tPIPE tCARET
  left     tAMPER2
  left     tLSHFT tRSHFT
  left     tPLUS tMINUS
  left     tSTAR2 tDIVIDE tPERCENT
  right    tUMINUS_NUM tUMINUS
  right    tPOW
  right    tBANG tTILDE tUPLUS
prechigh

rule

program       : {
                  self.lexer.state = LexState::EXPR_BEG
                  # HACK support.init_top_local_variables
                } compstmt {
# HACK
#                   support.result.ast = support.add_root_node(val[1])
                   result = val[1]
                 }

bodystmt      : compstmt opt_rescue opt_else opt_ensure {
                  body = val[0] == s(:block) ? nil : val[0]

                  if not val[1].nil? then
                    body = s(:rescue, body, val[1]).compact
                    body << val[2] if val[2]
                  elsif not val[2].nil? then
                    warnings.warn("else without rescue is useless")
                    if body.nil? then
                      body = val[2]
                    else
                      body << val[2]
                    end
                  end

                  body = s(:ensure, body, val[3]) if val[3]

                  result = body
                 }

compstmt     : stmts opt_terms {
                 result = val[0]
               }

stmts        : none { result = s(:block) }
             | stmt {
                 result = val[0] # TODO: wrap in newline node
                 # result = s(:newline, result);
               }
             | stmts terms stmt {
                 # result = support.append_to_block(val[0], support.newline_node(val[2]));
                 # HACK result = val[0].Node.args << line(val[2])
                 if val[0] then
                   val[0] = s(:block, val[0]) unless val[0].node_type == :block
                   val[0] << val[2]
                   result = val[0]
                 else
                   result = val[2]
                 end
                 }
             | error stmt {
                 result = val[1];
                 }

stmt          : kALIAS fitem {
                 lexer.state = LexState::EXPR_FNAME
                } fitem {
                  result = s(:alias, val[1], val[3])
                 }
             | kALIAS tGVAR tGVAR {
                  result = s(:valias, val[1].value.to_sym, val[2].value.to_sym)
                 }
             | kALIAS tGVAR tBACK_REF {
                  result = s(:valias, val[1].value, "$" + val[2].getType); #  XXX
                 }
             | kALIAS tGVAR tNTH_REF {
                  yyerror("can't make alias for the number variables");
                 }
             | kUNDEF undef_list {
                  result = val[1];
                 }
             | stmt kIF_MOD expr_value {
                  result = s(:if, cond(val[2]), val[0], nil)
                 }
             | stmt kUNLESS_MOD expr_value {
                  result = s(:if, cond(val[2]), nil, val[0]);
                 }
             | stmt kWHILE_MOD expr_value {
                 block = val[0]
                 block = block.last if block.size == 2
                 block = nil if block == s(:block) # HACK
      
                 result = s(:while, val[2], block, false);
               }
             | stmt kUNTIL_MOD expr_value {
                 block = val[0] # REFACTOR
                 block = block.last if block.size == 2
                 block = nil if block == s(:block) # HACK
                 result = s(:until, val[2], block, false);
               }
             | stmt kRESCUE_MOD stmt {
                  result = s(:rescue, val[0], s(:resbody, nil, val[2]))
                 }
             | klBEGIN {
                 if (support.is_in_def || support.is_in_single > 0) then
                   yyerror("BEGIN in method");
                 end
                 support.push_local_scope;
               } tLCURLY compstmt tRCURLY {
                 # support.result.add_begin_node(s(:pre_exe, support.current_scope, val[3]));
                 # support.pop_local_scope;
                 result = nil; # XXX 0;
               }
             | klEND tLCURLY compstmt tRCURLY {
                 if (support.is_in_def || support.is_in_single > 0) then
                   yyerror("END in method; use at_exit");
                 end
                 result = s(:iter, s(:postexe), nil, val[2])
                 }
             | lhs '=' command_call {
                  result = s(:lasgn, val[0].last.to_sym, val[2])
                 }
             | mlhs '=' command_call {
                  val[0][2] = if val[0][1] then
                                s(:toary, val[2])
                              else
                                s(:array, val[2])
                              end
                  result = val[0];
                 }
             | var_lhs tOP_ASGN command_call {
                  name = val[0].get_name;
                  asgn_op = val[1].value;

                  if asgn_op == "||" then
                    val[0][2] = (val[2]);
                    result = s(:op_asgn, support.gettable2(name), :"||", val[0])
                  elsif asgn_op == "&&" then
                    result = s(:op_asgn, support.gettable2(name), :"&&", val[0])
                  else
                    result = s(:lasgn, support.gettable2(name), s(:call, val[0], asgn_op, val[1]))
                end
                 }
             | primary_value '[' aref_args tRBRACK tOP_ASGN command_call {
                  result = s(:op_asgn1, val[0], val[2], val[4].value.to_sym, val[5]);
                 }
             | primary_value tDOT tIDENTIFIER tOP_ASGN command_call {
                  result = s(:op_asgn, val[0], val[4], val[2].value, val[3].value);
                 }
             | primary_value tDOT tCONSTANT tOP_ASGN command_call {
                  result = s(:op_asgn, val[0], val[4], val[2].value, val[3].value);
                 }
             | primary_value tCOLON2 tIDENTIFIER tOP_ASGN command_call {
                  result = s(:op_asgn, val[0], val[4], val[2].value, val[3].value);
                 }
             | backref tOP_ASGN command_call {
                  support.backref_assign_error(val[0]);
                 }
             | lhs '=' mrhs {
                  result = s(:lasgn, val[0].last.to_sym, s(:svalue, val[2]))
                 }
             | mlhs '=' arg_value {
                 val[0][2] = if val[0][1] then
                               s(:to_ary, val[2])
                             else
                               s(:array, val[2])
                             end
                 result = val[0]
               }
             | mlhs '=' mrhs {
                  result = val[0] << val[2]
                 }
             | expr {
                 result = val[0]
                 }


expr          : command_call
             | expr kAND expr {
                  result = s(:and, val[0], val[2])
                 }
             | expr kOR expr {
                  result = s(:or, val[0], val[2])
                 }
             | kNOT expr {
                  result = s(:not, val[1])
                 }
             | tBANG command_call {
                  result = s(:not, val[1])
                 }
             | arg {
                 result = val[0]
                 }

expr_value    : expr # TODO: value_expr(val[0]) -> value_expr0(remove_begin(node))

command_call : command
             | block_command
             | kRETURN call_args {
                 retval = val[1]
                 retval = retval.last if retval.size == 2
                 result = s(:return, retval)
               }
             | kBREAK call_args {
                 args = val[1]
                 args = args.last if args.size == 2
                 result = s(:break, args)
               }
             | kNEXT call_args {
                 result = s(:next, val[1])
               }

block_command : block_call
              | block_call tDOT operation2 command_args {
                  result = s(:call, val[0], val[2], val[3]);
                }
              | block_call tCOLON2 operation2 command_args {
                  result = s(:call, val[0], val[2], val[3]);
                }

cmd_brace_block : tLBRACE_ARG {
                    support.push_local_scope;
                  } opt_block_var compstmt tRCURLY {
                    result = s(:iter, val[2], val[3])
                    support.pop_local_scope;
                  }

command      : operation command_args = tLOWEST {
                 result = s(:fcall, val[0].value.to_sym, nil, val[1])
               }
             | operation command_args cmd_brace_block {
                 result = s(:fcall, val[0].value.to_sym, val[1], val[2])
               }
             | primary_value tDOT operation2 command_args = tLOWEST {
                 result = s(:call, val[0], val[2], val[3]);
               }
             | primary_value tDOT operation2 command_args cmd_brace_block {
                 result = s(:call, val[0], val[2], val[3], val[4]);
               }
             | primary_value tCOLON2 operation2 command_args = tLOWEST {
                 result = s(:call, val[0], val[2], val[3]);
               }
             | primary_value tCOLON2 operation2 command_args cmd_brace_block {
                 result = s(:call, val[0], val[2], val[3], val[4]);
               }
             | kSUPER command_args {
                 result = support.new_super(val[1], val[0]);
               }
             | kYIELD command_args {
                 result = support.new_yield(val[1]);
               }

mlhs          : mlhs_basic
             | tLPAREN mlhs_entry tRPAREN {
                  result = val[1];
                 }

mlhs_entry    : mlhs_basic
              | tLPAREN mlhs_entry tRPAREN {
                  result = s(:masgn, s(:array, val[1]));
                }

mlhs_basic    : mlhs_head {
                  result = s(:masgn, val[0]);
                }
              | mlhs_head mlhs_item {
                  result = s(:masgn, val[0] << val[1]);
                }
              | mlhs_head tSTAR mlhs_node {
                  result = s(:masgn, val[0], val[2]);
                }
              | mlhs_head tSTAR {
                  result = s(:masgn, val[0], s(:star))
                }
              | tSTAR mlhs_node {
                  result = s(:masgn, nil, val[1]);
                }
              | tSTAR {
                  result = s(:masgn, nil, s(:star))
                }

mlhs_item     : mlhs_node
              | tLPAREN mlhs_entry tRPAREN {
                  result = val[1];
                }

mlhs_head     : mlhs_item ',' {
                  result = s(:array, val[0])
                }
              | mlhs_head mlhs_item ',' {
                  result = val[0] << val[1]
                }

mlhs_node    : variable {
                 result = support.assignable(val[0], nil)
               }
             | primary_value '[' aref_args tRBRACK {
                 result = support.aryset(val[0], val[2]);
               }
             | primary_value tDOT tIDENTIFIER {
                 result = s(:attrasgn, val[0], :"#{val[2].value}=");
               }
             | primary_value tCOLON2 tIDENTIFIER {
                 result = s(:attrasgn, val[0], :"#{val[2].value}=");
               }
             | primary_value tDOT tCONSTANT {
                 result = s(:attrasgn, val[0], :"#{val[2].value}=");
               }
             | primary_value tCOLON2 tCONSTANT {
                 if (support.is_in_def || support.is_in_single > 0) then
                   yyerror("dynamic constant assignment");
                 end

                 result = s(:constdecl, nil,
                            s(:colon2, val[0], val[2].value), nil)
               }
             | tCOLON3 tCONSTANT {
                 if (support.is_in_def || support.is_in_single > 0) then
                   yyerror("dynamic constant assignment");
                 end

                 result = s(:const, nil, s(:colon3, val[1].value.to_sym))
               }
             | backref {
                  support.backref_assign_error(val[0]);
                 }

lhs          : variable {
                  case val[0].first
                  when :gvar then
                    result = s(:gvar, val[0].last)
                  when :lvar then
                    result = s(:lvar, val[0].last)
                  when :ivar then
                    self.env[val[0].last] = true
                    result = s(:ivar, val[0].last.value.to_sym)
                  else
                    self.env[val[0].last] = true
                    result = s(:lvar, val[0].last)
                  end
               }
             | primary_value '[' aref_args tRBRACK {
                  result = support.aryset(val[0], val[2]);
                 }
             | primary_value tDOT tIDENTIFIER {
                 result = s(:attrasgn, val[0], :"#{val[2].value}=");
                 }
             | primary_value tCOLON2 tIDENTIFIER {
                 result = s(:attrasgn, val[0], :"#{val[2].value}=");
                 }
             | primary_value tDOT tCONSTANT {
                 result = s(:attrasgn, val[0], :"#{val[2].value}=");
                 }
             | primary_value tCOLON2 tCONSTANT {
                 if (support.is_in_def || support.is_in_single > 0) then
                   yyerror("dynamic constant assignment");
                 end

                 result = s(:constdecl, nil, s(:colon2, val[0], val[2].value), nil);
                 }
             | tCOLON3 tCONSTANT {
                  if (support.is_in_def || support.is_in_single > 0) then
                    yyerror("dynamic constant assignment");
                  end

                  result = s(:const, nil, s(:colon3, val[1].value.to_sym))
                  }
             | backref {
                   support.backref_assign_error(val[0]);
                 }

cname         : tIDENTIFIER {
                  yyerror("class/module name must be CONSTANT");
                 }
             | tCONSTANT

cpath         : tCOLON3 cname {
                  result = s(:colon3, val[1].value.to_sym)
                 }
             | cname {
                  result = s(:colon2, nil, val[0].value);
                 }
             | primary_value tCOLON2 cname {
                  result = s(:colon2, val[0], val[2].value);
                 }

#  Token:fname - A function name [!nil]
fname         : tIDENTIFIER | tCONSTANT | tFID
              | op {
                  lexer.state = LexState::EXPR_END
                  result = val[0];
                }

              | reswords {
                  lexer.state = LexState::EXPR_END
                  result = $<>1;
                }

fitem         : fname
              | symbol { result = s(:lit, val[0]) } # HACK

undef_list    : fitem {
                  result = s(:undef, val[0])
                 }
              | undef_list ',' {
                  lexer.state = LexState::EXPR_FNAME
                } fitem {
                  item = s(:undef, val[3])
                  if val[0][0] == :block then
                    result << item
                  else
                    result = s(:block, val[0], item)
                  end
                }

#  Token:op - inline operations [!nil]
op           : tPIPE  | tCARET  | tAMPER2 | tCMP    | tEQ      | tEQQ   | tMATCH
             | tGT    | tGEQ    | tLT     | tLEQ    | tLSHFT   | tRSHFT | tPLUS
             | tMINUS | tSTAR2  | tSTAR   | tDIVIDE | tPERCENT | tPOW   | tTILDE
             | tUPLUS | tUMINUS | tAREF   | tASET   | tBACK_REF2

#  Keyword:reswords - reserved words [!nil]
reswords     : k__LINE__ | k__FILE__  | klBEGIN | klEND  | kALIAS  | kAND
             | kBEGIN    | kBREAK     | kCASE   | kCLASS | kDEF    | kDEFINED
             | kDO       | kELSE      | kELSIF  | kEND   | kENSURE | kFALSE
             | kFOR      | kIN        | kMODULE | kNEXT  | kNIL    | kNOT
             | kOR       | kREDO      | kRESCUE | kRETRY | kRETURN | kSELF
             | kSUPER    | kTHEN      | kTRUE   | kUNDEF | kWHEN   | kYIELD
             | kIF_MOD   | kUNLESS_MOD | kWHILE_MOD | kUNTIL_MOD | kRESCUE_MOD

arg           : lhs '=' arg {
                  case val[0].first
                  when :gvar then
                    result = s(:gasgn, val[0].last, val[2])
                  when :lvar then
                    self.env[val[0].last] = true
                    val[0] = s(:lvar, val[0].last)
                    result = s(:lasgn, val[0].last, val[2])
                  when :ivar then
                    self.env[val[0].last] = true
                    result = s(:iasgn, val[0].last, val[2])
                  else
                    warn "*** unknown type #{val.inspect}"
                    result = s(:lasgn, val[0], val[2])
                  end
                 }
             | lhs '=' arg kRESCUE_MOD arg {
                 result = s(:lasgn, val[0].last.to_sym,
                            s(:rescue, val[2],
                              s(:resbody, nil, val[4], nil), nil))
                 }
             | var_lhs tOP_ASGN arg {
                 name = val[0].value;
                 asgn_op = val[1].value;

                 if asgn_op == "||" then
                   val[0] = s(:lasgn, val[0], val[2])  # HACK
                   # val[0][2] = (val[2]);
                   result = s(:op_asgn_or, support.gettable2(name), val[0]);
                 elsif asgn_op == "&&" then
                   # val[0][2] = (val[2]);
                   result = s(:op_asgn_and, support.gettable2(name), val[0]);
                 else
                   val[0][2] = (s(:call, support.gettable2(name), asgn_op, val[2]));
                   result = val[0];
                 end
                 }
             | primary_value '[' aref_args tRBRACK tOP_ASGN arg {
                  result = s(:op_asgn1, val[0], val[2], val[4].value.to_sym, val[5]);
                 }
             | primary_value tDOT tIDENTIFIER tOP_ASGN arg {
                             result = s(:op_asgn2, val[0], :"#{val[2].value}=", val[3].value.to_sym, val[4]);
                 }
             | primary_value tDOT tCONSTANT tOP_ASGN arg {
                  result = s(:op_asgn, val[0], val[4], val[2].value, val[3].value);
                 }
             | primary_value tCOLON2 tIDENTIFIER tOP_ASGN arg {
                  result = s(:op_asgn, val[0], val[4], val[2].value, val[3].value);
                 }
             | primary_value tCOLON2 tCONSTANT tOP_ASGN arg {
                 yyerror("constant re-assignment");
                 }
             | tCOLON3 tCONSTANT tOP_ASGN arg {
                 yyerror("constant re-assignment");
                 }
             | backref tOP_ASGN arg {
                  support.backref_assign_error(val[0]);
                 }
             | arg tDOT2 arg {
                 v1, v2 = val[0], val[2]
                 if v1.first == :lit and v2.first == :lit then
                   result = s(:lit, (v1.last)..(v2.last))
                 else
                   result = s(:dot2, v1, v2)
                 end
               }
             | arg tDOT3 arg {
                 v1, v2 = val[0], val[2]
                 if v1.first == :lit and v2.first == :lit then
                   result = s(:lit, (v1.last)...(v2.last))
                 else
                   result = s(:dot3, v1, v2)
                 end
               }
             | arg tPLUS arg {
                  result = s(:call, val[0], :+, s(:array, val[2]))
                 }
             | arg tMINUS arg {
                  result = s(:call, val[0], :-, s(:array, val[2]))
                 }
             | arg tSTAR2 arg {
                  result = s(:call, val[0], :*, s(:array, val[2]))
                 }
             | arg tDIVIDE arg {
                  result = s(:call, val[0], :"/", s(:array, val[2]))
                 }
             | arg tPERCENT arg {
                  result = s(:call, val[0], :%, s(:array, val[2]))
                 }
             | arg tPOW arg {
                 result = s(:call, val[0], :**, s(:array, val[2]))
                 }
             | tUMINUS_NUM tINTEGER tPOW arg {
                  result = s(:call, s(:call, val[1], :"**", s(:array, val[3])), :"-@");
                 }
             | tUMINUS_NUM tFLOAT tPOW arg {
                  result = s(:call, s(:call, val[1], :"**", s(:array, val[3])), :"-@");
                 }
             | tUPLUS arg {
                  if (support.is_literal(val[1])) then
                    result = val[1];
                  else
                    result = s(:call, val[1], :"+@");
                  end
                 }
             | tUMINUS arg {
                  result = s(:call, val[1], :"-@");
                 }
             | arg tPIPE arg {
                  result = s(:call, val[0], :"|", s(:array, val[2]));
                 }
             | arg tCARET arg {
                  result = s(:call, val[0], :"^", s(:array, val[2]));
                 }
             | arg tAMPER2 arg {
                  result = s(:call, val[0], :"&", s(:array, val[2]));
                 }
             | arg tCMP arg {
                  result = s(:call, val[0], :"<=>", s(:array, val[2]));
                 }
             | arg tGT arg {
                  result = s(:call, val[0], :">", s(:array, val[2]));
                 }
             | arg tGEQ arg {
                  result = s(:call, val[0], :">=", s(:array, val[2]));
                 }
             | arg tLT arg {
                  result = s(:call, val[0], :"<", s(:array, val[2]));
                 }
             | arg tLEQ arg {
                  result = s(:call, val[0], :"<=", s(:array, val[2]));
                 }
             | arg tEQ arg {
                  result = s(:call, val[0], :"==", s(:array, val[2]));
                 }
             | arg tEQQ arg {
                  result = s(:call, val[0], :"===", s(:array, val[2]));
                 }
             | arg tNEQ arg {
                  result = s(:not, s(:call, val[0], :"==", s(:array, val[2])));
                 }
             | arg tMATCH arg {
                  result = support.get_match_node(val[0], val[2])
                 }
             | arg tNMATCH arg {
                  result = s(:not, support.get_match_node(val[0], val[2]))
                 }
             | tBANG arg {
                  result = s(:not, val[1])
                 }
             | tTILDE arg {
                  result = s(:call, val[1], :"~");
                 }
             | arg tLSHFT arg {
                  result = s(:call, val[0], :"<<", s(:array, val[2])) # " stupid emacs
                 }
             | arg tRSHFT arg {
                  result = s(:call, val[0], :">>", s(:array, val[2]))
                 }
             | arg tANDOP arg {
                  result = support.new_and_node(val[0], val[2]);
                 }
             | arg tOROP arg {
                  result = s(:or, val[0], val[2])
                 }
             | kDEFINED opt_nl arg {
                  result = s(:defined, val[2]);
                 }
             | arg '?' arg ':' arg {
                  result = s(:if, val[0], val[2], val[4]);
                 }
             | primary {
                  result = val[0]
# TODO ?          result = result[1] if result[0] == :begin
                 }

arg_value     : arg

aref_args     : none
             | command opt_nl {
                 warnings.warn("parenthesize argument(s) for future version");
                 result = s(:array, val[0]);
               }
             | args trailer {
                 result = val[0];
               }
             | args ',' tSTAR arg opt_nl {
                 result = val[0] << val[3]
               }
             | assocs trailer {
                 result = s(:array, s(:hash, *val[0].values))
               }
             | tSTAR arg opt_nl {
                 result = s(:splat, val[1]) # HACK s(:newline, 
               }

paren_args    : tLPAREN2 none tRPAREN {
                  result = s(:array)
                 }
             | tLPAREN2 call_args opt_nl tRPAREN {
                  result = val[1];
                 }
             | tLPAREN2 block_call opt_nl tRPAREN {
                  warnings.warn("parenthesize argument(s) for future version");
                  result = s(:array, val[1]);
                 }
             | tLPAREN2 args ',' block_call opt_nl tRPAREN {
                  warnings.warn("parenthesize argument(s) for future version");
                  result = val[1].add(val[3]);
                 }

opt_paren_args: none | paren_args

#  Node:call_args - Arguments for a function call
call_args    : command {
                  warnings.warn("parenthesize argument(s) for future version");
                  result = s(:array, val[0])
                 }
             | args opt_block_arg {
                 if val[1] then
                   result = val[1] << val[0]
                 else
                   result = val[0]
                 end
               }
             | args ',' tSTAR arg_value opt_block_arg {
                 result = support.arg_concat(val[0], val[3]);
                 result = support.arg_blk_pass(result, val[4]);
               }
             | assocs opt_block_arg {
                 result = s(:array, s(:hash, val[0]))
                 result = support.arg_blk_pass(result, val[1]);
               }
             | assocs ',' tSTAR arg_value opt_block_arg {
                 result = support.arg_concat(s(:array, s(:hash, val[0])), val[3])
                 result = support.arg_blk_pass(result, val[4]);
               }
             | args ',' assocs opt_block_arg {
                 result = val[0].add(s(:hash, val[2]));
                 result = support.arg_blk_pass(result, val[3]);
               }
             | args ',' assocs ',' tSTAR arg opt_block_arg {
                 result = support.arg_concat(val[0].add(s(:hash, val[2])), val[5])
                 result = support.arg_blk_pass(result, val[6]);
               }
             | tSTAR arg_value opt_block_arg {
                  result = s(:splat, val[1])
                  result << val[2] if val[2] # HACK
                 }
             | block_arg

call_args2    : arg_value ',' args opt_block_arg {
                  result = support.arg_blk_pass(s(:array, val[0]).add_all(val[2]), val[3]);
                 }
             | arg_value ',' block_arg {
                  result = support.arg_blk_pass(s(:array, val[0]), val[2]);
                 }
             | arg_value ',' tSTAR arg_value opt_block_arg {
                  result = support.arg_concat(s(:array, val[0]), val[3]);
                  result = support.arg_blk_pass(result, val[4]);
                 }
             | arg_value ',' args ',' tSTAR arg_value opt_block_arg {
                  result = support.arg_concat(s(:array, val[0]).add_all(s(:hash, val[2])), val[5]);
                  result = support.arg_blk_pass(result, val[6]);
                 }
             | assocs opt_block_arg {
                  result = s(:array, s(:hash, val[0]));
                  result = support.arg_blk_pass(result, val[1]);
                 }
             | assocs ',' tSTAR arg_value opt_block_arg {
                  result = support.arg_concat(s(:array, s(:hash, val[0])), val[3]);
                  result = support.arg_blk_pass(result, val[4]);
                 }
             | arg_value ',' assocs opt_block_arg {
                  result = s(:array, val[0]).add(s(:hash, val[2]));
                  result = support.arg_blk_pass(result, val[3]);
                 }
             | arg_value ',' args ',' assocs opt_block_arg {
                  result = s(:array, val[0]).add_all(val[2]).add(s(:hash, val[4]));
                  result = support.arg_blk_pass(result, val[5]);
                 }
             | arg_value ',' assocs ',' tSTAR arg_value opt_block_arg {
                  result = support.arg_concat(s(:array, val[0]).add(s(:hash, val[2])), val[5]);
                  result = support.arg_blk_pass(result, val[6]);
                 }
             | arg_value ',' args ',' assocs ',' tSTAR arg_value opt_block_arg {
                  result = support.arg_concat(s(:array, val[0]).add_all(val[2]).add(s(:hash, val[4])), val[7]);
                  result = support.arg_blk_pass(result, val[8]);
                 }
             | tSTAR arg_value opt_block_arg {
                  result = support.arg_blk_pass(s(:splat, val[1]), val[2]);
                 }
             | block_arg {}

command_args  : {
                  val[0] = lexer.cmdarg.stack
                  lexer.cmdarg.push true
                } open_args {
                  lexer.cmdarg.stack = val[0]
                  result = val[1]
                }

open_args    : call_args
             | tLPAREN_ARG  {
                 lexer.state = LexState::EXPR_ENDARG
               } tRPAREN {
                  warnings.warn("don't put space before argument parentheses");
                  result = nil;
                 }
             | tLPAREN_ARG call_args2 {
                 lexer.state = LexState::EXPR_ENDARG
               } tRPAREN {
                  warnings.warn("don't put space before argument parentheses");
                  result = val[1];
                 }

block_arg     : tAMPER arg_value {
                  result = s(:block_pass, val[1]);
                 }

opt_block_arg : ',' block_arg {
                  result = val[1];
                 }
             | none_block_pass

args         : arg_value {
                 result = s(:array, val[0])
               }
             | args ',' arg_value {
                 result = val[0] << val[2]
               }

mrhs         : args ',' arg_value {
                 result = val[0]
                 result << val[2]
               }
             | args ',' tSTAR arg_value {
                 result = s(:argscat, val[0], val[3])
               }
             | tSTAR arg_value {
                 result = s(:splat, val[1])
               }

primary      : literal
             | strings
             | xstring
             | regexp
             | words
             | qwords
             | var_ref
             | backref
             | tFID {
                 result = s(:fcall, val[0].value.to_sym, nil)
               }
             | kBEGIN bodystmt kEND {
                 val[1] = s(:nil) unless val[1]
                 result = s(:begin, val[1])
                 # result = result.last if result.size == 2
               }
             | tLPAREN_ARG expr {
                  lexer.state = LexState::EXPR_ENDARG
               } opt_nl tRPAREN {
                  warnings.warning("(...) interpreted as grouped expression");
                  result = val[1];
                 }
             | tLPAREN compstmt tRPAREN {
                 result = val[1];
                 }
             | primary_value tCOLON2 tCONSTANT {
                 result = s(:colon2, val[0], val[2].value.to_sym)
               }
             | tCOLON3 tCONSTANT {
                 result = s(:colon3, val[1].value.to_sym)
               }
             | primary_value '[' aref_args tRBRACK {
                 if val[0].first == :self then
                   result = s(:fcall, :"[]", val[2])
                 else
                   result = s(:call, val[0], :"[]", val[2])
                 end
               }
             | tLBRACK aref_args tRBRACK {
                  if (val[1] == nil) then
                      result = s(:zarray)
                  else
                      result = val[1];
                  end
                 }
             | tLBRACE assoc_list tRCURLY {
                 result = s(:hash, *val[1][1..-1])
               }
             | kRETURN {
                 result = s(:return)
               }
             | kYIELD tLPAREN2 call_args tRPAREN {
                 result = support.new_yield(val[2]);
               }
             | kYIELD tLPAREN2 tRPAREN {
                 result = s(:yield)
               }
             | kYIELD {
                 result = s(:yield)
               }
             | kDEFINED opt_nl tLPAREN2 expr tRPAREN {
                 result = s(:defined, val[3]);
               }
             | operation brace_block {
                 name = val[0].value.to_sym
                 iter = val[1]
                 iter.insert 1, s(:fcall, name)
                 result = iter
               }
             | method_call
             | method_call brace_block {
#                if val[0] && val[0].get_iter_node.instanceof(BlockPassNode) then
#                  raise SyntaxException "Both block arg and actual block given."
#                end
                 call = val[0]
                 iter = val[1].find_and_replace_all(:lvar, :dvar)
                 iter[1][0] = :dasgn_curr if iter[1][0] == :dvar
                 iter.insert 1, call
                 result = iter
               }
             | kIF expr_value then compstmt if_tail kEND {
                  # HACK: val[1] needs to call value_expr(node)
                  result = s(:if, cond(val[1]), val[3], val[4]);
# 			if (cond_negative(&$$->nd_cond)) {
# 		            NODE *tmp = $$->nd_body;
# 		            $$->nd_body = $$->nd_else;
# 		            $$->nd_else = tmp;
# 			}
                 }
             | kUNLESS expr_value then compstmt opt_else kEND {
                  result = s(:if, val[1], val[4], val[3]);
                 }
             | kWHILE {
                 lexer.cond.push true
d :while => lexer.cond
               } expr_value do {
                 lexer.cond.pop
d :while => lexer.cond
               } compstmt kEND {
                  block = val[5]
                  block = nil if block == s(:block) # HACK
                  result = s(:while, val[2], block, true);
                 }
             | kUNTIL {
                 lexer.cond.push true;
               } expr_value do {
                 lexer.cond.pop;
               } compstmt kEND {
                 block = val[5] # REFACTOR
                 block = block.last if block.size == 2
                 block = nil if block == s(:block) # HACK

                 result = s(:until, val[2], block, true)
               }
             | kCASE expr_value opt_terms case_body kEND {
                  result = s(:case, val[1]);

                  body = val[3]
                  while body and body.first == :when
                    result << body
                    body = body.delete_at 3
                  end

                  if body then
                    result << body == s(:block) ? nil : body
                  else
                    result << s(:nil)
                  end
                 }
             | kCASE opt_terms case_body kEND {
                  result = s(:case, nil); # REFACTOR

                  body = val[2]
                  while body and body.first == :when
                    result << body
                    body = body.delete_at 3
                  end

                  if body then
                    result << body == s(:block) ? nil : body
                  else
                    result << s(:nil)
                  end
                 }
             | kCASE opt_terms kELSE compstmt kEND {
                 result = val[3];
                 }
             | kFOR block_var kIN {
                 lexer.cond.push true
               } expr_value do {
                 lexer.cond.pop;
               } compstmt kEND {
                 result = s(:for, val[4], s(:lasgn, val[1].value), val[7])
               }
             | kCLASS cpath superclass {
                  if (support.is_in_def || support.is_in_single > 0) then
                    yyerror("class definition in method body");
                  end
                  support.push_local_scope;
                } bodystmt kEND {
                  scope = val[4]
                  scope = s(:scope) if scope == s(:block) or scope.nil?
                  scope = s(:scope, scope) if scope and scope.first != :scope
                  result = s(:class, val[1].last.to_sym, val[2], scope)
                  support.pop_local_scope;
                 }
             | kCLASS tLSHFT expr {
                  result = support.is_in_def
                  support.in_def = false
                } term {
                  result = support.in_single
                  support.in_single = 0
                  support.push_local_scope;
                } bodystmt kEND {

                  scope = val[6] # REFACTOR
                  scope = s(:scope) if scope == s(:block)
                  scope = s(:scope, scope) unless scope.first == :scope

                  result = s(:sclass, val[2], scope);
                  support.pop_local_scope;
                  support.in_def = val[3] # FIX  HUH?
                  support.in_single = val[5] ? 1 : 0 # HACK
                 }
             | kMODULE cpath {
                 if (support.is_in_def || support.is_in_single > 0) then
                      yyerror("module definition in method body");
                 end
                 support.push_local_scope;
               } bodystmt kEND {
                  body = val[3]
                  body[0] = :scope if body[0] == :block # not sure
                  result = s(:module, val[1].last.to_sym, body);
                  support.pop_local_scope;
                 }
             | kDEF fname {
                  self.in_def = true
                  support.push_local_scope
                } f_arglist bodystmt kEND {
                    name = val[1].value.to_sym
                    args = val[3]
                    body = val[4].nil? ? s(:nil) : val[4]
                    body = s(:block, body) unless body[0] == :block
                    body.insert 1, args
                    result = s(:defn, name, s(:scope, body))
                  support.pop_local_scope
                  self.in_def = false
                 }
             | kDEF singleton dot_or_colon { # 0-2, 3
                 lexer.state = LexState::EXPR_FNAME
               } fname {                     # 4, 5
                 support.in_single += 1
                 support.push_local_scope;
                 lexer.state = LexState::EXPR_END # force for args
               } f_arglist bodystmt kEND {   # 6-8
                 recv, name, args, body = val[1], val[4], val[6], val[7]
                 name = name.value.to_sym
                 body = s(:nil) unless body  # REFACTOR
                 body = s(:block, body) unless body[0] == :block
                 body.insert 1, args
                 result = s(:defs, recv, name, s(:scope, body))
                 support.pop_local_scope;
                 support.in_single -= 1
               }
             | kBREAK {
                  result = s(:break)
                 }
             | kNEXT {
                  result = s(:next)
                 }
             | kREDO {
                  result = s(:redo)
                 }
             | kRETRY {
                  result = s(:retry)
                 }

primary_value : primary {
                  result = val[0];
                }

then          : term
              | ":"
              | kTHEN
              | term kTHEN

do            : term
              | ":"
              | kDO_COND

if_tail       : opt_else
              | kELSIF expr_value then compstmt if_tail {
                  result = s(:if, val[1], val[3], val[4]);
              }

opt_else      : none
              | kELSE compstmt {
                  result = val[1];
              }

block_var     : lhs
              | mlhs

opt_block_var : none
              | tPIPE tPIPE {
                  result = 0
              }
              | tOROP {
                  result = 0
                }
              | tPIPE block_var tPIPE {
                  result = val[1];
              }

do_block      : kDO_BLOCK {
                  support.push_local_scope;
                } opt_block_var compstmt kEND {
                  result = s(:iter)
                  result << val[2] if val[2]
                  result << val[1] if val[1]
                  result << val[3] if val[3]
                  support.pop_local_scope;
                }

block_call    : command do_block {
              if val[0] && val[0].get_iter_node[0] == :blockpass then
                throw SyntaxException.new("Both block arg and actual block given.");
              end
              val[0].set_iter_node(val[1]);
              }
              | block_call tDOT operation2 opt_paren_args {
                  result = s(:call, val[0], val[2]);
                  result << val[3] if val[3]
              }
              | block_call tCOLON2 operation2 opt_paren_args {
                  result = s(:call, val[0], val[2]);
                  result << val[3] if val[3]
              }

method_call   : operation paren_args {
                  result = s(:fcall, val[0].value.to_sym, val[1])
                }
              | primary_value tDOT operation2 opt_paren_args {
                  result = s(:call, val[0], val[2].value.to_sym)
                  result << val[3] if val[3]
                }
              | primary_value tCOLON2 operation2 paren_args {
                  result = s(:call, val[0], val[2]);
                  result << val[3] if val[3]
                }
              | primary_value tCOLON2 operation3 {
                  result = s(:call, val[0], val[2]);
                }
              | kSUPER paren_args {
                  result = support.new_super(val[1], val[0]);
                }
              | kSUPER {
                  result = s(:zsuper)
                }

brace_block   : tLCURLY {
                  support.push_local_scope;
                } opt_block_var compstmt tRCURLY {
                  args = val[2]
                  body = val[3] == s(:block) ? nil : val[3]
                  result = s(:iter)
                  result << args
                  result << body if body
                  support.pop_local_scope;
                }
              | kDO {
                  support.push_local_scope;
                } opt_block_var compstmt kEND {
                  result = s(:iter, val[2], val[3]);
                  support.pop_local_scope;
                }

case_body     : kWHEN when_args then compstmt cases {
                  body = val[3]
                  body = nil if body == s(:block) # REFACTOR
                  result = s(:when, val[1], body, val[4])
                }

when_args     : args
              | args ',' tSTAR arg_value {
                  result = val[0].add(s(:when, val[3], nil, nil));
              }
              | tSTAR arg_value {
                  result = s(:array, s(:when, val[1], nil, nil));
              }

cases         : opt_else | case_body

opt_rescue    : kRESCUE exc_list exc_var then compstmt opt_rescue {
                  val[4] = nil if val[4] == s(:block)
                  body = val[4]
                  if val[2] then
                    body = s(:block, s(:lasgn, val[2].last.to_sym, s(:gvar, :"$!")), body)
                  end

                  result = s(:resbody)
                  result << val[1] # if val[1]
                  result << body if body
                  result << val[5] if val[5]
                 }
             | {result = nil;}

exc_list      : arg_value {
                  result = s(:array, val[0]);
                 }
             | mrhs
             | none

exc_var       : tASSOC lhs {
                  result = val[1];
                 }
             | none

opt_ensure    : kENSURE compstmt {
                  if (val[1] != nil) then
                      result = val[1];
                  else
                      result = s(:nil)
                  end
                 }
              | none

literal       : numeric { result = s(:lit, val[0]) }
              | symbol  { result = s(:lit, val[0]) }
              | dsym    { result = s(:lit, val[0]) }

strings       : string {
                  result = s(:dstr, val[0].value) if val[0][0] == :evstr
                  result = val[0];
                }

string        : string1
              | string string1 {
                  result = support.literal_concat(val[0], val[1]);
                }

string1       : tSTRING_BEG string_contents tSTRING_END {
                  result = val[1];
                  extra_length = (val[0].value).length - 1;

                  #  We may need to subtract addition offset off of first
                  #  string fragment (we optimistically take one off in
                  #  ParserSupport.literal_concat).  Check token length
                  #  and subtract as neeeded.
#                   if ((val[1].instanceof DStrNode) && extra_length > 0) then
#                     str_node = (val[1]).get(0);
#                     assert str_node != nil;
#                   end
                 }

xstring       : tXSTRING_BEG xstring_contents tSTRING_END {
                  result = s(:xstr, val[1].value) # TODO: clone?
                  # result[0] = :dxstr if val[1][0] = :dstr
#                   if val[1].nil? then
#                     result = s(:xstr, nil)
#                   elsif val[1][0] == :str then
#                     result = s(:xstr, val[1].value)
#                   elsif val[1][0] == :dstr then
#                     result = s(:dxstr, val[1].value)
#                   else
#                     result = s(:xstr, val[1].value)
#                   end
                 }

regexp        : tREGEXP_BEG xstring_contents tREGEXP_END {
                  result = s(:lit, Regexp.new(val[1].value)) # HACK: deal with options for real
                  # HACK result[0] = :dregex if val[1][0] = :dstr
# HACK
#                   options = val[2].get_options;
#                   node = val[1];

#                   if (node == nil) then
#                     result = s(:regex, ByteList.create(""), options & ~ReOptions.RE_OPTION_ONCE);
#                   elsif (node.instanceof StrNode) then
#                     result = s(:regex,  node.value.clone, options & ~ReOptions.RE_OPTION_ONCE);
#                   elsif (node.instanceof DStrNode) then
#                     result = s(:dregex, node, options, (options & ReOptions.RE_OPTION_ONCE) != 0);
#                   else
#                     result = s(:dregex, options, (options & ReOptions.RE_OPTION_ONCE) != 0).add(node);
#                   end
                 }

words          : tWORDS_BEG ' ' tSTRING_END {
                   result = s(:zarray);
                 }
               | tWORDS_BEG word_list tSTRING_END {
                 result = val[1];
                 }

word_list      :  {
                   result = s(:array)
                 }
               | word_list word ' ' {
                 result = val[0].add(val[1][0] == :evstr ? s(:dstr, val[1]) : val[1])
                 }

word           : string_content
               | word string_content {
                 result = support.literal_concat(val[0], val[1]);
                 }

qwords         : tQWORDS_BEG ' ' tSTRING_END {
                   result = s(:zarray);
                 }
               | tQWORDS_BEG qword_list tSTRING_END {
                   result = val[1];
                 }

qword_list     : {
                   result = s(:array)
                 }
               | qword_list tSTRING_CONTENT ' ' {
                   result = val[0].add(val[1]);
                 }

string_contents: { result = s(:str, "") }
               | string_contents string_content {
                   result = s(:str, val[0].value + val[1].value)
                 }

xstring_contents: { result = s(:xstr, "") }
                | xstring_contents string_content {
                    result = s(:xstr, val[0].value + val[1].value)
                  }

string_content : tSTRING_CONTENT {
                   result = val[0];
                 }
               | tSTRING_DVAR {
                   result = lexer.str_term;
                   lexer.str_term = nil
                   lexer.state = LexState::EXPR_BEG
                 } string_dvar {
                   lexer.str_term = val[1]
                   result = s(:evstr, val[2]);
                   }
               | tSTRING_DBEG {
                   result = lexer.str_term;
                   lexer.str_term = nil
                   lexer.state = LexState::EXPR_BEG
                   lexer.cond.push false
                   lexer.cmdarg.push false
                 } compstmt tRCURLY {
                   lexer.str_term = val[1]
                   lexer.cond.lexpop
                   lexer.cmdarg.lexpop
                   result = support.new_ev_str_node(val[2]);
                 }

string_dvar    : tGVAR {
                   result = s(:global_var, val[0].value);
                 }
               | tIVAR {
                   result = s(:inst_var, val[0].value);
                 }
               | tCVAR {
                   result = s(:class_var, val[0].value);
                 }
               | backref


symbol         : tSYMBEG sym {
                   lexer.state = LexState::EXPR_END
                   result = val[1].value.to_sym
                 }

sym            : fname | tIVAR | tGVAR | tCVAR

dsym           : tSYMBEG xstring_contents tSTRING_END {
                   lexer.state = LexState::EXPR_END

                   #  DStrNode: :"some text #{some expression}"
                   #  StrNode: :"some text"
                   #  EvStrNode :"#{some expression}"
                   if (val[1] == nil) then
                     yyerror("empty symbol literal");
                   end

                   # HACK if val[1][0] == :dstr then
                   result = /#{val[1]}/
                 }

#  Node:numeric - numeric value [!nil]
numeric      : tINTEGER
             | tFLOAT
             | tUMINUS_NUM tINTEGER = tLOWEST {
                 result = support.negate_integer(val[1]);
               }
             | tUMINUS_NUM tFLOAT = tLOWEST {
                 result = support.negate_float(val[1]);
               }

#  Token:variable - name (special and normal onces)
variable       : tIDENTIFIER {
                    name = val[0].value.to_sym
                    if env.has_key? name then
                      result = s(:lvar, val[0].value.to_sym)
                    else
                      result = s(:vcall, val[0].value.to_sym)
                    end
                  }
                | tIVAR {
                    result = s(:ivar, val[0]);
                  }
                | tGVAR {
                    result = s(:gvar, val[0].value.to_sym);
                  }
                | tCONSTANT {
                    result = s(:const, val[0].value.to_sym);
                  }
                | tCVAR {
                    result = s(:cvar, val[0].value.to_sym);
                  }
                | kNIL {
                   result = s(:nil)
                 }
               | kSELF {
                   result = s(:self)
                 }
               | kTRUE {
                   result = s(:true)
                 }
               | kFALSE {
                   result = s(:false)
                 }
               | k__FILE__ {
                   result = Token.new("__FILE__");
                 }
               | k__LINE__ {
                   result = Token.new("__LINE__");
                 }

var_ref        : variable

var_lhs        : variable {
                   result = val[0] # HACK support.assignable(val[0], nil);
                 }

backref        : tNTH_REF | tBACK_REF

superclass     : term {
                   result = nil;
                 }
             | tLT {
                 lexer.state = LexState::EXPR_BEG
             } expr_value term {
                 result = val[2];
                 }
             | error term {
                 yyerrok;
                 result = nil;
                 }

#  f_arglist: Function Argument list for definitions
f_arglist      : tLPAREN2 f_args opt_nl tRPAREN {
                   result = val[1];
                   lexer.state = LexState::EXPR_BEG
                 }
               | f_args term {
                   result = val[0];
                 }

f_args       : f_arg ',' f_optarg ',' f_rest_arg opt_f_block_arg {
                 result = val[0]
                 result << val[2] if val[2]
                 result << val[4] if val[4]
                 result << val[5] if val[5]
               }
             | f_arg ',' f_optarg opt_f_block_arg {
                 result = val[0]
                 result << val[2] if val[2]
                 result << val[3] if val[3]
                 }
             | f_arg ',' f_rest_arg opt_f_block_arg {
                 result = val[0]
                 result << val[2] if val[2]
                 result << val[3] if val[3]
                 }
             | f_arg opt_f_block_arg {
                 result = val[0]
                 result << val[1] if val[1]
                 }
             | f_optarg ',' f_rest_arg opt_f_block_arg {
                   result = s(:args, val[0], val[2], val[3]).compact
                 }
             | f_optarg opt_f_block_arg {
                   result = s(:args, val[0], -1, nil, val[1]).compact
                 }
             | f_rest_arg opt_f_block_arg {
                   result = s(:args, val[0], val[1]).compact
                 }
             | f_block_arg {
                   result = s(:args, val[0]).compact
                 }
             |  {
                   result = s(:args)
                 }

f_norm_arg   : tCONSTANT {
                 yyerror("formal argument cannot be a constant");
               }
             | tIVAR {
                 yyerror("formal argument cannot be an instance variable");
               }
             | tCVAR {
                 yyerror("formal argument cannot be a class variable");
               }
             | tIDENTIFIER {
                 identifier = val[0].value.to_sym
# TODO
#                    if (IdUtil.var_type(identifier) != IdUtil.LOCAL_VAR) then
#                      yyerror("formal argument must be local variable");
#                    elsif (support.current_scope.local_scope.is_defined(identifier) >= 0) then
#                      yyerror("duplicate argument name");
#                    end

                   self.env[identifier] = true

                   result = val[0];
                 }

f_arg          : f_norm_arg {
                   result = s(:args)
                   result << val[0].value.to_sym
                 }
               | f_arg ',' f_norm_arg {
                   val[0] << val[2].value.to_sym
                   result = val[0]
                 }

f_opt          : tIDENTIFIER '=' arg_value {
                   identifier = val[0].value.to_sym

# TODO
#                    if (IdUtil.var_type(identifier) != IdUtil.LOCAL_VAR) then
#                      yyerror("formal argument must be local variable");
#                    elsif (support.current_scope.local_scope.is_defined(identifier) >= 0) then
#                      yyerror("duplicate optional argument name");
#                    end
                   self.env[identifier] = true

                   result = support.assignable(val[0], val[2]);
                 }

f_optarg      : f_opt {
                  result = s(:block, val[0])
                 }
             | f_optarg ',' f_opt {
                  result = support.append_to_block(val[0], val[2]);
                 }

restarg_mark  : tSTAR2 | tSTAR

f_rest_arg    : restarg_mark tIDENTIFIER {
                  name = val[1].value.to_sym

# HACK
#                   if (IdUtil.var_type(identifier) != IdUtil.LOCAL_VAR) then
#                     yyerror("rest argument must be local variable");
#                   elsif (support.current_scope.local_scope.is_defined(identifier) >= 0) then
#                     yyerror("duplicate rest argument name");
#                   end
                 self.env[name] = true

                 result = :"*#{name}"
               }
             | restarg_mark {
                  self.env[:"*"] = true
                  result = :"*"
                 }

blkarg_mark   : tAMPER2 | tAMPER

f_block_arg   : blkarg_mark tIDENTIFIER {
                  identifier = val[1].value.to_sym

# HACK
#                   if (IdUtil.var_type(identifier) != IdUtil.LOCAL_VAR) then
#                     yyerror("block argument must be local variable");
#                   elsif (support.current_scope.local_scope.is_defined(identifier) >= 0) then
#                     yyerror("duplicate block argument name");
#                   end
                  self.env[identifier] = true
                  result = s(:block_arg, :"&#{identifier}")
                 }

opt_f_block_arg: ',' f_block_arg {
                  result = val[1];
                 }
             | {
                 result = nil;
                 }

singleton     : var_ref {
                  result = val[0];
                 }
             | tLPAREN2 {
                 lexer.state = LexState::EXPR_BEG
               } expr opt_nl tRPAREN {
                 if (val[2].instanceof ILiteralNode) then
                    yyerror("Can't define single method for literals.");
                 end
                 result = val[2];
               }

#  ListNode:assoc_list - list of hash values pairs, like assocs but also
#    will accept ordinary list-style (e.g. a,b,c,d or a=>b,c=>d) [?nil]
assoc_list    : none { #  [!nil]
                  result = s(:array)
                 }
             | assocs trailer { #  [!nil]
                  result = val[0];
                 }
             | args trailer {
                  if (val[0].size % 2 != 0) then
                    yyerror("Odd number list for Hash.");
                  end
                  result = val[0];
                 }

#  ListNode:assocs - list of hash value pairs (e.g. a => b, c => d)
assocs        : assoc
              | assocs ',' assoc {
                  list = val[0].dup
                  more = val[2][1..-1]
                  list.push(*more) unless more.empty?
                  result = list
                }

#  ListNode:assoc - A single hash value pair (e.g. a => b)
assoc         : arg_value tASSOC arg_value {
                  result = s(:array, val[0], val[2])
                }

operation     : tIDENTIFIER | tCONSTANT | tFID
operation2    : tIDENTIFIER | tCONSTANT | tFID | op
operation3    : tIDENTIFIER | tFID | op
dot_or_colon  : tDOT | tCOLON2
opt_terms     :  | terms
opt_nl        :  | '\n'
trailer       :  | '\n' | ','

term          : ';' {
                  yyerrok;
                }
             | '\n'

terms         : term
             | terms ';' {
                 yyerrok;
               }

none          : {
                  result = nil;
                }

none_block_pass:  {
                  result = nil;
                 }

end

---- inner

require 'ruby_lexer'
