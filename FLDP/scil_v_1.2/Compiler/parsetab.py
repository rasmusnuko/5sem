
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'rightEQNEQLTGTLTEGTEleftPLUSMINUSleftTIMESDIVIDEASSIGN COMMA DIVIDE DO ELSE EQ FUNCTION GT GTE IDENT IF INT LCURL LPAREN LT LTE MINUS NEQ PLUS PRINT RCURL RETURN RPAREN SEMICOL THEN TIMES VAR WHILEprogram : bodyempty :body : optional_variables_declaration_list optional_functions_declaration_list statement_listoptional_variables_declaration_list : empty\n                                           | variables_declaration_listvariables_declaration_list : VAR variables_list\n                                  | VAR variables_list variables_declaration_listvariables_list : IDENT\n                      | IDENT COMMA variables_listoptional_functions_declaration_list : empty\n                                           | functions_declaration_listfunctions_declaration_list : function\n                                  | function functions_declaration_listfunction : FUNCTION IDENT LPAREN optional_parameter_list RPAREN LCURL body RCURLoptional_parameter_list : empty\n                               | parameter_listparameter_list : IDENT\n                      | IDENT COMMA parameter_liststatement : statement_return\n                 | statement_print\n                 | statement_assignment\n                 | statement_ifthenelse\n                 | statement_while\n                 | statement_compoundstatement_return : RETURN expression SEMICOLstatement_print : PRINT expression SEMICOLstatement_assignment : IDENT ASSIGN expression SEMICOLstatement_ifthenelse : IF expression THEN statement ELSE statementstatement_while :  WHILE expression DO statementstatement_compound :  LCURL statement_list RCURLstatement_list : statement\n                      | statement statement_listexpression : expression_integer\n                  | expression_identifier\n                  | expression_call\n                  | expression_binop\n                  | expression_groupexpression_integer : INTexpression_identifier : IDENTexpression_call : IDENT LPAREN optional_expression_list RPARENexpression_binop : expression PLUS expression\n                        | expression MINUS expression\n                        | expression TIMES expression\n                        | expression DIVIDE expression\n                        | expression EQ expression\n                        | expression NEQ expression\n                        | expression LT expression\n                        | expression GT expression\n                        | expression LTE expression\n                        | expression GTE expressionexpression_group : LPAREN expression RPARENoptional_expression_list : empty\n                                | expression_listexpression_list : expression\n                       | expression COMMA expression_list'
    
_lr_action_items = {'FUNCTION':([0,3,4,5,10,12,13,30,48,95,99,],[-2,11,-4,-5,11,-6,-8,-7,-9,-2,-14,]),'RETURN':([0,3,4,5,7,8,9,10,12,13,15,16,17,18,19,20,21,27,28,30,48,49,62,64,65,66,86,88,93,95,97,99,],[-2,-2,-4,-5,22,-10,-11,-12,-6,-8,22,-19,-20,-21,-22,-23,-24,22,-13,-7,-9,-25,-26,22,22,-30,-27,-29,22,-2,-28,-14,]),'PRINT':([0,3,4,5,7,8,9,10,12,13,15,16,17,18,19,20,21,27,28,30,48,49,62,64,65,66,86,88,93,95,97,99,],[-2,-2,-4,-5,23,-10,-11,-12,-6,-8,23,-19,-20,-21,-22,-23,-24,23,-13,-7,-9,-25,-26,23,23,-30,-27,-29,23,-2,-28,-14,]),'IDENT':([0,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,25,26,27,28,30,31,41,43,47,48,49,50,51,52,53,54,55,56,57,58,59,60,62,64,65,66,86,88,89,92,93,95,97,99,],[-2,-2,-4,-5,13,24,-10,-11,-12,29,-6,-8,24,-19,-20,-21,-22,-23,-24,40,40,40,40,24,-13,-7,13,40,40,67,-9,-25,40,40,40,40,40,40,40,40,40,40,40,-26,24,24,-30,-27,-29,67,40,24,-2,-28,-14,]),'IF':([0,3,4,5,7,8,9,10,12,13,15,16,17,18,19,20,21,27,28,30,48,49,62,64,65,66,86,88,93,95,97,99,],[-2,-2,-4,-5,25,-10,-11,-12,-6,-8,25,-19,-20,-21,-22,-23,-24,25,-13,-7,-9,-25,-26,25,25,-30,-27,-29,25,-2,-28,-14,]),'WHILE':([0,3,4,5,7,8,9,10,12,13,15,16,17,18,19,20,21,27,28,30,48,49,62,64,65,66,86,88,93,95,97,99,],[-2,-2,-4,-5,26,-10,-11,-12,-6,-8,26,-19,-20,-21,-22,-23,-24,26,-13,-7,-9,-25,-26,26,26,-30,-27,-29,26,-2,-28,-14,]),'LCURL':([0,3,4,5,7,8,9,10,12,13,15,16,17,18,19,20,21,27,28,30,48,49,62,64,65,66,86,88,90,93,95,97,99,],[-2,-2,-4,-5,27,-10,-11,-12,-6,-8,27,-19,-20,-21,-22,-23,-24,27,-13,-7,-9,-25,-26,27,27,-30,-27,-29,95,27,-2,-28,-14,]),'VAR':([0,12,13,48,95,],[6,6,-8,-9,6,]),'$end':([1,2,14,15,16,17,18,19,20,21,32,49,62,66,86,88,97,],[0,-1,-3,-31,-19,-20,-21,-22,-23,-24,-32,-25,-26,-30,-27,-29,-28,]),'COMMA':([13,34,35,36,37,38,39,40,67,71,72,73,74,75,76,77,78,79,80,84,85,91,],[31,-33,-34,-35,-36,-37,-38,-39,89,-41,-42,-43,-44,-45,-46,-47,-48,-49,-50,92,-51,-40,]),'RCURL':([14,15,16,17,18,19,20,21,32,46,49,62,66,86,88,97,98,],[-3,-31,-19,-20,-21,-22,-23,-24,-32,66,-25,-26,-30,-27,-29,-28,99,]),'ELSE':([16,17,18,19,20,21,49,62,66,86,87,88,97,],[-19,-20,-21,-22,-23,-24,-25,-26,-30,-27,93,-29,-28,]),'INT':([22,23,25,26,41,43,50,51,52,53,54,55,56,57,58,59,60,92,],[39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,]),'LPAREN':([22,23,25,26,29,40,41,43,50,51,52,53,54,55,56,57,58,59,60,92,],[41,41,41,41,47,60,41,41,41,41,41,41,41,41,41,41,41,41,41,41,]),'ASSIGN':([24,],[43,]),'SEMICOL':([33,34,35,36,37,38,39,40,42,63,71,72,73,74,75,76,77,78,79,80,85,91,],[49,-33,-34,-35,-36,-37,-38,-39,62,86,-41,-42,-43,-44,-45,-46,-47,-48,-49,-50,-51,-40,]),'PLUS':([33,34,35,36,37,38,39,40,42,44,45,61,63,71,72,73,74,75,76,77,78,79,80,84,85,91,],[50,-33,-34,-35,-36,-37,-38,-39,50,50,50,50,50,-41,-42,-43,-44,50,50,50,50,50,50,50,-51,-40,]),'MINUS':([33,34,35,36,37,38,39,40,42,44,45,61,63,71,72,73,74,75,76,77,78,79,80,84,85,91,],[51,-33,-34,-35,-36,-37,-38,-39,51,51,51,51,51,-41,-42,-43,-44,51,51,51,51,51,51,51,-51,-40,]),'TIMES':([33,34,35,36,37,38,39,40,42,44,45,61,63,71,72,73,74,75,76,77,78,79,80,84,85,91,],[52,-33,-34,-35,-36,-37,-38,-39,52,52,52,52,52,52,52,-43,-44,52,52,52,52,52,52,52,-51,-40,]),'DIVIDE':([33,34,35,36,37,38,39,40,42,44,45,61,63,71,72,73,74,75,76,77,78,79,80,84,85,91,],[53,-33,-34,-35,-36,-37,-38,-39,53,53,53,53,53,53,53,-43,-44,53,53,53,53,53,53,53,-51,-40,]),'EQ':([33,34,35,36,37,38,39,40,42,44,45,61,63,71,72,73,74,75,76,77,78,79,80,84,85,91,],[54,-33,-34,-35,-36,-37,-38,-39,54,54,54,54,54,-41,-42,-43,-44,54,54,54,54,54,54,54,-51,-40,]),'NEQ':([33,34,35,36,37,38,39,40,42,44,45,61,63,71,72,73,74,75,76,77,78,79,80,84,85,91,],[55,-33,-34,-35,-36,-37,-38,-39,55,55,55,55,55,-41,-42,-43,-44,55,55,55,55,55,55,55,-51,-40,]),'LT':([33,34,35,36,37,38,39,40,42,44,45,61,63,71,72,73,74,75,76,77,78,79,80,84,85,91,],[56,-33,-34,-35,-36,-37,-38,-39,56,56,56,56,56,-41,-42,-43,-44,56,56,56,56,56,56,56,-51,-40,]),'GT':([33,34,35,36,37,38,39,40,42,44,45,61,63,71,72,73,74,75,76,77,78,79,80,84,85,91,],[57,-33,-34,-35,-36,-37,-38,-39,57,57,57,57,57,-41,-42,-43,-44,57,57,57,57,57,57,57,-51,-40,]),'LTE':([33,34,35,36,37,38,39,40,42,44,45,61,63,71,72,73,74,75,76,77,78,79,80,84,85,91,],[58,-33,-34,-35,-36,-37,-38,-39,58,58,58,58,58,-41,-42,-43,-44,58,58,58,58,58,58,58,-51,-40,]),'GTE':([33,34,35,36,37,38,39,40,42,44,45,61,63,71,72,73,74,75,76,77,78,79,80,84,85,91,],[59,-33,-34,-35,-36,-37,-38,-39,59,59,59,59,59,-41,-42,-43,-44,59,59,59,59,59,59,59,-51,-40,]),'THEN':([34,35,36,37,38,39,40,44,71,72,73,74,75,76,77,78,79,80,85,91,],[-33,-34,-35,-36,-37,-38,-39,64,-41,-42,-43,-44,-45,-46,-47,-48,-49,-50,-51,-40,]),'DO':([34,35,36,37,38,39,40,45,71,72,73,74,75,76,77,78,79,80,85,91,],[-33,-34,-35,-36,-37,-38,-39,65,-41,-42,-43,-44,-45,-46,-47,-48,-49,-50,-51,-40,]),'RPAREN':([34,35,36,37,38,39,40,47,60,61,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,91,94,96,],[-33,-34,-35,-36,-37,-38,-39,-2,-2,85,-17,90,-15,-16,-41,-42,-43,-44,-45,-46,-47,-48,-49,-50,91,-52,-53,-54,-51,-40,-18,-55,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,],[1,]),'body':([0,95,],[2,98,]),'optional_variables_declaration_list':([0,95,],[3,3,]),'empty':([0,3,47,60,95,],[4,8,69,82,4,]),'variables_declaration_list':([0,12,95,],[5,30,5,]),'optional_functions_declaration_list':([3,],[7,]),'functions_declaration_list':([3,10,],[9,28,]),'function':([3,10,],[10,10,]),'variables_list':([6,31,],[12,48,]),'statement_list':([7,15,27,],[14,32,46,]),'statement':([7,15,27,64,65,93,],[15,15,15,87,88,97,]),'statement_return':([7,15,27,64,65,93,],[16,16,16,16,16,16,]),'statement_print':([7,15,27,64,65,93,],[17,17,17,17,17,17,]),'statement_assignment':([7,15,27,64,65,93,],[18,18,18,18,18,18,]),'statement_ifthenelse':([7,15,27,64,65,93,],[19,19,19,19,19,19,]),'statement_while':([7,15,27,64,65,93,],[20,20,20,20,20,20,]),'statement_compound':([7,15,27,64,65,93,],[21,21,21,21,21,21,]),'expression':([22,23,25,26,41,43,50,51,52,53,54,55,56,57,58,59,60,92,],[33,42,44,45,61,63,71,72,73,74,75,76,77,78,79,80,84,84,]),'expression_integer':([22,23,25,26,41,43,50,51,52,53,54,55,56,57,58,59,60,92,],[34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,]),'expression_identifier':([22,23,25,26,41,43,50,51,52,53,54,55,56,57,58,59,60,92,],[35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,]),'expression_call':([22,23,25,26,41,43,50,51,52,53,54,55,56,57,58,59,60,92,],[36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,]),'expression_binop':([22,23,25,26,41,43,50,51,52,53,54,55,56,57,58,59,60,92,],[37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,]),'expression_group':([22,23,25,26,41,43,50,51,52,53,54,55,56,57,58,59,60,92,],[38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,]),'optional_parameter_list':([47,],[68,]),'parameter_list':([47,89,],[70,94,]),'optional_expression_list':([60,],[81,]),'expression_list':([60,92,],[83,96,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> body','program',1,'p_program','lexer_parser.py',116),
  ('empty -> <empty>','empty',0,'p_empty','lexer_parser.py',121),
  ('body -> optional_variables_declaration_list optional_functions_declaration_list statement_list','body',3,'p_body','lexer_parser.py',126),
  ('optional_variables_declaration_list -> empty','optional_variables_declaration_list',1,'p_optional_variables_declaration_list','lexer_parser.py',131),
  ('optional_variables_declaration_list -> variables_declaration_list','optional_variables_declaration_list',1,'p_optional_variables_declaration_list','lexer_parser.py',132),
  ('variables_declaration_list -> VAR variables_list','variables_declaration_list',2,'p_variables_declaration_list','lexer_parser.py',137),
  ('variables_declaration_list -> VAR variables_list variables_declaration_list','variables_declaration_list',3,'p_variables_declaration_list','lexer_parser.py',138),
  ('variables_list -> IDENT','variables_list',1,'p_variables_list','lexer_parser.py',146),
  ('variables_list -> IDENT COMMA variables_list','variables_list',3,'p_variables_list','lexer_parser.py',147),
  ('optional_functions_declaration_list -> empty','optional_functions_declaration_list',1,'p_optional_functions_declaration_list','lexer_parser.py',155),
  ('optional_functions_declaration_list -> functions_declaration_list','optional_functions_declaration_list',1,'p_optional_functions_declaration_list','lexer_parser.py',156),
  ('functions_declaration_list -> function','functions_declaration_list',1,'p_functions_declaration_list','lexer_parser.py',161),
  ('functions_declaration_list -> function functions_declaration_list','functions_declaration_list',2,'p_functions_declaration_list','lexer_parser.py',162),
  ('function -> FUNCTION IDENT LPAREN optional_parameter_list RPAREN LCURL body RCURL','function',8,'p_function','lexer_parser.py',170),
  ('optional_parameter_list -> empty','optional_parameter_list',1,'p_optional_parameter_list','lexer_parser.py',175),
  ('optional_parameter_list -> parameter_list','optional_parameter_list',1,'p_optional_parameter_list','lexer_parser.py',176),
  ('parameter_list -> IDENT','parameter_list',1,'p_parameter_list','lexer_parser.py',181),
  ('parameter_list -> IDENT COMMA parameter_list','parameter_list',3,'p_parameter_list','lexer_parser.py',182),
  ('statement -> statement_return','statement',1,'p_statement','lexer_parser.py',190),
  ('statement -> statement_print','statement',1,'p_statement','lexer_parser.py',191),
  ('statement -> statement_assignment','statement',1,'p_statement','lexer_parser.py',192),
  ('statement -> statement_ifthenelse','statement',1,'p_statement','lexer_parser.py',193),
  ('statement -> statement_while','statement',1,'p_statement','lexer_parser.py',194),
  ('statement -> statement_compound','statement',1,'p_statement','lexer_parser.py',195),
  ('statement_return -> RETURN expression SEMICOL','statement_return',3,'p_statement_return','lexer_parser.py',200),
  ('statement_print -> PRINT expression SEMICOL','statement_print',3,'p_statement_print','lexer_parser.py',205),
  ('statement_assignment -> IDENT ASSIGN expression SEMICOL','statement_assignment',4,'p_statement_assignment','lexer_parser.py',210),
  ('statement_ifthenelse -> IF expression THEN statement ELSE statement','statement_ifthenelse',6,'p_statement_ifthenelse','lexer_parser.py',215),
  ('statement_while -> WHILE expression DO statement','statement_while',4,'p_statement_while','lexer_parser.py',220),
  ('statement_compound -> LCURL statement_list RCURL','statement_compound',3,'p_statement_compound','lexer_parser.py',225),
  ('statement_list -> statement','statement_list',1,'p_statement_list','lexer_parser.py',230),
  ('statement_list -> statement statement_list','statement_list',2,'p_statement_list','lexer_parser.py',231),
  ('expression -> expression_integer','expression',1,'p_expression','lexer_parser.py',239),
  ('expression -> expression_identifier','expression',1,'p_expression','lexer_parser.py',240),
  ('expression -> expression_call','expression',1,'p_expression','lexer_parser.py',241),
  ('expression -> expression_binop','expression',1,'p_expression','lexer_parser.py',242),
  ('expression -> expression_group','expression',1,'p_expression','lexer_parser.py',243),
  ('expression_integer -> INT','expression_integer',1,'p_expression_integer','lexer_parser.py',248),
  ('expression_identifier -> IDENT','expression_identifier',1,'p_expression_identifier','lexer_parser.py',253),
  ('expression_call -> IDENT LPAREN optional_expression_list RPAREN','expression_call',4,'p_expression_call','lexer_parser.py',258),
  ('expression_binop -> expression PLUS expression','expression_binop',3,'p_expression_binop','lexer_parser.py',263),
  ('expression_binop -> expression MINUS expression','expression_binop',3,'p_expression_binop','lexer_parser.py',264),
  ('expression_binop -> expression TIMES expression','expression_binop',3,'p_expression_binop','lexer_parser.py',265),
  ('expression_binop -> expression DIVIDE expression','expression_binop',3,'p_expression_binop','lexer_parser.py',266),
  ('expression_binop -> expression EQ expression','expression_binop',3,'p_expression_binop','lexer_parser.py',267),
  ('expression_binop -> expression NEQ expression','expression_binop',3,'p_expression_binop','lexer_parser.py',268),
  ('expression_binop -> expression LT expression','expression_binop',3,'p_expression_binop','lexer_parser.py',269),
  ('expression_binop -> expression GT expression','expression_binop',3,'p_expression_binop','lexer_parser.py',270),
  ('expression_binop -> expression LTE expression','expression_binop',3,'p_expression_binop','lexer_parser.py',271),
  ('expression_binop -> expression GTE expression','expression_binop',3,'p_expression_binop','lexer_parser.py',272),
  ('expression_group -> LPAREN expression RPAREN','expression_group',3,'p_expression_group','lexer_parser.py',277),
  ('optional_expression_list -> empty','optional_expression_list',1,'p_optional_expression_list','lexer_parser.py',282),
  ('optional_expression_list -> expression_list','optional_expression_list',1,'p_optional_expression_list','lexer_parser.py',283),
  ('expression_list -> expression','expression_list',1,'p_expression_list','lexer_parser.py',288),
  ('expression_list -> expression COMMA expression_list','expression_list',3,'p_expression_list','lexer_parser.py',289),
]