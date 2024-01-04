type token =
  | VAR of (string)
  | INT of (int)
  | PLUS
  | MINUS
  | ASTERISK
  | SLASH
  | EQUAL
  | LESS
  | GREATER
  | NOTEQUAL
  | COLCOL
  | LPAREN
  | RPAREN
  | LBRA
  | RBRA
  | ARROW
  | VBAR
  | SEMICOL
  | TRUE
  | FALSE
  | FUN
  | LET
  | REC
  | IN
  | IF
  | THEN
  | ELSE
  | MATCH
  | WITH
  | HEAD
  | TAIL
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "lib/parser.mly"
open Syntax
# 40 "lib/parser.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* ASTERISK *);
  262 (* SLASH *);
  263 (* EQUAL *);
  264 (* LESS *);
  265 (* GREATER *);
  266 (* NOTEQUAL *);
  267 (* COLCOL *);
  268 (* LPAREN *);
  269 (* RPAREN *);
  270 (* LBRA *);
  271 (* RBRA *);
  272 (* ARROW *);
  273 (* VBAR *);
  274 (* SEMICOL *);
  275 (* TRUE *);
  276 (* FALSE *);
  277 (* FUN *);
  278 (* LET *);
  279 (* REC *);
  280 (* IN *);
  281 (* IF *);
  282 (* THEN *);
  283 (* ELSE *);
  284 (* MATCH *);
  285 (* WITH *);
  286 (* HEAD *);
  287 (* TAIL *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\005\000\
\005\000\006\000\006\000\006\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\003\000\001\000\001\000\001\000\001\000\
\002\000\003\000\003\000\001\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\004\000\006\000\008\000\006\000\004\000\001\000\003\000\
\005\000\001\000\001\000\001\000\001\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\031\000\005\000\006\000\000\000\000\000\000\000\
\007\000\008\000\000\000\000\000\000\000\000\000\000\000\000\000\
\040\000\000\000\012\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\013\000\011\000\000\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\
\034\000\035\000\000\000\036\000\037\000\000\000\000\000\000\000\
\000\000\000\000\038\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\017\000\023\000\024\000\019\000\070\000\071\000"

let yysindex = "\008\000\
\164\255\000\000\000\000\000\000\000\000\164\255\164\255\125\255\
\000\000\000\000\010\255\001\255\164\255\164\255\048\255\048\255\
\000\000\158\001\000\000\048\255\234\255\000\000\023\002\011\255\
\009\255\020\255\016\255\205\001\205\255\000\000\000\000\164\255\
\164\255\164\255\164\255\164\255\164\255\164\255\164\255\164\255\
\000\000\000\000\000\000\164\255\000\000\164\255\164\255\028\255\
\164\255\129\255\002\255\002\255\048\255\048\255\063\002\063\002\
\063\002\063\002\063\002\000\000\043\002\231\001\023\255\178\001\
\000\000\000\000\025\255\000\000\000\000\024\255\255\254\164\255\
\164\255\164\255\000\000\129\255\129\255\164\255\043\002\255\001\
\043\002\007\255\031\255\043\002\164\255\164\255\043\002\043\002"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\028\000\000\000\000\000\029\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\032\255\000\000\000\000\000\000\000\000\
\000\000\000\000\109\000\136\000\055\000\082\000\159\000\182\000\
\205\000\228\000\251\000\000\000\017\001\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\040\001\000\000\
\067\001\000\000\027\255\094\001\000\000\000\000\114\001\129\001"

let yygindex = "\000\000\
\000\000\255\255\009\000\036\000\000\000\199\255"

let yytablesize = 851
let yytable = "\018\000\
\030\000\026\000\004\000\005\000\020\000\021\000\034\000\035\000\
\001\000\077\000\025\000\028\000\029\000\007\000\078\000\008\000\
\048\000\077\000\082\000\083\000\009\000\010\000\086\000\027\000\
\046\000\045\000\047\000\014\000\063\000\073\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\075\000\
\076\000\077\000\039\000\002\000\061\000\062\000\003\000\064\000\
\004\000\005\000\030\000\031\000\060\000\042\000\017\000\042\000\
\042\000\000\000\042\000\007\000\000\000\008\000\000\000\042\000\
\042\000\000\000\009\000\010\000\000\000\000\000\079\000\080\000\
\081\000\000\000\000\000\000\000\084\000\000\000\000\000\000\000\
\000\000\018\000\000\000\087\000\088\000\000\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\000\000\
\042\000\042\000\000\000\042\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\015\000\000\000\000\000\000\000\
\000\000\000\000\042\000\042\000\042\000\000\000\000\000\042\000\
\000\000\000\000\042\000\042\000\003\000\004\000\005\000\000\000\
\006\000\065\000\066\000\000\000\000\000\000\000\000\000\016\000\
\007\000\000\000\008\000\022\000\000\000\000\000\067\000\009\000\
\010\000\011\000\012\000\068\000\069\000\013\000\000\000\000\000\
\014\000\000\000\015\000\016\000\000\000\000\000\019\000\000\000\
\000\000\000\000\000\000\003\000\004\000\005\000\000\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\000\000\008\000\000\000\000\000\000\000\020\000\009\000\010\000\
\011\000\012\000\000\000\000\000\013\000\000\000\000\000\014\000\
\000\000\015\000\016\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\021\000\004\000\005\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000\
\007\000\000\000\008\000\000\000\000\000\000\000\000\000\009\000\
\010\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\000\000\050\000\004\000\005\000\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\007\000\043\000\008\000\
\000\000\000\000\023\000\000\000\009\000\010\000\000\000\000\000\
\000\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
\026\000\000\000\030\000\030\000\030\000\000\000\000\000\000\000\
\030\000\000\000\030\000\030\000\000\000\030\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\027\000\
\014\000\000\000\014\000\000\000\014\000\014\000\000\000\000\000\
\000\000\000\000\000\000\014\000\000\000\014\000\014\000\000\000\
\014\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\029\000\017\000\000\000\017\000\000\000\017\000\
\017\000\000\000\000\000\000\000\000\000\000\000\017\000\000\000\
\017\000\017\000\000\000\017\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\032\000\018\000\000\000\
\018\000\000\000\018\000\018\000\000\000\000\000\000\000\000\000\
\000\000\018\000\000\000\018\000\018\000\000\000\018\000\015\000\
\015\000\028\000\000\000\015\000\015\000\015\000\015\000\015\000\
\000\000\015\000\000\000\015\000\000\000\015\000\015\000\000\000\
\033\000\000\000\000\000\000\000\015\000\000\000\015\000\015\000\
\000\000\015\000\016\000\016\000\000\000\000\000\016\000\016\000\
\016\000\016\000\016\000\000\000\016\000\000\000\016\000\000\000\
\016\000\016\000\000\000\000\000\000\000\041\000\000\000\016\000\
\000\000\016\000\016\000\000\000\016\000\019\000\019\000\019\000\
\019\000\000\000\000\000\019\000\000\000\019\000\000\000\019\000\
\019\000\000\000\000\000\000\000\000\000\000\000\019\000\000\000\
\019\000\019\000\000\000\019\000\020\000\020\000\020\000\020\000\
\000\000\000\000\020\000\000\000\020\000\000\000\020\000\020\000\
\000\000\000\000\000\000\000\000\000\000\020\000\000\000\020\000\
\020\000\000\000\020\000\021\000\021\000\021\000\021\000\000\000\
\000\000\021\000\000\000\021\000\000\000\021\000\021\000\000\000\
\000\000\000\000\000\000\000\000\021\000\000\000\021\000\021\000\
\000\000\021\000\022\000\022\000\022\000\022\000\000\000\000\000\
\022\000\000\000\022\000\000\000\022\000\022\000\000\000\000\000\
\000\000\000\000\000\000\022\000\000\000\022\000\022\000\000\000\
\022\000\023\000\023\000\023\000\023\000\000\000\000\000\023\000\
\000\000\023\000\000\000\023\000\023\000\000\000\000\000\000\000\
\000\000\000\000\023\000\000\000\023\000\023\000\000\000\023\000\
\000\000\000\000\000\000\000\000\000\000\026\000\000\000\026\000\
\000\000\026\000\026\000\000\000\000\000\000\000\000\000\000\000\
\026\000\000\000\026\000\026\000\000\000\026\000\000\000\000\000\
\000\000\000\000\000\000\000\000\027\000\000\000\027\000\000\000\
\027\000\027\000\000\000\000\000\000\000\000\000\000\000\027\000\
\000\000\027\000\027\000\000\000\027\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\000\000\029\000\000\000\029\000\029\000\000\000\000\000\000\000\
\000\000\000\000\029\000\000\000\029\000\029\000\000\000\029\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\032\000\000\000\032\000\000\000\032\000\032\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\032\000\
\032\000\000\000\032\000\000\000\000\000\000\000\028\000\000\000\
\028\000\000\000\028\000\028\000\000\000\000\000\000\000\000\000\
\000\000\028\000\000\000\028\000\028\000\033\000\028\000\033\000\
\000\000\033\000\033\000\000\000\000\000\000\000\000\000\000\000\
\033\000\000\000\033\000\033\000\000\000\033\000\004\000\005\000\
\032\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\040\000\007\000\000\000\008\000\000\000\000\000\000\000\000\000\
\009\000\010\000\004\000\005\000\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\007\000\000\000\008\000\
\000\000\000\000\000\000\000\000\009\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\074\000\004\000\005\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000\
\007\000\000\000\008\000\000\000\000\000\000\000\000\000\009\000\
\010\000\000\000\000\000\000\000\000\000\000\000\049\000\004\000\
\005\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\007\000\000\000\008\000\000\000\000\000\000\000\
\000\000\009\000\010\000\000\000\000\000\000\000\072\000\004\000\
\005\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\007\000\000\000\008\000\000\000\000\000\000\000\
\000\000\009\000\010\000\000\000\000\000\000\000\085\000\004\000\
\005\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\007\000\000\000\008\000\000\000\000\000\000\000\
\044\000\009\000\010\000\004\000\005\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\040\000\007\000\000\000\
\008\000\000\000\000\000\000\000\000\000\009\000\010\000\004\000\
\005\000\032\000\033\000\034\000\035\000\000\000\000\000\000\000\
\000\000\040\000\007\000\000\000\008\000\000\000\000\000\000\000\
\000\000\009\000\010\000"

let yycheck = "\001\000\
\000\000\001\001\001\001\002\001\006\000\007\000\005\001\006\001\
\001\000\011\001\001\001\013\000\014\000\012\001\016\001\014\001\
\001\001\011\001\076\000\077\000\019\001\020\001\016\001\023\001\
\016\001\015\001\007\001\000\000\001\001\007\001\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\040\000\015\001\
\017\001\011\001\016\001\015\001\046\000\047\000\015\001\049\000\
\001\001\002\001\015\000\016\000\044\000\018\000\000\000\020\000\
\021\000\255\255\023\000\012\001\255\255\014\001\255\255\028\000\
\029\000\255\255\019\001\020\001\255\255\255\255\072\000\073\000\
\074\000\255\255\255\255\255\255\078\000\255\255\255\255\255\255\
\255\255\000\000\255\255\085\000\086\000\255\255\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\255\255\
\061\000\062\000\255\255\064\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\079\000\080\000\081\000\255\255\255\255\084\000\
\255\255\255\255\087\000\088\000\000\001\001\001\002\001\255\255\
\004\001\001\001\002\001\255\255\255\255\255\255\255\255\000\000\
\012\001\255\255\014\001\015\001\255\255\255\255\014\001\019\001\
\020\001\021\001\022\001\019\001\020\001\025\001\255\255\255\255\
\028\001\255\255\030\001\031\001\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\000\001\001\001\002\001\255\255\004\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\012\001\
\255\255\014\001\255\255\255\255\255\255\000\000\019\001\020\001\
\021\001\022\001\255\255\255\255\025\001\255\255\255\255\028\001\
\255\255\030\001\031\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\255\255\014\001\255\255\255\255\255\255\255\255\019\001\
\020\001\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\029\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\255\255\255\255\000\000\255\255\019\001\020\001\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\000\000\255\255\018\001\019\001\020\001\255\255\255\255\255\255\
\024\001\255\255\026\001\027\001\255\255\029\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\000\000\
\013\001\255\255\015\001\255\255\017\001\018\001\255\255\255\255\
\255\255\255\255\255\255\024\001\255\255\026\001\027\001\255\255\
\029\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\000\000\013\001\255\255\015\001\255\255\017\001\
\018\001\255\255\255\255\255\255\255\255\255\255\024\001\255\255\
\026\001\027\001\255\255\029\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\000\000\013\001\255\255\
\015\001\255\255\017\001\018\001\255\255\255\255\255\255\255\255\
\255\255\024\001\255\255\026\001\027\001\255\255\029\001\003\001\
\004\001\000\000\255\255\007\001\008\001\009\001\010\001\011\001\
\255\255\013\001\255\255\015\001\255\255\017\001\018\001\255\255\
\000\000\255\255\255\255\255\255\024\001\255\255\026\001\027\001\
\255\255\029\001\003\001\004\001\255\255\255\255\007\001\008\001\
\009\001\010\001\011\001\255\255\013\001\255\255\015\001\255\255\
\017\001\018\001\255\255\255\255\255\255\000\000\255\255\024\001\
\255\255\026\001\027\001\255\255\029\001\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\255\255\015\001\255\255\017\001\
\018\001\255\255\255\255\255\255\255\255\255\255\024\001\255\255\
\026\001\027\001\255\255\029\001\007\001\008\001\009\001\010\001\
\255\255\255\255\013\001\255\255\015\001\255\255\017\001\018\001\
\255\255\255\255\255\255\255\255\255\255\024\001\255\255\026\001\
\027\001\255\255\029\001\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\255\255\015\001\255\255\017\001\018\001\255\255\
\255\255\255\255\255\255\255\255\024\001\255\255\026\001\027\001\
\255\255\029\001\007\001\008\001\009\001\010\001\255\255\255\255\
\013\001\255\255\015\001\255\255\017\001\018\001\255\255\255\255\
\255\255\255\255\255\255\024\001\255\255\026\001\027\001\255\255\
\029\001\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\255\255\015\001\255\255\017\001\018\001\255\255\255\255\255\255\
\255\255\255\255\024\001\255\255\026\001\027\001\255\255\029\001\
\255\255\255\255\255\255\255\255\255\255\013\001\255\255\015\001\
\255\255\017\001\018\001\255\255\255\255\255\255\255\255\255\255\
\024\001\255\255\026\001\027\001\255\255\029\001\255\255\255\255\
\255\255\255\255\255\255\255\255\013\001\255\255\015\001\255\255\
\017\001\018\001\255\255\255\255\255\255\255\255\255\255\024\001\
\255\255\026\001\027\001\255\255\029\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\013\001\
\255\255\015\001\255\255\017\001\018\001\255\255\255\255\255\255\
\255\255\255\255\024\001\255\255\026\001\027\001\255\255\029\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\013\001\255\255\015\001\255\255\017\001\018\001\
\255\255\255\255\255\255\255\255\255\255\024\001\255\255\026\001\
\027\001\255\255\029\001\255\255\255\255\255\255\013\001\255\255\
\015\001\255\255\017\001\018\001\255\255\255\255\255\255\255\255\
\255\255\024\001\255\255\026\001\027\001\013\001\029\001\015\001\
\255\255\017\001\018\001\255\255\255\255\255\255\255\255\255\255\
\024\001\255\255\026\001\027\001\255\255\029\001\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\255\255\014\001\255\255\255\255\255\255\255\255\
\019\001\020\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\255\255\014\001\
\255\255\255\255\255\255\255\255\019\001\020\001\255\255\255\255\
\255\255\255\255\255\255\255\255\027\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\255\255\014\001\255\255\255\255\255\255\255\255\019\001\
\020\001\255\255\255\255\255\255\255\255\255\255\026\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\014\001\255\255\255\255\255\255\
\255\255\019\001\020\001\255\255\255\255\255\255\024\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\014\001\255\255\255\255\255\255\
\255\255\019\001\020\001\255\255\255\255\255\255\024\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\014\001\255\255\255\255\255\255\
\018\001\019\001\020\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\255\255\
\014\001\255\255\255\255\255\255\255\255\019\001\020\001\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\255\255\255\255\
\255\255\011\001\012\001\255\255\014\001\255\255\255\255\255\255\
\255\255\019\001\020\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  ASTERISK\000\
  SLASH\000\
  EQUAL\000\
  LESS\000\
  GREATER\000\
  NOTEQUAL\000\
  COLCOL\000\
  LPAREN\000\
  RPAREN\000\
  LBRA\000\
  RBRA\000\
  ARROW\000\
  VBAR\000\
  SEMICOL\000\
  TRUE\000\
  FALSE\000\
  FUN\000\
  LET\000\
  REC\000\
  IN\000\
  IF\000\
  THEN\000\
  ELSE\000\
  MATCH\000\
  WITH\000\
  HEAD\000\
  TAIL\000\
  EOF\000\
  "

let yynames_block = "\
  VAR\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 60 "lib/parser.mly"
    ( _1 )
# 402 "lib/parser.ml"
               : Syntax.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 64 "lib/parser.mly"
        ( Cons(_1, Empty) )
# 409 "lib/parser.ml"
               : 'list_inner))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 65 "lib/parser.mly"
                ( Cons(_1, Empty) )
# 416 "lib/parser.ml"
               : 'list_inner))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_inner) in
    Obj.repr(
# 66 "lib/parser.mly"
                           ( Cons(_1, _3) )
# 424 "lib/parser.ml"
               : 'list_inner))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "lib/parser.mly"
    ( Var _1 )
# 431 "lib/parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 73 "lib/parser.mly"
    ( IntLit _1 )
# 438 "lib/parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "lib/parser.mly"
    ( BoolLit true )
# 444 "lib/parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "lib/parser.mly"
    ( BoolLit false )
# 450 "lib/parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "lib/parser.mly"
              ( Empty )
# 456 "lib/parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_inner) in
    Obj.repr(
# 83 "lib/parser.mly"
                         ( _2 )
# 463 "lib/parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 86 "lib/parser.mly"
    ( _2 )
# 470 "lib/parser.ml"
               : 'arg_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg_exp) in
    Obj.repr(
# 91 "lib/parser.mly"
    ( _1 )
# 477 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'arg_exp) in
    Obj.repr(
# 94 "lib/parser.mly"
    ( App (_1, _2) )
# 485 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 97 "lib/parser.mly"
    ( Minus (IntLit 0, _2) )
# 492 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 100 "lib/parser.mly"
    ( Plus (_1, _3) )
# 500 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 103 "lib/parser.mly"
    ( Minus (_1, _3) )
# 508 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 106 "lib/parser.mly"
    ( Times (_1, _3) )
# 516 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 109 "lib/parser.mly"
    ( Div (_1, _3) )
# 524 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 112 "lib/parser.mly"
    ( Eq (_1, _3) )
# 532 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 115 "lib/parser.mly"
    ( Less (_1, _3) )
# 540 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 118 "lib/parser.mly"
    ( Greater (_1, _3) )
# 548 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 121 "lib/parser.mly"
    ( Neq (_1, _3) )
# 556 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 124 "lib/parser.mly"
    ( Cons (_1, _3) )
# 564 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'arg_exp) in
    Obj.repr(
# 127 "lib/parser.mly"
    ( Head _2 )
# 571 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'arg_exp) in
    Obj.repr(
# 130 "lib/parser.mly"
    ( Tail _2 )
# 578 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 133 "lib/parser.mly"
    ( Fun (_2, _4) )
# 586 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 136 "lib/parser.mly"
    ( Let (_2, _4, _6) )
# 595 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 139 "lib/parser.mly"
    ( LetRec (_3, _4, _6, _8) )
# 605 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 142 "lib/parser.mly"
    ( If (_2, _4, _6) )
# 614 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cases_rev) in
    Obj.repr(
# 145 "lib/parser.mly"
    ( Match (_2, List.rev _4) )
# 622 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 148 "lib/parser.mly"
    ( 
      let message =
        Printf.sprintf 
          "parse error near characters %d-%d"
          (Parsing.symbol_start ())
	        (Parsing.symbol_end ())
	    in
	    failwith message
	  )
# 636 "lib/parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 161 "lib/parser.mly"
    ( [(_1, _3)] )
# 644 "lib/parser.ml"
               : 'cases_rev))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'cases_rev) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 164 "lib/parser.mly"
    ( (_3, _5) :: _1 )
# 653 "lib/parser.ml"
               : 'cases_rev))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 169 "lib/parser.mly"
    ( Var _1 )
# 660 "lib/parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 172 "lib/parser.mly"
    ( IntLit _1 )
# 667 "lib/parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 175 "lib/parser.mly"
    ( BoolLit true )
# 673 "lib/parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 178 "lib/parser.mly"
    ( BoolLit false )
# 679 "lib/parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 181 "lib/parser.mly"
    ( Empty )
# 685 "lib/parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 184 "lib/parser.mly"
    ( Cons (_1, _3) )
# 693 "lib/parser.ml"
               : 'pattern))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.exp)
