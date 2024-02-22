{
module Grammar where

import Lexer
}

%name parse Grammar
%tokentype { GrammarToken }
%error { parseError }

%token
  '%'      { GTPercent }
  '%%'     { GTDelim }
  ':'      { GTColon }
  '|'      { GTPipe }
  nonTerm  { GTNonTerm $$ }
  term     { GTTerm $$ }
  block    { GTBlock $$ }
  regex    { GTRegex $$ }

%%

Grammar : Header Directives '%%' Productions Trailer { Grammar $1 $2 $4 $5 }

Header : block { $1 }

Directives : list(snd('%', Directive)) { $1 }

Directive : term list(DirectiveArg) { Directive $1 $2 }

DirectiveArg
  : term    { Term $1 }
  | nonTerm { NonTerm $1 }
  | block   { Block $1 }
  | regex   { Regex $1 }

Productions : list(Production) { $1 }

Production : nonTerm ':' sep1(Rule, '|') { Prod $1 $3 }

Rule : list(or(term, nonTerm)) block { Rule $1 $2 }


Trailer : block { $1 }

or(p, q)
  : p { Left $1 }
  | q { Right $1 }

opt(p)
  : {- empty -} { Nothing }
  | p           { Just $1 }

fst(p,q)  : p q  { $1 }
snd(p,q)  : p q  { $2 }
both(p,q) : p q  { ($1,$2) }

rev_list1(p)
  : p              { [$1] }
  | rev_list1(p) p { $2 : $1 }

list1(p) : rev_list1(p) { reverse $1 }
list(p)
  : list1(p)    { $1 }
  | {- empty -} { [] }

sep1(p,q) : p list(snd(q,p)) { $1 : $2 }

{
data Grammar = Grammar Block [Directive] [Production] Block deriving Show
data Directive = Directive TermName [DirectiveArg] deriving Show
data DirectiveArg
  = Term TermName
  | NonTerm NonTermName
  | Block Block
  | Regex String
  deriving Show
data Production = Prod NonTermName [Rule] deriving Show
data Rule = Rule [Either TermName NonTermName] Block deriving Show

parseError tokens = error $ "Parse error: " ++ (show tokens)
}