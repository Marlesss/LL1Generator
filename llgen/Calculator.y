{=
module Calculator where
=}

%name calc E
%tokentype {= Token =}
%error {= parseError =}

%token
  n   /[[:digit:]]+/ {= TokenN NumT =}
  '(' /\(/           {= TokenLBr   =}
  ')' /\)/           {= TokenRBr   =}
  '+' /\+/           {= TokenPlus  =}
  '-' /-/            {= TokenMinus  =}
  '*' /\*/           {= TokenMult  =}
  '/' ///            {= TokenDiv  =}

%skip /[[:space:]]+/

%%

E : T Ef {= E $1 $2 =}

Ef : '+' T Ef {= Plus $2 $3 =}
   | '-' T Ef {= Minus $2 $3 =}
   |          {= NoEf =}

T : F Tf {= T $1 $2 =}

Tf : '*' F Tf {= Mult $2 $3 =}
   | '/' F Tf {= Div $2 $3 =}
   |          {= NoTf =}

F : '(' E ')' {= Brace $2 =}
  | n         {= N $1 =}

{=
type NumT = Double

data E = E T Ef deriving Show
data Ef
  = Plus T Ef
  | Minus T Ef
  | NoEf
  deriving Show
data T = T F Tf deriving Show
data Tf
  = Mult F Tf
  | Div F Tf
  | NoTf
  deriving Show
data F
  = Brace E
  | N NumT
  deriving Show

class Calc a where
  calc :: a -> NumT

instance Calc E where
  calc (E t ef) = (applyEf ef) (calc t)
    where
      applyEf ef = case ef of
        (Plus t ef2)  -> (applyEf ef2).(+ (calc t))
        (Minus t ef2) -> (applyEf ef2).(subtract (calc t))
        NoEf          -> id

instance Calc T where
  calc (T f tf) = (applyTf tf) (calc f)
    where
      applyTf tf = case tf of
        (Mult f tf2) -> (applyTf tf2).(* (calc f))
        (Div f tf2)  -> (applyTf tf2).(/ (calc f))
        NoTf         -> id

instance Calc F where
  calc (Brace e) = calc e
  calc (N x) = x

parseError tkns = error $ "Parse Error!!!: " ++ show tkns
=}