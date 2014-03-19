module Schema

import SQLiteTypes

import Decidable.Equality
import Language.Reflection

%default total


infix 5 :::
data Attribute = (:::) String SQLiteType
%name Attribute attr,attr'

getName : Attribute -> String
getName (c:::_) = c

getTy : Attribute -> SQLiteType
getTy (_:::t) = t

attrEta : (attr : Attribute) -> (getName attr ::: getTy attr) = attr
attrEta (x ::: y) = refl

attrInj : (c ::: t = c' ::: t') -> (c=c', t=t')
attrInj refl = (refl, refl)

instance DecEq Attribute where
  decEq (x ::: y) (z ::: w) with (decEq x z, decEq y w)
    decEq (x ::: y) (x ::: y) | (Yes refl, Yes refl) = Yes refl
    decEq (x ::: y) (x ::: w) | (Yes refl, No prf)   = No $ prf . snd . attrInj
    decEq (x ::: y) (z ::: w) | (No prf, _) = No $ prf . fst . attrInj

data Schema = Nil | (::) Attribute Schema
%name Schema s,s'

append : (s1, s2 : Schema) -> Schema
append [] s2 = s2
append (attr :: s) s2 = attr :: (append s s2)

consNotEmpty : with Schema (Nil = a::s) -> _|_
consNotEmpty refl impossible

consInj : with Schema (a::s = a'::s') -> (a=a', s=s')
consInj refl = (refl, refl)

instance DecEq Schema where
  decEq []          []            = Yes refl
  decEq []          (attr :: s)   = No consNotEmpty
  decEq (attr :: s) []            = No $ consNotEmpty . sym
  decEq (attr :: s) (attr' :: s') with (decEq attr attr', decEq s s')
    decEq (attr :: s) (attr :: s) | (Yes refl, Yes refl) = Yes refl
    decEq (attr :: s) (attr' :: s') | (Yes a, No b) = No $ b . snd . consInj
    decEq (attr :: s) (attr' :: s') | (No a, _) = No $ a . fst . consInj

names : Schema -> List String
names [] = []
names ((n ::: _) :: s) = n :: names s


data HasCol : Schema -> Attribute -> Type where
  Here : HasCol (attr::s) attr
  There : HasCol s attr -> HasCol (attr'::s) attr

HasColNotEmpty : HasCol [] a -> _|_
HasColNotEmpty Here impossible
HasColNotEmpty (There _) impossible

instance Uninhabited (HasCol [] a) where
  uninhabited x = HasColNotEmpty x

decHasColLemma :  (HasCol s attr -> _|_) ->
                  (attr' = attr -> _|_) ->
                  HasCol (attr' :: s) attr -> _|_
decHasColLemma h1 h2 Here = h2 refl
decHasColLemma h1 h2 (There x) = h1 x

decHasCol : (s : Schema) -> (attr : Attribute) -> Dec (HasCol s attr)
decHasCol [] attr = No HasColNotEmpty
decHasCol (attr' :: s) attr with (decEq attr' attr)
  decHasCol (attr' :: s) attr' | (Yes refl) = Yes Here
  decHasCol (attr' :: s) attr | (No f) with (decHasCol s attr)
    decHasCol (attr' :: s) attr | (No f) | (Yes x) = Yes (There x)
    decHasCol (attr' :: s) attr | (No f) | (No g) = No $ \h => decHasColLemma g f h

%reflection
solveHasCol : Type -> Tactic
solveHasCol (HasCol (_::tl) attr) =
  Try (Refine "Here" `Seq` Solve)
      (Refine "There" `Seq` (Solve `Seq` solveHasCol (HasCol tl attr)))
solveHasCol (HasCol (append a b) attr) = Refine "Here" `Seq` Solve
solveHasCol (HasCol _ attr) = Refine "Here" `Seq` Solve

data SubSchema : Schema -> Schema -> Type where
  Empty : SubSchema [] s
  Head : (tailSub : SubSchema small large) ->
         (alsoThere : HasCol large attr) ->
         SubSchema (attr :: small) large

decSubSchema_lemma : ((SubSchema s s2) -> _|_) -> SubSchema (attr :: s) s2 -> _|_
decSubSchema_lemma f (Head tailSub alsoThere) = f tailSub

decSubSchema_lemma2 : ((HasCol s2 attr) -> _|_) -> (SubSchema (attr :: s) s2) -> _|_
decSubSchema_lemma2 f (Head tailSub alsoThere) = f alsoThere

emptySubSchema : SubSchema (attr::s1) [] -> _|_
emptySubSchema (Head tailSub alsoThere) = absurd alsoThere

decSubSchema : (s1, s2 : Schema) -> Dec (SubSchema s1 s2)
decSubSchema [] s2 = Yes Empty
decSubSchema (attr :: s) s2 with (decSubSchema s s2)
  decSubSchema (attr :: s) s2 | (Yes x) with (decHasCol s2 attr)
    decSubSchema (attr :: s) s2 | (Yes x) | (Yes y) = Yes (Head x y)
    decSubSchema (attr :: s) s2 | (Yes x) | (No f) = No $ decSubSchema_lemma2 f
  decSubSchema (attr :: s) s2 | (No f) = No $ decSubSchema_lemma f



HasColNamed : Schema -> String -> Type
HasColNamed s col = (t : SQLiteType ** HasCol s (col ::: t))

decHasColNamed_lemma : ((HasColNamed s col) -> _|_) -> ((col' = col) -> _|_) ->
                       (t ** HasCol ((col' ::: ty) :: s) (col ::: t)) -> _|_
decHasColNamed_lemma notThere notHere (ty ** Here)         = notHere refl
decHasColNamed_lemma notThere notHere (ty ** (There more)) = notThere (ty ** more)


decHasColNamed : (s : Schema) -> (col : String) -> Dec (HasColNamed s col)
decHasColNamed [] col = No $ \h => HasColNotEmpty (getProof h)
decHasColNamed ((col' ::: ty) :: s) col with (decEq col' col)
  decHasColNamed ((col ::: ty) :: s)  col | (Yes refl) = Yes (ty ** Here)
  decHasColNamed ((col' ::: ty) :: s) col | (No f) with (decHasColNamed s col)
    decHasColNamed ((col' ::: ty) :: s) col | (No f) | (Yes x) =
      Yes (getWitness x ** There (getProof x))
    decHasColNamed ((col' ::: ty) :: s) col | (No f) | (No g) = No (decHasColNamed_lemma g f)

colNames : Schema -> List String
colNames [] = []
colNames ((col ::: _) :: s) = col :: colNames s

disjoint : (s1, s2 : Schema) -> Type
disjoint [] s2 = ()
disjoint ((col ::: _) :: s1) s2 = (HasColNamed s2 col -> _|_, disjoint s1 s2)

decDisjoint : (s1, s2 : Schema) -> Dec (disjoint s1 s2)
decDisjoint [] s2 = Yes ()
decDisjoint ((col:::ty) :: s) s2 with (decHasColNamed s2 col)
  decDisjoint ((col:::ty) :: s) s2 | (Yes x) = No $ \h => fst h x
  decDisjoint ((col:::ty) :: s) s2 | (No f) with (decDisjoint s s2)
    decDisjoint ((col:::ty) :: s) s2 | (No f) | (Yes x) = Yes (f, x)
    decDisjoint ((col:::ty) :: s) s2 | (No f) | (No g) = No $ g . snd

data Disjointness : Type where
  Disjoint : Disjointness
  Overlap : (attr : Attribute) -> Disjointness

isDisjoint : (s1, s2 : Schema) -> Disjointness
isDisjoint [] s2 = Disjoint
isDisjoint (attr :: s) s2 with (decHasColNamed s2 (getName attr))
  isDisjoint (attr :: s) s2 | (Yes x) = Overlap attr
  isDisjoint (attr :: s) s2 | (No f) = isDisjoint s s2


