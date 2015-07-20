module sql.table

import schema
import row

%default total

infixr 7 $$

data Table : Schema -> Type where
  TNil : Table s
  ($$) : Row s -> Table s -> Table s

-- utils
foldl : (acc -> Row s -> acc) -> acc -> Table s -> acc
foldl f z TNil      = z
foldl f z (r $$ rs) = foldl f (f z r) rs

reverse : Table s -> Table s
reverse ts = foldl (\z => \r => r $$ z) TNil ts

zipWithId : Table s -> Table (attrId :: s)
zipWithId rs = reverse $ fst $ table.foldl (\(rs,id) => \r =>
                                             ((id |: r) $$ rs, id + 1))
                                             (TNil, 0)
                                             rs
-- Selection
π : (s : Schema) -> Table s' -> Table (s `inter` s')
π s TNil = TNil
π s (r $$ rs) = row.π s r $$ π s rs

-- Projection
σ : (Row s -> Bool) -> Table s -> Table s
σ _ TNil                         = TNil
σ p (r $$ rs) with (p r) | True  = r $$ σ p rs
                         | False = σ p rs

-- Vertical fragmentation
frag : (s : Schema) -> Table s' -> (Table $ attrId :: (s `inter` s'),
                                    Table $ attrId :: (s' \\ s))
frag s rs = let (left, right) = frag_ rs in
            (zipWithId left, zipWithId right)
  where
  frag_ : Table s' -> (Table (s `inter` s'), Table (s' \\ s))
  frag_ TNil = (TNil, TNil)
  frag_ (r $$ rs) = let (rleft,  rright)  = row.frag s r in
                    let (rsleft, rsright) = frag_ rs in
                    (rleft $$ rsleft, rright $$ rsright)

-- Note: The following requires a proof that s' is a subset of s.
-- Henceforth, the proof should be give as argument, which is not as
-- practiable as defining `π` as the intersection of s' and s.
π' : (s' : Schema) -> Table s -> {auto p : s' `Sub` s} -> Table s'
π' s' TNil = TNil
π' s' (r $$ rs) {p=p'} = row.π' s' r {p=p'} $$ π' s' rs

frag' : (s' : Schema) -> Table s -> {auto p : s' `Sub` s} -> (Table $ attrId :: s',
                                                              Table $ attrId :: (s \\ s'))
frag' s' rs {p=p'} = let (left, right) = frag_ rs {p=p'} in
                   (zipWithId left, zipWithId right)
  where
  frag_ : Table s -> {auto p : s' `Sub` s} -> (Table s', Table (s \\ s'))
  frag_ TNil = (TNil, TNil)
  frag_ (r $$ rs) {p=p'} = let (rleft,  rright)  = row.frag' s' r {p=p'} in
                           let (rsleft, rsright) = frag_ rs in
                           (rleft $$ rsleft, rright $$ rsright)

-- Tests
tAgenda : Table scAgenda
tAgenda = row1 $$ row2 $$ row3 $$ TNil
