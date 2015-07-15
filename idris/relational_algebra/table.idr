module sql.table

import schema
import row

%default total

Table : Schema -> Type
Table s = List (Row s)

zipWithId : Table s -> Table (attrId :: s)
zipWithId rs = reverse $ fst $ foldl (\(rs,id) => \r =>
                                       ((id |: r) :: rs, (S id)))
                                     (Nil, Z)
                                     rs
-- zipWithId rs = let size = (toIntegerNat $ length rs) - 1 in
--                let zip = [0..size] in
--                zipWith (\id => \r => id |: r) zip rs ?=σ

-- Selection
π : (s: Schema) -> Table s' -> Table (s `inter` s')
π s []        = []
π s (r :: rs) = row.π s r :: π s rs

σ : (Row s -> Bool) -> Table s -> Table s
σ p []        = []
σ p (x :: xs) with (p x)
  σ p (x :: xs) | False = σ p xs
  σ p (x :: xs) | True  = x :: σ p xs



-- Tests
tAgenda : Table scAgenda
tAgenda = [row1,row2,row3]

-- pi1 : Table [attrDate,attrAddr]
-- pi1 ?= π [attrName] tAgenda
