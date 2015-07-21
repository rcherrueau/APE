module sql.table

import schema
import row
import Data.List

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

-- Projection
π : (s : Schema) -> Table s' -> Table (s `intersect` s')
π s []        = []
π s (r :: rs) = row.π s r :: π s rs

diff : (s : Schema) -> Table s' -> Table (s' \\ s)
diff s [] = []
diff s (r :: rs) = row.diff s r :: diff s rs

-- Selection
σ : (Row s -> Bool) -> Table s -> Table s
σ p []        = []
σ p (r :: rs) with (p r)
  σ p (r :: rs) | False = σ p rs
  σ p (r :: rs) | True  = r :: σ p rs

-- Grouping
group : Table (a :: s) -> List (type a)
group [] = []
group ((t |: _) :: rs) = t :: group rs

-- Fragmentation
frag : (s : Schema) -> Table s' -> (Table $ attrId :: (s `intersect` s'),
                                    Table $ attrId :: (s' \\ s))
frag s rs {s'} = let (left, right) = frag' in
                 let leftWithId    = zipWithId left in
                 let rightWithId   = zipWithId right in
                 (leftWithId, rightWithId)
  where
  frag' : (Table (intersect s s'), Table (s' \\ s))
  frag' = let left  = π s rs in
          let right = diff s rs in
          (left, right)

defrag : Table $ attrId :: s -> Table $ attrId :: s' -> Table (s ++ s')
defrag []        us  = []
defrag (r :: rs) us  = let id = get attrId r {p=Here} in
                       case σ (\u => head u == id) us of
                         []        => defrag rs us
                         (::) u _ => let r' = row.diff [attrId] r in
                                     let u' = row.diff [attrId] u in
                                     let ru = union r' u' in
                                     ru :: defrag rs us

-- Tests
tAgenda : Table scAgenda
tAgenda = [row1,row2,row3]

pi1 : Table [attrName]
pi1 = π [attrName] tAgenda

pi2 : Table [attrAddr]
pi2 = π [attrAddr] tAgenda

sl1 : Table scAgenda
sl1 = σ (\r => (get attrDate r) == "2015-07-08") tAgenda

today : {auto p : Elem attrDate s} -> Row s -> Bool
today r {p} = let date = get attrDate r {p=p} in
                  date == "2015-07-08"

sl2 : Table scAgenda
sl2 = σ today tAgenda

dif1 : Table scAgenda
dif1 = diff [] tAgenda

dif2 : Table []
dif2 = diff scAgenda tAgenda

dif3 : Table [attrDate, attrAddr]
dif3 = diff [attrName] tAgenda

frg1 : (Table [attrId,attrDate], Table [attrId,attrName,attrAddr])
frg1 = frag [attrDate] tAgenda

dfg1 : Table scAgenda
dfg1 = defrag (fst frg1) (snd frg1)
