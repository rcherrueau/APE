module sql.guard

import schema
import row
import table

import Data.List

import Effect.State
import Effects


frag : (s : Schema) -> Eff () [STATE (Table s')]
                              [STATE (Table $ attrId :: (s `intersect` s'),
                                      Table $ attrId :: (s' \\ s))]
frag s = updateM (\t => frag s t)

defrag : Eff () [STATE (Table $ attrId :: s,
                        Table $ attrId :: s')]
                [STATE (Table $ s ++ s')]
defrag = updateM (\(tl, tr) => defrag tl tr)

query : (Table s -> a) -> Eff a [STATE (Table s)]
query q = do t <- get
             pure (q t)

queryL : (Table s -> a) -> Eff a [STATE (Table s, Table _)]
queryL q = do (l,_) <- get
              pure (q l)

queryR : (Table s -> a) -> Eff a [STATE (Table _, Table s)]
queryR q = do (_,r) <- get
              pure (q r)

-- Test
localApp : Eff (List String) [STATE (Table scAgenda)]
localApp = do q <- query (\db => let r1 = σ today db in
                                 let r2 = π [attrDate] r1 in
                                 group r2)
              pure q

cloudApp : Eff  (Table [attrDate,attrName]) [STATE (Table scAgenda)]
                                            [STATE (Table [attrId,attrDate],
                                                    Table [attrId,attrName,attrAddr])]
cloudApp = do frag [attrDate]
              qL <- queryL (\left => σ today left)
              qR <- queryR (\right => π [attrId,attrName]right)
              pure $ table.defrag qL qR


cloudApp = do frag [date]
              crypt [name]
              qL <- queryL (\left => group [date] left)
              qR <- queryR (\right => let r1 = σ bobName right in
                                      let r2 = σ workPlace r1 in
                                      π [id] r2)
              pure $ count [date] $ table.defrag qL qR
