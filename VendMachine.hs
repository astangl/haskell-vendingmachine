-- Vending Machine Simulator
-- written 02/18/2009 by Alex Stangl for 5/2009 STL Lambda Lounge shootout

module VendMachine where

import Data.Char(chr, ord, toUpper)
import Data.List((\\), sortBy, stripPrefix)
import Data.Ord(comparing)

-- find change of specified total from l, if possible, using greedy, relatively
-- efficient algorithm. Use either flip or negation to reverse sort order.
getChange l total = findTotal (sortBy (flip $ comparing amount) l) [] total
findTotal [] _ total = Nothing
findTotal (x:xs) acc total =
  if (amount x) > total then findTotal (dropWhile (==x) xs) acc total
  else if (amount x) == total then Just (x:acc)
  else case findTotal xs (x:acc) (total- amount x) of
         Nothing -> findTotal (dropWhile (==x) xs) acc total
         Just a -> Just a

-- return slotname for slot: 0.. -> A..
slotname :: Int -> String
slotname slot = slotnamR (slot+1)
  where slotnamR slot = if slot <= 26 then chr(64+slot):""
                        else if rem == 0 then (slotnamR (quo-1)) ++ "Z"
                        else (slotnamR quo) ++ chr(64+rem):""
                        where rem = slot `mod` 26
                              quo = slot `div` 26

-- return slot number, given its name (ala Excel column names: A..Z == 0..25,
-- AA..AZ == 26..51, BA..BZ == 52..77, etc.)
slotnumber :: String -> Int
slotnumber s = slotnR 0 0 s
               where slotnR _ t [] = t
                     slotnR 0 0 (x:xs) = slotnR 1 (ord(x)-65) xs
                     slotnR l t (x:xs) = slotnR (l+1) (26*(t+1)+ord(x)-65) xs

-- break string up into list of commands delimited by space and/or comma
cmds :: String -> [String]
cmds s = let isSpace = (`elem` [' ', ',']) in
         case dropWhile isSpace s of
              "" -> []
              s' -> c : cmds s''
                    where (c, s'') = break isSpace s'

-- currency representation, either coins or bills
data Currency = Nickel | Dime | Quarter | Dollar
     deriving (Eq, Show)

-- return amount of currency, in cents
amount :: Currency -> Int
amount Nickel  = 5
amount Dime    = 10
amount Quarter = 25
amount Dollar  = 100

-- return display/input name of currency
name :: Currency -> String
name x = map toUpper $ show x


-- tuple representing machine's current state: inventory of coins and bills,
-- user's unspent total, count of vending items remaining in each slot
data MachineState = MachineState{coinbox :: [Currency],
                                 deposits :: [Currency],
                                 itemCounts :: [(Int, Int)]}

-- process machine transitions, taking initial state, list of commands, output
-- list, and returning tuple of new state and output
machine :: MachineState -> [String] -> [String] -> (MachineState, [String])
machine t [] os = (t, os)
machine t@(MachineState coinbox deposits itemCounts) (c:cs) os =
  case stripPrefix "GET-" c of
    Just a -> let slotnum = slotnumber a in
              if slotnum < length itemCounts then vend slotnum t cs os
              else machine t cs (os ++ ["REPORT INVALID PRODUCT CODE"])
    Nothing -> case c of
               "NICKEL" -> machine t {deposits = Nickel : deposits} cs os
               "DIME" -> machine t {deposits = Dime : deposits} cs os
               "QUARTER" -> machine t {deposits = Quarter : deposits} cs os
               "DOLLAR" -> machine t {deposits = Dollar : deposits} cs os
               "COIN-RETURN" -> machine t {deposits=[]} cs (os ++
                                           map name deposits)
               "SERVICE" -> service t cs os
               _ -> machine t cs (os ++ ["REPORT DON'T UNDERSTAND " ++ c])


-- vend item
vend :: Int -> MachineState -> [String] -> [String] -> (MachineState, [String])
vend slot t@(MachineState coinbox deposits itemCounts) cs os =
  let unspent = sum $ map amount deposits
      newinv = if count==0 then Left ("REPORT " ++ (slotname slot) ++ " EMPTY")
               else Right ((take slot itemCounts) ++ [(count - 1, price)] ++
                           (drop (slot+1) itemCounts))
      (count, price) = itemCounts !! slot
  in case newinv of
    Left a -> machine t cs (os ++ [a])
    Right a -> if unspent < price then
                  machine t cs (os ++ ["REPORT INSUFFICIENT DEPOSIT"])
               else if unspent == price then
                  machine t {coinbox = coinbox ++ deposits, deposits=[],
                             itemCounts=a} cs (os ++ [slotname slot])
               else
                  let change = getChange (coinbox++deposits) (unspent-price)
                  in case (change) of
                    Nothing -> machine t cs (os++["REPORT USE EXACT CHANGE"])
                    Just c -> machine t {coinbox = ((coinbox++deposits) \\ c),
                                         deposits=[], itemCounts=a} cs 
                                         (os++(slotname slot):(map name c)) 

-- process service commands, output same as machine
service :: MachineState -> [String] -> [String] -> (MachineState, [String])
service t [] os = (t, os)
service t@(MachineState coinbox deposits itemCounts) (c:cs) os =
  case c of
    "DIAG" -> service t cs (os ++ ["Coinbox: " ++ show coinbox, "Deposits: " ++
                            show deposits, "Inventory: " ++ show itemCounts])
    "EMPTYMONEY" -> service t {coinbox=[]} cs os
    "RESTOCK" -> case cs of
                 [] -> service t cs (os++["REPORT RESTOCK MISSING ARGUMENTS"])
                 (c:cs) -> let num=(read c) in
                           if (length cs) < num*2 then
                             service t cs (os ++ ["REPORT RESTOCK SPECIFIED "
                                           ++ (show num) ++
                                           " SLOTS, REQUIRING " ++
                                           (show (num*2))
                                           ++ " ARGUMENTS, WHEREAS ONLY " ++
                                           show (length cs) ++ " ARE PRESENT."])
                           else
                             service t {itemCounts=(tupleize num [] $
                               map read (take (num*2) cs))} (drop (num*2) cs) os
                               where tupleize 0 o _ = o
                                     tupleize n o (qty:price:cs) =
                                       tupleize (n-1) (o++[(qty, price)]) cs

    _ -> service t cs (os ++ ["REPORT DON'T UNDERSTAND SERVICE " ++ c ++
                      "; ONLY UNDERSTAND DIAG, EMPTYMONEY, RESTOCK"])

-- run machine, starting off initially empty coinbox, empty deposits,
-- three each of 65 cents, $1.00 and $1.50 items
runMachine = vendmachine (MachineState [] [] [(3, 65), (3, 100), (3, 150)])

-- loop, parsing list of commands from stdin, sending it to machine,
-- displaying output, and then tail recursing
vendmachine :: MachineState -> IO MachineState
vendmachine i = do input <- getLine
                   let (newstate, strs) = machine i (cmds input) []
                   mapM (\x -> putStrLn ("-> " ++ x)) strs
                   vendmachine newstate
