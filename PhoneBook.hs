module PhoneBook
  ( PhoneBook,
  )
where

import qualified Data.Map as Map

type Name = String

type PhoneNumber = String

type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
  [ ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

data LockerState = Taken | Free deriving (Show, Eq)

type LockerId = Int

type Code = String

type LockerMap = Map.Map LockerId (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) ->
      if state /= Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"