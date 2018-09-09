{-# LANGUAGE TemplateHaskell #-}

module Person
  ( Address(..)
  , Born(..)
  , Name(..)
  , Person(..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import           Control.Lens       (Lens', makeLenses, over, set, view)
import           Data.Time.Calendar (Day)
import           Data.Time.Lens     (months)

data Person = Person
  { _name    :: Name
  , _born    :: Born
  , _address :: Address
  }

data Name = Name
  { _foreNames :: String
  , _surName   :: String
  }

data Born = Born
  { _bornAt :: Address
  , _bornOn :: Day
  }

data Address = Address
  { _street      :: String
  , _houseNumber :: Int
  , _place       :: String
  , _country     :: String
  }

makeLenses ''Person

makeLenses ''Name

makeLenses ''Born

makeLenses ''Address

bornStreet :: Born -> String
bornStreet = view (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set (address . street)

setBirthMonth :: Int -> Person -> Person
setBirthMonth = set (born . bornOn . months)

renameStreets :: (String -> String) -> Person -> Person
renameStreets = over (address . street ~.~ born . bornAt . street)

infixr 8 ~.~
(~.~) :: Monad f => (c -> d -> f a) -> (c -> a -> f b) -> c -> d -> f b
(~.~) a b f = (b f =<<) . a f
