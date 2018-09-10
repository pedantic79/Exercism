{-# LANGUAGE RankNTypes      #-}
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

import           Control.Lens       (Lens', Traversal', makeLenses, over, set,
                                     view)
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

-- renameStreets :: (String -> String) -> Person -> Person
renameStreets = over $ combine (address . street) (born . bornAt . street)

-- Based on zhenengxie's solution
-- https://exercism.io/tracks/haskell/exercises/lens-person/solutions/9a9c5003e2014c2cab6574eb31500c18
combine :: Lens' s a -> Lens' s a -> Traversal' s a
combine one two f x =
  (\a b -> set two b . set one a $ x) <$> f (view one x) <*> f (view two x)
