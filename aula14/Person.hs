

module Person where


-- example

data Address
  = Address {
      _street :: String
    , _city :: String
    , _cep  :: String
    } deriving Show

data Person
  = Person {
      _name :: String
    , _age :: Int
    , _address :: Address
    } deriving Show


-- example

pex :: Person
pex = Person "João" 30 addex

addex :: Address
addex = Address "Rua A" "Nova Iorque" "123"
