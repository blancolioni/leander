module UseGreeting where
-- Instances declared here, of imported classes and of the Prelude's, all
-- falling back on the class's own defaults.
import Greeting
import Hailing

data Pet = Cat | Dog

petTag :: Pet -> Int
petTag Cat = 0
petTag Dog = 1

instance Greeting Pet where
  greetName _ = "pet"

instance Hailing Pet where
  hailName _ = "pet"

instance Eq Pet where
  a == b = petTag a == petTag b

instance Ord Pet where
  compare a b = compare (petTag a) (petTag b)

instance Show Pet where
  show Cat = "Cat"
  show Dog = "Dog"

ugGreetBool = greet True == "hello bool"
ugGreetPet  = greet Cat == "hello pet"
ugHailPet   = hail Dog == "hi pet"
ugDefaults  = Cat /= Dog && Cat < Dog && max Cat Dog == Dog
ugShowList  = show [Cat, Dog] == "[Cat,Dog]"
