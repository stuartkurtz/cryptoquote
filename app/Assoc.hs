module Assoc where

-- Assoc key value -- an association list.
-- We will ensure and assume that each key occurs at most once in
-- an association list.

type Assoc key value = [(key,value)]

-- An empty association list

empty:: Assoc key value
empty = undefined

-- Return the Maybe-wrapped value associated with a key in
-- an association list, e.g.,
-- 
--    find 'a' [('a',1),('b',2),('c',3)] = Just 1
--    find 'd' [('a',1),('b',2),('c',3)] = Nothing

find :: Eq key => key -> Assoc key value 
     -> Maybe value
find = undefined

-- Return the value associated with a key in
-- an association list, returning the default value if the
-- key does not occur, e.g, 
-- 
--    findWithDefault 4 'a' [('a',1),('b',2),('c',3)] = 1
--    findWithDefault 4 'd' [('a',1),('b',2),('c',3)] = 4

findWithDefault :: Eq key => value -> key -> Assoc key value -> value
findWithDefault = undefined

-- Delete a key from an Association List, e.g,,
--
--    delete 'b' [('a',1),('b',2),('c',3)] = [('a',1),('c',3)]

delete :: Eq key => key 
          -> Assoc key value -> Assoc key value
delete = undefined

-- Insert a key value pair into an association list. Note that if the key is already
-- present, it should be deleted first to maintain the single-occurrence convention, e.g.,
--
--   insert 'd' 4 [('a',1),('b',2),('c',3)] = [('d',4),('a',1),('b',2),('c',3]
--   insert 'c' 4 [('a',1),('b',2),('c',3)] = [('c',4),('a',1),('b',2)]

insert :: (Eq key) => key -> value 
       -> Assoc key value -> Assoc key value
insert = undefined

-- Modify the value asssociated with a key in an AssociationList. If the key isn't
-- present, it should be bound to the provided default value, e.g.,
-- 
--    modifyWithDefault (+1) 1 'a' [('a',1),('b',2),('c',3)] = [('a',2),('b',2),('c',3)]
--    modifyWithDefault (+1) 1 'c' [('a',1),('b',2)] = [('a',1),('b',2),('c',1)]

modifyWithDefault :: Eq key => (value -> value) -> value -> key 
                  -> Assoc key value -> Assoc key value
modifyWithDefault = undefined

-- Does the given key occur in an Association List? E.g.,
-- 
--    'a' `isKeyOf` [('a',1),('b',2),('c',3)] = True
--    'd' `isKeyOf` [('a',1),('b',2),('c',3)] = False

isKeyOf :: Eq key => key -> Assoc key value -> Bool
isKeyOf = undefined

-- Does the given value occur in an Association List? E.g.,
-- 
--    3 `isValueOf` [('a',1),('b',2),('c',3)] = True
--    4 `isValueOf` [('a',1),('b',2),('c',3)] = False

isValueOf :: Eq value => value -> Assoc key value -> Bool
isValueOf = undefined