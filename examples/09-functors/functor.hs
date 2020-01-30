
{- 
WHAT IS A FUNCTOR? 

Functor in Haskell is a kind of functional representation of different 
Types which can be mapped over. 

A functor is anything that can be mapped over. This is most commonly a list, but really it's any object that can be mapped over.

It is a high level concept of implementing polymorphism. 

Any type that has a map function is a functor.

Now the map function also needs to meet the following criteria.
> Identity Law  [1]

If map is given an identity function it must return the exact same object. Like so:

    functor.map(x => x) === functor

This one is hopefully straightforward.

> Composition Law  [1]

    functor.map(x => f(g(x))) === functor.map(g).map(f)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-}

{-
Types that can act like a box can be functors. You can think of a list as a box 
that has an infinite amount of little compartments and they can all be empty, one 
can be full and the others empty or a number of them can be full. 

So, what else has the properties of being like a box? 

For one, the Maybe a type. In a way, it's like a box that can either hold nothing, 
in which case it has the value of Nothing , or it can hold one item, like "HAHA" , 
in which case it has a value of Just "HAHA" . Here's how Maybe is a functor. 

instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
-}



{-
Again, notice how we wrote instance Functor Maybe where instead of instance 
Functor (Maybe m) where , like we did when we were dealing with Maybe and YesNo . 
Functor wants a type constructor that takes one type and not a concrete type. 
If you mentally replace the f s with Maybe s, fmap acts like a 

(a -> b) -> Maybe a -> Maybe b 

for this particular type, which looks OK. But if you replace f with (Maybe m) , 
then it would seem to act like a 
(a -> b) -> Maybe m a -> Maybe m b , 
which doesn't make any damn sense because Maybe takes just one type parameter. 

-}

{-
Anyway, the fmap implementation is pretty simple. If it's an empty value of Nothing , 
then just return a Nothing . 

If we map over an empty box, we get an empty box. It makes sense. 
Just like if we map over an empty list, we get back an empty list. 

If it's not an empty value, but rather a single value packed up in a Just , 
then we apply the function on the contents of the Just . 
-}


{-
http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

Functors
When a value is wrapped in a context, you can't apply a normal function to it:

This is where fmap comes in. fmap is from the street, fmap is hip to contexts. fmap knows how to apply functions to values that are wrapped in a context. For example, suppose you want to apply (+3) to Just 2. Use fmap:
-}  

main = do 

    print "--- using fmap ---"
    print $ fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")

    print $ fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing

    print $ fmap (*2) (Just 200)

    print $ fmap (*2) Nothing

    print $ fmap (+3) (Just 2)

    -- <$> is infix fmap 
    print "--- using fmap infix <$> ---"
    print $ (++ " HEY GUYS IM INSIDE THE JUST") <$> (Just "Something serious.")

    print $ (++ " HEY GUYS IM INSIDE THE JUST") <$> Nothing

    print $ (*2) <$> (Just 200)

    print $ (*2) <$> Nothing

    print $ (+3) <$> (Just 2)




