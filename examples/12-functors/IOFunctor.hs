
{-
  If some value has a type of, say, IO String, that means that it's an I/O action that, when performed, will go out into the real world and get some string for us, which it will yield as a result. 
  
  We can use <- in do syntax to bind that result to a name. We mentioned that I/O actions are like boxes with little feet that go out and fetch some value from the outside world for us. 
  
  We can inspect what they fetched, but after inspecting, we have to wrap the value back in IO. By thinking about this box with little feet analogy, we can see how IO acts like a functor.

  Let's see how IO is an instance of Functor. When we fmap a function over an I/O action, we want to get back an I/O action that does the same thing, but has our function applied over its result value.
-}
instance Functor IO where
  fmap f action = do
    result <- action
    return (f result)

