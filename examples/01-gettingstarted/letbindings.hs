

module LetBindings where 

-- Let statement
-- do statements
--    let variable = expression
--    statements


-- -----------------------------------------------------------------------------
-- Let binding example 
-- -----------------------------------------------------------------------------
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h in
  let topArea = pi * r ^ 2 in sideArea + 2 * topArea


ex8 :: (RealFloat a) => [(a, a)] -> [a]
ex8 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]



main :: IO ()
main = do 

    -- ===========================================================================
    -- Let expressions
    --   let variable = expression in expression
    -- ===========================================================================
    -- Multiple bindings separated by ;
    print $ let foo = "Hey "; bar = "there!" in foo ++ bar

    print $ show ( (let x = 2 in x * 2) + 3)

    
    -- Let binding for pattern match 
    let (a, b, c) = (1, 2, 3)
    print $ show a

    -- Let binding example
    let cylinder r h = do {
      let sideArea = 2 * pi * r * h in
      let topArea = pi * r ^ 2 in sideArea + 2 * topArea
    }
    print $ cylinder 4.5 6.8

    -- Let binding inline
    print $ 4 * (let a = 9 in a + 1) + 2

    -- Let in list comprehension
    print $ ([(x, y) | x <- [1..3], let y = 2 * x ])


    let ex7 = (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
    print $ ex7

    print $ ex8 [(1.2 , 3.4)]

    -- Without the 'in' statement the ex9 var is available
    let fnEx9 x y z = x * y + z
    print $ fnEx9 1 2 3

    -- With the 'in' statement the ex10 var is unavailable
    -- let ex10 x y z = x * y + z 
    --   in ex10 3 4 2
    -- -- print $ ex10  error: Variable not in scope: ex10

    

    -- let a2 = do {
    --   [let square x = x * x in (square 5, square 3, square 2)]
    -- }
    let ex10 = 1 
        ex11 = 2
    print $ ex10 + ex11
    
    -- ============================================================================================
    -- Let vs Where
    -- The contrast here is that let introduces an expression, so it can be used wherever 
    -- you can have an expression, but where is a declaration and is bound to a surrounding 
    -- syntactic construct.
    -- ============================================================================================
    let printInc n = print plusTwo where plusTwo = n + 2
    printInc 2

    let printInc2 n = let plusTwo = n + 2 in print plusTwo
    printInc2 2

    -- Let expression
    print $ let x = 5; y = 6 in x * y
    -- can be rewritten as 
    let mult1 = x * y where x = 5; y = 6
    print $ mult1


    let ex1a = let x = 3; y = 1000 in x * 3 + y
    -- can be rewritten as
    let ex1b = x * 3 + y 
         where x = 3
               y = 1000
    print ex1a 
    print ex1b 

    
    let ex2a = let y = 10; x = 10 * 5 + y in x * 5
    -- can be rewritten as
    let ex2b = x * 5
         where y = 10
               x = 10 * 5 + y 
    print ex2a 
    print ex2b 

    
    let ex3a = let x = 7; y = negate x; z = y * 10 in z / x + y
    -- can be rewritten as
    let ex3b = z / x + y
         where x = 7
               y = negate x
               z = y * 10 
    print ex3a 
    print ex3b 
           





