{-
https://wiki.haskell.org/Introduction_to_Haskell_IO/Actions

In fact, all IO actions will have a type IO a for some result type a. When an action doesn't provide any useful data back to the program the unit type (written ()) is used to denote the result. For programmers familiar with C, C++ or Java, this is similar to the return type of "void" in those languages. The IO actions mentioned above have the following types:

Print the string "hello" to the console:        IO ()
Read a line of input from the console:          IO String
Network connection www.google.com on port 80:   IO Socket
Read two lines of input from the terminal, interpret them as numbers, add them together and print out the result:              IO Int
A first-person shooter game that uses mouse movements as input and renders graphics to the screen:                         IO ()

-}


--------------------------------------------------------------------------------
{-
  Haskell only runs one IO action in a program, the action called main. This action should have the type IO (). The following haskell program will print out "hello":
-}
module Main where
main :: IO ()
main = putStrLn "hello"


--------------------------------------------------------------------------------
{-
Do Notation
You may be wondering how any Haskell program can do anything useful if it can only run a single IO action. As we saw earlier, IO actions can be very complex. We can combine many simple actions together to form more complicated actions. To combine actions together we use a do-block.

A do-block combines together two or more actions into a single action. When two IO actions are combined the result is an IO action that, when invoked, performs the first action and then performs the second action. Here's a simple example.

Main is an action that prints a line "hello" and then prints a line "world". If the first action had any side effects, those effects are visible to the second action when it is performed. For example, if a file is written in the first action and read in the second action, the change to the file will be visible to the read. Remember that IO actions can return results to the program. The result of a do-block is the result of the last action in the do block. In our example above, the last action (putStrLn "world") doesn't provide a useful result and so the type of the entire do-block is IO ().
-}
main :: IO ()
main = do
    putStrLn "hello"
    putStrLn "world"



--------------------------------------------------------------------------------
{-
  Do-blocks can also make use of the result of one action when constructing another action.

  This example uses the action getLine (getLine :: IO String) which reads a line of input from the console. The do-block makes an action that, when invoked, invokes the getLine, takes its result and invokes the action putStrLn ("you said: " ++ line) with the previous result bound to line.

  Notice that an arrow (<-) is used in the binding and not an equal sign (as is done when binding with let or where). The arrow indicates that the result of an action is being bound. The type of getLine is IO String, and the arrow binds the result of the action to line which will be of type String.
-}
main :: IO ()
main = do
    line <- getLine                                     -- line :: String
    putStrLn ("you said: " ++ line)


--------------------------------------------------------------------------------
{-
  Do-blocks allow multiple actions to be specified in a single block. The meaning of these multi-action blocks is identical to the nested example above: the bindings are made visible to all successive actions. The previous example can be rewritten more compactly as
-}
main :: IO ()
main = do
    putStrLn "Enter two lines"
    line1 <- getLine                                    -- line1 :: String
    line2 <- getLine                                    -- line2 :: String
    putStrLn ("you said: " ++ line1 ++ " and " ++ line2)



--------------------------------------------------------------------------------
{-
 Of course we are free to use other Haskell language features when writing our program. Instead of putting all of our actions in main we may want to factor some common operations out as separate actions or functions that build actions. For example, we may want to combine prompting and user input:
 
 Here we made a function promptLine which returns an action. The action prints a prompt (using putStr :: IO (), which prints a string without a newline character) and reads a line from the console. The result of the action is the result of the last action, getLine.
-}
promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine

main :: IO ()
main = do
    line1 <- promptLine "Enter a line: "                -- line1 :: String
    line2 <- promptLine "And another: "                 -- line2 :: String
    putStrLn ("you said: " ++ line1 ++ " and " ++ line2)



--------------------------------------------------------------------------------
{-
  Let's try to write a slightly more helpful function that reads two lines and returns both of them concatenated together:  
-}
promptTwoLines :: String -> String -> IO String
promptTwoLines prompt1 prompt2 = do
    line1 <- promptLine prompt1                         -- line1 :: String  
    line2 <- promptLine prompt2                         -- line2 :: String
    line1 ++ " and " ++ line2    -- ??

{-
  There's a problem here. We know how to prompt for and read in both lines of input, and we know how to combine those lines of input, but we don't have an action that results in the combined string. 
  
  Remember, do-blocks combine together actions and the result of the do-block is the result of the last action. line1 ++ " and " ++ line2 is a string, not an action resulting in a string and so cannot be used as the last line of the do-block. 
  
  What we need is a way to make an action that results in a particular value. This is exactly what the return function does. Return is a function that takes any type of value and makes an action that results in that value.
  
  return (line1 ++ " and " ++ line2) is an action of type IO String that doesn't affect the outside world in any way, but results in a string that combines line1 and line2.
-}
promptTwoLines :: String -> String -> IO String
promptTwoLines prompt1 prompt2 = do
    line1 <- promptLine prompt1                         -- line1 :: String
    line2 <- promptLine prompt2                         -- line2 :: String
    return (line1 ++ " and " ++ line2)

main :: IO ()
main = do
    both <- promptTwoLines "First line: " "Second line: "
    putStrLn ("you said " ++ both)



--------------------------------------------------------------------------------
{-
  Here's a very important point that many beginners get confused about: 
  
    return does not affect the control flow of the program! 
    return does not break the execution of the do-block. 
    return may occasionally be used in the middle of a do-block where it doesn't directly contribute to the result of the do-block. 
    
    return is simply a function that makes an action whose result is a particular value. In a sense it wraps up a value into an action.

  We can also use Haskell's control flow features such as if-then-else, case-of or recursion with actions. For example:
-}
main :: IO ()
main = do
    line <- promptLine "What do you want? "             -- line :: String
    if line == "wisdom"
        then putStrLn "No man is without enemies."
        else putStrLn ("I don't have any " ++ line)









