# Haskell

## GHCup
GHCup is a universal installer for Haskell that will install everything you need to program in Haskell, and will help you manage those installations (update, switch versions, …).

## Cabal or Stack
Both are Haskell build tools. Used to structure your Haskell projects, build them, run them, define dependencies, … .They handle the management of your toolchain (including GHC — the Glasgow Haskell Compiler — and, for Windows users, MSYS2), building and registering libraries, building build tool dependencies, and more.
Stack is just an alternative to Cabal.


### Steps For Learning Haskell
1. [Installation and Getting Started on Windows](https://www.youtube.com/watch?v=gLr2u6CjSsM)
2. [Haskell Documentation](https://www.haskell.org/)
3. [Hasekll Ghcup user guide](https://www.haskell.org/ghcup/)
4. [IntelliJ plugin for Haskell](https://github.com/rikvdkleij/intellij-haskell#installing-the-plugin)
5. [Vscode Debug plugin for Haskell](https://github.com/phoityne/hdx4vsc/tree/master)
6. [Basic Tutorial](https://www.youtube.com/watch?v=02_H3LjqMr8)
7. [Haskell Beginner Course 2022](https://youtube.com/playlist?list=PLOJjn67NeYg9cWA4hyIWcxfaeX64pwo1c)
8. [Course and Documentation for beginners](https://github.com/haskell-beginners-2022/course-plan?tab=readme-ov-file)
9. [Get the hang of language by solving HackerRank Problems using Haskell](https://www.youtube.com/playlist?list=PLguYJK7ydFE4aS8fq4D6DqjF6qsysxTnx)
10. [Create Minimal Servant server (check Haskel-Cabal-Minimal-Server-Project Markdown file)](https://www.youtube.com/watch?v=YYmxAHWrFR4&t=10s)
11. [Building a REST API with Haskell](https://dev.to/fabianveal/building-a-rest-api-with-haskell-2d54#resume-of-how-to-use-the-functors-and-applicatives)
12. [Cabal vs Stack (Important to Read)](https://gist.github.com/merijn/8152d561fb8b011f9313c48d876ceb07)
13. [Hoogle: Search engine](https://hoogle.haskell.org/)
14. [Stack Documentation](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md#external-dependencies)
15. [Another More theorized Haskell Course - Part 1]()
16. [Another More theorized Haskell Course - Part 2]()

## Course Recommended
[Haskell and Functional Programming course for complete beginners](https://github.com/haskell-beginners-2022/course-plan?tab=readme-ov-file)


## GHCi
is an interactive environments used for developing and testing Haskell code.

## GHCi (Glasgow Haskell Compiler interactive):
GHCi is an interactive command-line interface for Haskell.
It allows you to load Haskell modules, evaluate expressions, and interactively test and debug Haskell code.
You can start GHCi by running the ghci command in your terminal.
```
ghci
```
or
```
stack repl
```
Once inside GHCi, you can load Haskell modules using the `:load` or `:l` command, evaluate expressions, define functions, and more.

GHCi is a part of the GHC (Glasgow Haskell Compiler) toolchain, which is one of the most widely used Haskell compilers.

#### Stack REPL (Read-Eval-Print Loop):
Stack REPL is an interactive environment similar to GHCi but is integrated with the Stack build system.
It provides all the features of GHCi along with additional features such as managing project dependencies, building project-specific configurations, and ensuring reproducible builds.
You can start Stack REPL by running the `stack repl` command in your terminal within a Stack-managed Haskell project directory.
Stack REPL <b> loads your project's dependencies </b>  and provides an environment where you can interactively work with your project's code.

- Both GHCi and Stack REPL are useful tools for Haskell development, offering interactive environments for testing and exploring Haskell code. The choice between them often depends on the specific needs of your project and your familiarity with the Haskell ecosystem.


## Hoogle
Hoogle is a Haskell API search engine, which allows you to search the Haskell libraries on Stackage by either function name, or by approximate type signature.


## Interactive commands (ghci)

- `:info` is a GHCi command that provides information about a type, class, or function in Haskell.

- `:t` is a GHCi command that shows the type of an expression or function in Haskell.

- `:l {{filaneme}}` us a GHCi command that load a a file into ghci

- `:cd ` change directory inside the ghci

- `:q` quit the ghci


## Haskell Debugging
Check this [haskell debugging on stack](https://stackoverflow.com/questions/48208827/haskell-debug-plugin-in-intellij)

![Debugging](/images/debug.png)


## Leanring Curve
[Programming language learning curve](https://github.com/Dobiasd/articles/blob/master/programming_language_learning_curves.md)
![alt text](/images/haskell.png)


## Haskell OOP Paradigm
In Haskell, while it doesn't strictly adhere to the object-oriented programming (OOP) paradigm, it provides constructs to achieve similar goals through different mechanisms => Data, Instance, and Class:

### Data:
In Haskell, the data keyword is used to define new algebraic data types.
A data type (Product or Sum Type) declaration specifies the structure of a new type by listing its constructors and their types.
Constructors are like functions that build values of the new type.
For example:

```haskell
data Person = Person String Int
```
Here, Person is a new data type with a constructor of the same name. It takes a String (name) and an Int (age) as parameters.

### Class:
In Haskell, a "class" refers to a type class.
Type classes define a set of functions that types must implement.
When a type implements the functions specified by a type class, it is said to be an instance of that type class.
Type classes provide a way to achieve ad-hoc polymorphism in Haskell. For example:

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
```

Here, Eq is a type class with two methods == and /=. Types that implement these methods can be compared for equality meaning can use `==` or `/=` to compare them.

### Instance:
In Haskell, an "instance" typically refers to type class instances.
Haskell's type classes provide a way to define interfaces or behaviors that types can implement.
An instance declaration is used to specify how a particular type implements the functions defined in a type class.
For example:

```haskell
-- Define an instance of the `Eq` type class for `Person`
instance Eq Person where
    -- Define the equality operator `(==)` for `Person`
    -- It takes two `Person` values and checks if their name and age attributes are equal.
    (Person name1 age1) == (Person name2 age2) = (name1 == name2) && (age1 == age2)
```

```haskell
john = Person "John" 30
jane = Person "Jane" 25
john2 = Person "John" 30

-- Comparing persons for equality
print $ john == jane  -- False
print $ john == john2 -- True
```

Here, Eq is a type class that defines a method `==` for equating values. We're defining an instance of Eq for the Person type. In the instance declaration, we implement the `(==)` function, which checks if two `Person` values are equal based on their name and age attributes. If both the name and age are equal, the `Person` values are considered equal.

In summary, in Haskell:

- Data defines new types and their constructors.
- Instance declarations specify how types implement functions defined in type classes.
- Class declarations define type classes, which are interfaces specifying functions that types must implement to be instances of the class.

These concepts provide a way to achieve abstraction, polymorphism, and encapsulation in Haskell, similar to OOP principles but with a different syntax and underlying mechanisms.

![dataVsIntstanceVsClass](/images/data_Vs_class_Vs_instance.png)


# Table of Contents
- [query Vs query_ Vs execute:](#query-vs-query_-vs-execute)
- [FromRow and Functor Operator (<$>) and Applicative Operator <*>](#fromrow-and-functor-operator--and-applicative-operator)
- [Object in aeson library](#object-in-aeson-library)
- [Combine Operator (</>)](#combine-operator)
- [Type Application (help in fixing type mismatch errors)](#type-application-help-in-fixing-type-mismatch-errors)
- [Pragma {-# ___ #-} (Glasgow Haskell Compiler pragma)](#pragma---___---glasgow-haskell-compiler-pragma)
    - [{-# LANGUAGE TypeApplications #-}](#language-typeapplications)
- [try (Control.Exception)](#try-controlexception)
- [GHCup cache](#ghcup-cache)
- [Return multiple elements from function](#return-multiple-elements-from-function)
- [Eta-reduction](#eta-reduction)
- [Function Composition](#function-composition)
- [data vs newtype](#data-vs-newtype)
- [Polymorphism](#polymorphism)
    - [Parametric Polymorphism](#parametric-polymorphism)
    - [Ad-hoc Polymorphism](#ad-hoc-polymorphism)
- [Type Constructor](#type-constructor)
    - [Algebraic Data Types:](#algebraic-data-types)
        - [Sum Types:](#sum-types)
        - [Product Types:](#product-types)
        - [Parameterized Types:](#parameterized-types)
        - [Type Classes:](#type-classes)
        - [Higher-Kinded Types:](#higher-kinded-types)
- [Data Types (Product types Vs SumType)](#data-types-product-types-vs-sumtype)
    - [Product Type](#product-type)
    - [Sum Type](#sum-type)
- [Polymorphic](#polymorphic)
    - [Maybe a:](#maybe-a)
    - [Either a b:](#either-a-b)
- [Monads](#monads)
    - [IO vs Monad (side-effects)](#io-vs-monad-side-effects)
        - [Monadic Bind (>>=):](#monadic-bind)
    - [Monad type class hierarchy](#monad-type-class-hierarchy)
        - [Functor (Functor f):](#functor-functor-f)
        - [Applicative (Applicative f):](#applicative-applicative-f)
        - [Monad (Monad m):](#monad-monad-m)
    - [do notation](#do-notation)
    - [`<-` in do notation](#in-do-notation)
    - [`return` in do notation](#return-in-do-notation)
    - [`<-` &  return  & <$> Operator](#return---operator)
        - [<$> Vs <- with return](#vs---with-return)
        - [<$> (or fmap):](#or-fmap)
        - [<- with return:](#with-return)

# query Vs query_ Vs execute:
All can execute a @SELECT  @INSERT@  @UPDATE@, or other SQL query
- The `query` function is used to execute a **parameterized** query that **returns** a result set.
- The `query_` function is used for queries that **don't have any parameters** and **returns** a result set
- The `execute` function execute a query  that is **not expected to return results**.


# FromRow and Functor Operator (<$>) and Applicative Operator <*>
The `FromRow` typeclass is typically used in Haskell database libraries like postgresql-simple or mysql-simple.
It allows you to convert a row from a SQL query result into a Haskell data type.
The fromRow function would typically be implemented using the field function to extract values from the result row.
Each call to field extracts one column from the current row.

```haskell
instance FromRow Product where
  fromRow = Product  (<$>) field <*> field <*> field
```

1. field is used three times in sequence to extract values from the result row.
   The first field extracts an Int value for the product ID, the second field extracts a String value for the product name, and the third field extracts a Double value for the product price.
2. The `<$>` operator (also known as fmap) is used to apply the Product constructor to the values extracted by field.
   This constructs a Product value using the extracted values.
3. The `<*>` operator is used to sequence the extraction of values and application of the Product constructor.
   It's part of the Applicative typeclass and allows you to apply a function inside a context (in this case, the Product constructor)
   to values inside another context (in this case, the FieldParser context returned by each call to field).
4. Finally, the fromRow function is implemented to return a Product value constructed using the values extracted from the result row.

So, when you execute a SQL query and fetch a row representing a product, the fromRow instance will be used to convert that row into a Product value.
For example, if the result row contains 1, "Product A", 10.99, the fromRow instance will produce a Product value with productId 1, productName "Product A", and productPrice 10.99.

# Object in aeson library
Object is a specific data type provided by the aeson library in Haskell.
It's used to represent JSON objects.
It's not a class, class type, record, or newtype in the general sense of Haskell data structures. Instead, it's a specific type defined within the aeson library for working with JSON data.

In Haskell, JSON objects are typically represented using the Object type from the Data.Aeson.Types module. This type is defined as:
```haskell
newtype Object = Object { unObject :: HashMap Text Value }
```

Here, Object is a newtype wrapper around a HashMap from the unordered-containers package. It represents a JSON object as a mapping from Text keys to Value values.


# Combine Operator (</>)
`(</>)` = combine

# Type Application (help in fixing type mismatch errors)
- `@SomeException` syntax is a type application. In Haskell, type applications are used to specify a particular instance or type when Haskell's type inference system needs a little help. `SomeException` is a type that represents any exception.
- By using @SomeException with try, you're specifying that you want to catch any exceptions of type SomeException that may be thrown by the IO action writeContent2File. This helps to handle exceptions gracefully and continue execution without crashing.

# Pragma {-# ___ #-} (Glasgow Haskell Compiler pragma)
The line {-# OPTIONS_GHC -Wno-missing-signatures #-} is a GHC (Glasgow Haskell Compiler) pragma. Pragmas in Haskell are used to give additional instructions to the compiler. This specific pragma is used to suppress warnings about missing type signatures in your Haskell code.
## {-# LANGUAGE TypeApplications #-}
The {-# LANGUAGE TypeApplications #-} pragma enables the use of type applications with the @ syntax. It's a language extension that provides more flexibility in type annotations and makes type-level programming more convenient.

# try (Control.Exception)
The try function (in Control.Exception) wraps an IO a action and tries to execute it. If an exception is thrown during the execution, try returns Left with the exception, otherwise, it returns Right with the result of the action.

# GHCup cache
GHCup has a few caching mechanisms to avoid redownloads. All cached files end up in ~/.ghcup/cache by default.
If you experience problems, consider clearing the cache via ```ghcup gc --cache```.

# Return multiple elements from function
if you want to return multiple elements from a function use tuple


# Eta-reduction
f(x) = g(x) can be written as f = g. This is called eta-reduction.
```haskell
printType :: Typeable a => a -> m a
printType = print . typeOf
-- This is equivalent to: 
printType a = print (typeOf a)

-- ALSO
prod::[Int] -> [Int] -> Int
prod xs ys = zipWith (*) xs ys
-- This is equivalent to:
prod = zipWith (*)
```
# Function Composition
Function composition is a way to combine multiple functions into a single function: The dot `(.)` operator
```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x  -> f (g x)
```



# data vs newtype
- In Haskell, both `data` and `newtype` are used for defining new data types, but they have different implications and use cases.

Using `data`:
```haskell
data ProductName = ProductName
{ name :: String
} deriving (Show)
```
With this declaration, ProductName is defined as a new data type with a single constructor ProductName.
This constructor takes one field named name of type String. Using `data` allows for defining multiple constructors with different fields,
pattern matching, and creating instances of typeclasses like Eq, Ord, etc.

Using `newtype`:
```haskell
newtype ProductName = ProductName
{ name :: String
} deriving Show
```
With newtype, you're also defining a new data type, but with a significant restriction compared to data.
newtype can** only have one constructor with one field**. This restriction allows the compiler to optimize more aggressively because it knows there's only one possible representation.

# Polymorphism
## Parametric Polymorphism
same behavior for different types. Not caring what type it is but same behvior whatever the type is
examples:
- the first element of pair
- reversing a list
- list length
- taking 5 elements from a list
- function composition

```haskell
take 2 [True, False, True, False, True, False] -- [True, False]
take 2 "hello" -- ['h', 'e']
```
## Ad-hoc Polymorphism
different behavior for different types.
examples:
- Number addition
- Equality
- Compare two variables
- conversion to string
- parsing from a string

```haskell
class Display a where
  display :: a -> String

instance Display Bool where
  display True = "True"
  display False = "False"

instance Display Char where
  display c = [c]  -- ['h'] = "h"
```

# Type Constructor

In Haskell, a type constructor is a higher-order function that takes one or more types as arguments and returns a new type. It's used to define new data types or parameterize existing ones. Type constructors are a fundamental concept in Haskell's type system and are often associated with algebraic data types and type classes.

## Algebraic Data Types:
### Sum Types:
These are types that have multiple constructors, and you can think of them as "or" types.

```haskell
data Color = Red | Green | Blue
```
Here, Color is a type constructor, and Red, Green, and Blue are data constructors.

### Product Types:
These are types that have a single constructor with multiple fields, and you can think of them as "and" types.
```haskell
data Point = Point Float Float
```
Here, Point is a type constructor, and Point Float Float is its single data constructor with two fields.

### Parameterized Types:
Type constructors can take type parameters to create parameterized types.
```haskell
data Maybe a = Nothing | Just a
```
Here, Maybe is a type constructor that takes a type a as a parameter. Maybe Int, Maybe String, etc., are all valid types constructed using Maybe.

### Type Classes:
Type constructors can also be associated with type classes.
```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```
Here, f is a type constructor that is an instance of the Functor type class. This means that for any type a, f a can be mapped over using fmap.

### Higher-Kinded Types:
Some type constructors take other type constructors as parameters, leading to higher-kinded types.
```haskell
data Either a b = Left a | Right b
```
Here, Either is a type constructor that takes two type parameters, a and b, making it a higher-kinded type constructor.

Understanding type constructors is crucial for grasping Haskell's powerful type system, which ensures type safety and allows for expressive and concise code.


# Data Types (Product types Vs SumType)
## Product Type
In Haskell, a Product Type is a type formed by **combining** other types together. It represents a logical "and" relationship between its components.
```haskell
data User = UserConstructor Int String String String Int
  deriving (Show)

getUserName :: User -> String
getUserName (UserConstructor _ name _ _ _) = name

getUserEmail :: User -> String
getUserEmail (UserConstructor _ _ email _ _) = email

getUserPassword :: User -> String
getUserPassword (UserConstructor _ _ _ password _) = password

setUserPassword :: String -> User -> User
setUserPassword newPassword (UserConstructor userId name email _ age) = UserConstructor userId name email newPassword age
```
The `User` type in example is a product type because it combines several fields together to represent a user. Each field represents a piece of information about the user, and they are all necessary to fully describe a user.

## Sum Type
A Sum Type, on the other hand, represents a choice between several alternatives. It is also known as a "disjoint union" or "variant" type.
```haskell
data Result = Error String | Success String | Warning String
```
The Result type in example is a sum type because it represents a choice between different outcomes (error, success, warning).

# Polymorphic
Maybe and Either types are examples of polymorphic types, meaning they can hold values of different types.
## Maybe a:
This type represents an optional value. It can either be Nothing, indicating the absence of a value, or Just a, where a is the type of the value wrapped inside the Just constructor.
- Maybe Int: Represents a value that could be an integer or nothing at all.
- Maybe String: Represents a value that could be a string or nothing at all.
- Maybe Bool: Represents a value that could be a boolean or nothing at all.
```haskell  
-- Optional data type  
data Maybe a = Nothing | Just a
```

## Either a b:
This type represents a choice between two types, conventionally denoted as Left and Right. It's often used to represent computations that may result in an error (Left) or a successful result (Right).
- Either String Int: Represents a value that could either be a string (usually an error message) or an integer (a successful result).
- Either String [a]: Represents a value that could either be a string (an error message) or a list of some type a.
- Either (Maybe a) (Maybe b): Represents a value that could either be an optional value of type a or an optional value of type b.
```haskell  
-- Choice between two types
data Either a b = Left a | Right b
-- usually LEFT means error and RIGHT means success
```

# Monads
Monads allow sequencing of computations that can produce side effects, like reading from a file or performing IO operations.
In Haskell, IO actions and many other types are instances of the Monad type class.

Monads are a powerful abstraction used for
- sequencing computations
- managing side effects
- handling complex control flows in a pure functional language like Haskell.

## IO vs Monad (side-effects)
IO is a monad in Haskell that represents function with **side-effects** (not pure) computations that may perform input or output. It's a way to sequence and combine IO actions in a pure functional manner.

IO is a specific instance of the more general concept of a Monad. A Monad is a typeclass that represents computational contexts that can be sequenced and combined.

While all IO actions are monadic (i.e., can be sequenced and combined using monadic operations), not all Monad instances are IO. For example, Maybe and Either are also monads but don't involve any IO.

### Monadic Bind (>>=):
The key component of do notation is the monadic bind operator (>>=), which is used to sequence monadic actions. It takes a monadic value and a function that produces another monadic value, and it combines them into a single monadic action.
```haskell
andThen :: [a] -> (a -> [b]) -> [b]
andThen l f = case l of
    []     -> []
    x : xs -> f x ++ andThen xs f


class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b -- lists [] are Monads and 'andThen' is the name of bind operator >>=

instance Monad Maybe where
    return :: a -> Maybe a
    return x = Just x

    (>>=) :: Maybe a 
          -> (a -> Maybe b) 
          -> Maybe b
    Nothing >>= _ = Nothing
    Just x  >>= f = f x
```


## Monad type class hierarchy
Monad is a part of FAMily
```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where
    return :: a -> m a
    return = pure
    
    (>>=) :: m a -> (a -> m b) -> m b
```
Everyone uses pure instead of return nowadays

In Haskell's type class hierarchy, Applicative is indeed a **superclass** of Monad, not the other way around. Let's break down the relationships between these type classes:

### Functor (Functor f):
- Represents types f that support a mapping operation fmap, which transforms values inside the functor.
- No additional functionality beyond mapping.
```haskell
-- Define a simple Functor instance for Maybe
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap _ Nothing = Nothing

-- Usage
incrementMaybe :: Maybe Int -> Maybe Int
incrementMaybe mx = fmap (+1) mx

-- Example usage
main = do
    let result = incrementMaybe (Just 5)
    print result  -- Output: Just 6

```

### Applicative (Applicative f):
- Builds on Functor.
- Introduces the pure function, which lifts a value into the applicative context.
- Introduces the <*> operator, allowing application of a function within the applicative context to a value within the same context.
- Both Functor and Applicative are necessary for defining a monad, but Applicative provides additional capabilities beyond Functor.
```haskell
-- Define an Applicative instance for Maybe
instance Applicative Maybe where
    pure = Just
    Just f <*> mx = fmap f mx
    Nothing <*> _ = Nothing

-- Usage
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe mx my = (+) <$> mx <*> my

-- Example usage
main = do
    let result = addMaybe (Just 3) (Just 5)
    print result  -- Output: Just 8
```

### Monad (Monad m):
- Builds on Applicative.
- Introduces the return function (which is equivalent to pure in Haskell), lifting a value into the monadic context.
- Introduces the `>>=` operator, also known as the bind operator, allowing sequencing of monadic actions based on the result of previous actions.
```haskell
-- Define a Monad instance for Maybe
instance Monad Maybe where
    return = pure
    Just x >>= f = f x
    Nothing >>= _ = Nothing

-- Usage
divideMaybe :: Int -> Int -> Maybe Int
divideMaybe x y
    | y == 0 = Nothing
    | otherwise = Just (x `div` y)

-- Example usage
main = do
    let result = Just 10 >>= divideMaybe 5 >>= divideMaybe 2
    print result  -- Output: Just 1
```
Note: In the provided code snippet:

```haskell
class Applicative m => Monad m where
    return :: a -> m a
    return = pure
    
    (>>=) :: m a -> (a -> m b) -> m b
```
Monad is defined with Applicative m =>, indicating that any type m that is an instance of Monad must also be an instance of Applicative.
The return function is defined in terms of pure, showcasing the relationship between Monad and Applicative.
The (>>=) operator defines the bind operation, which allows sequencing of monadic actions.


## do notation
In Haskell, the do notation is **syntactic sugar** for writing imperative-style code in a monadic context.
The do notation is commonly used with monads to write code that looks like imperative programming, making it easier to understand and reason about, especially for those who are more familiar with imperative languages. In a do block, you can sequence monadic actions using the <- syntax. Each line in the do block represents a monadic action. When you use <- to bind the result of a monadic action to a variable, you're effectively extracting the value from the monad and working with it as if it were a regular value.

Here's a brief overview of how do notation works:

```haskell
import Data.Maybe

-- Example function using Maybe monad
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe mx my = do
  x <- mx  -- Extract value from mx
  y <- my  -- Extract value from my
  return (x + y)  -- Perform addition and wrap the result in Maybe

-- Usage
main = do
  let result = addMaybe (Just 3) (Just 5)
  print result  -- Output: Just 8
```
Note the result of the entire `do block` must be a monad of the same type. This is necessary to maintain the sequencing of actions and to ensure consistency within the monadic context.

## `<-` in do notation
The `<-` operator in do notation is used for monadic binding.
When you use <- in do notation, you bind the result(String/Variable/Value) of the monadic action on its right to a variable on its left.
`bind`, **deferred Action**, meaning **NOT** extraction but meaning I will perform some action on variable later, when this sction is done, I will be returning a Monad as well. Meaning using <- doesn't immediately extract the value from the monad; instead, it defers the action. It means "I will perform some action on this variable later, within the monadic context."

So, in essence, when you use <- in do notation:
- You're binding the result of a monadic action to a variable.
- You're deferring further action on that variable until later within the monadic context.
- The overall do block must result in a monad of the same type to maintain consistency and sequencing within the monadic context.


## `return` in do notation
In do notation, return is used to lift a value into a monadic context.
For IO, it creates an IO action that, when executed, will produce the given value.
However, in Haskell's do notation, return does not mean to exit the function or return a value like in imperative programming languages.

```haskell

returnExample :: IO ()
returnExample = do
let x = 42
return x
putStrLn "This will still be printed!"
```
In this example, return x creates an IO action that produces the value 42(Monadic 42), but it doesn't **exit** the function. putStrLn will still be executed afterward.

## `<-` &  return  & <$> Operator
### <$> Vs <- with return
Both `<$>` (or fmap) and `<- with return` are used to apply a function to the result of a monadic action, but they do it in different ways.

### <$> (or fmap):
Applies a pure function to the result inside the monadic context.
It's used when you have a pure function, and you want to apply it to a value inside a monad without having to explicitly use `do` notation.

```haskell
--- Note loadEnv :: IO (HM.HashMap T.Text T.Text)
getConfig key = HM.lookup key <$> loadEnv
```
Here, HM.lookup key is a pure function, and `<$>` applies it to the result of `loadEnv` (which is inside the IO monad).

### <- with return:
Binds the result of a monadic action to a variable and then uses `return` to put it back into the monadic context.

```haskell

getConfig key = do
envMap <- loadEnv
return $ HM.lookup key envMap
```
Here, `loadEnv` returns a value inside the IO monad. `<- loadEnv` binds that value to `envMap`, and then return puts `HM.lookup key envMap` back into the IO monad.


#   R e s t - J W T - H a s k e l l 
 
 
