
# Features

Aplite-xi comes with a variety of features, some of them makes it quite unique along other programming languages. 

## Composability and Type Safety

The number one feature of Aplite is its type safety. A lot of error in programming are type error and poor boundary/error handling, in addition, a lot of pain and sweat could have been safe by simply having a type checker to work with the programmer. Aplite is a strongly (statically) typed language where all functions need to respect types. While this may seem rigid at first, taking power away from the user often guides the user to exactly what they need and prevent unnecessary errors. 

One major benefits of the strongly typed aspect of Aplite is that it makes softwares compose easily. The most difficult part of software intercorporating with each other is how to get them to communicate with each other without error. Wrapping objects in type and having explicit constructors enables softwares to communicate in a standard format, no more missing a comma in a connection string. 

Aplite emphasizes on composability (rather than say, optimal performace) because it is designed as a programming language for DevOps and infrastructure. In the age of cloud and microservices and Kubernetes, there are many many moving parts for any infrastructure, and they all communicate with each other, it is too easy to miss a field in a HTTP request and break the system. 
We can think about all the different pieces are like different shapes, each one irregular and doesn't fit with the others. What Aplite does is that it boxes each piece into a box with standard shapes, for example, a database have a url to call on, a standard list of commands etc. This way any kafka topics can easily write processed data to any database, because they have a standard protocal.

## Types and Implementations
A major theme in Aplite is the following slogan:
Make arguments explicit, hide the implementation.

This manifests in many different ways, the easiest is within the syntax of the language:
Types are the only information needed. ... need more work here.

This philosophy 

## Backends
Aplite has many many different backends. Traditionally, a PL's backend is either a complier or an interpreter. Aplite currently has two "classical interpreter backends", being Python and JavaScript (Deno). However, the real speciality of Aplit is its "nontraditional" backend. Aplite is designed so that any server which can communicate in the Aplite's standard HTTP protocal can effectively communicate with Aplite. A primary example (currently under development) of such backend is the 


## Frontend
In contrast, Aplite only has one frontend. The frontend is specifically designed to be "backend (implementation) invariant", that is, a piece of Aplite code should run correctly regardless if it is compiling to Python or Js, or communicating with a Kubernetes operator. Just as the implementations of functions, different backends are different implementations of how the code should run, and is "irrelevant" to the user. 

This type of separation of concern is also a fundamental philosophy of Aplite. One should be able to use libraries, backends that others have created without having to understanding the implementation details. The only thing they need to know is the type of the function.


## Imports, FFI, and Remote functions

Aplite currently support three different ways to introduce code of others, they are 
- Import: Importing functions of a Aplite library. 
- Foreign Function Interface (FFI): Introducing a function from the backend.
- Remote: Interacting with a function/operation defined on a remote server through Aplite's standard HTTP protocal.

Import is the standard import prodedure in all programming languages, prety self-explanatory.

FFI interaface is another staple of programming languages. The only thing to note here is that the user will have to specify the type of the function as well as the location of the file that defines the foreign function. Currently Aplite supports Javascript and Python FFI.

Remote is a unique feature of Aplite. A remote function is a function that is located at some remore server. A great example of when the user would like to run a remote function is running a machine learning algorithm on cloud, in which case the user would load the python file on cloud and send HTTP request to run it, and it sends the answer back automatically after it finishes its calculations. Aplite's backend automate the communication process so the user can "pretend" that the function is local.

