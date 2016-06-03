#haskell-semver

Tool for comparing Haskell modules and determining wich kinds of changes have been made between two versions of the same module. The classification of these changes is done according to the rules for [Semantic Versioning](http://semver.org/)

## Installation

This project requires ___stack___ to build and execute. 
All information for installing stack can be found at their github page: [https://github.com/commercialhaskell/stack](https://github.com/commercialhaskell/stack)

In order to install the project, go to the folder in which this project is located using a terminal or console and run the following command:

    stack build

## Running the project
Running the project also needs to be done using the terminal or console. To do so, go to the folder of the project and execute the following command:
   	
   	stack exec semver [v] Mod1.hs Mod2.hs
   	
The optional parameter ___v___ activates verbosity mode.

___Mod1.hs___ and ___Mod2.hs___ are the path to the file of respectively the old and the new version of a Haskell module.

It is also possible to run the project using the provided test modules. This can either be done manual or by running the command:
	
	stack exec semver test

