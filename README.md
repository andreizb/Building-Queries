# Building-Queries

Project used to build basic queries, using Haskell. The project is used to practice Haskell coding.

## Description

The program evaluates the result of applying certain queries on given tables. Three tables and some queries meant for testing, including their outputs are provided in order to understand the format and expected output.

## Provided queries

The following queries are available:

* Atom - The most basic query, it takes a table as its argument.
* Select - It takes a list of strings and a query as its arguments. It returns a table resulted from evaluating the query received that contains only the columns specified in the list of strings received.
* SelectLimit - Same as Select, but it also receives an argument that states how many entries will be part of the resulted table.
* Filter - Basic filtering of a table, it receives a FilterCondition and a query as its arguments. The FilterConditions available are: Lt (checks if the value is LESS than a given value), Eq (checks the equality, similar to Lt), In (checks if the value is part of a given list of values), Not (the negation of a given FilterCondition).
* Reunion - it takes two tables with THE SAME header and returns the table resulted from the reunion of those two tables, by appending them. 
* Cosine - It's a query designed to detect the similarities between two users, based on their movie preferences.
