#|

Exercise 2.76: As a large system with generic operations
evolves, new types of data objects or new operations may be
needed. For each of the three strategies---generic
operations with explicit dispatch, data-directed style, and
message-passing-style---describe the changes that must be
made to a system in order to add new types or new
operations. Which organization would be most appropriate for
a system in which new types must often be added? Which would
be most appropriate for a system in which new operations
must often be added?

|#

#| Answer

For generic operations with explicit dispatch to add a type
go to each existing operation function and add code to
handle the new type. To add an operation simply create a new
function and have it handle the types you wish.

For data-directed style to add a new type simply create the
new type however you wish and then add a glue layer (the
external interface of that type) by registering the
constructors and selectors you wish that type to support. To
add a new operation go to each type add to the glue layer as
desired.

For message-passing style, to add a type create a new
constructor that uses the style (represents the object with
a function that processes messages). To add an operation
modify the constructor with a new message handler for the
new operation.

Explicit dispatch is worst for adding new types because the
details of how each type works would be scattered throughout
many operations. It may be best for adding new operations
because it requires no changes to existing code and allows
great flexibility of how different types are handled in a
very small space. For example, we haven't yet seen complex
operations that take multiple types or need to coerce types.

If types are often added either data-directed or message-
passing are somewhat equivalent -- you are creating a table
or types by operations. I'm guessing how complex it is to
define this table isn't dependent on the approach.

Basically both data-directed or message-passing are a
solution to the problems caused by the "default" way of
doing things, explicit dispatch.

Note: For debugging purposes representing data as procedures
seems harder to read.

|#
