protobuf-native
===============

`protobuf-native` uses the code generated from protobuf for C++ in Haskell
to go between Haskell and protobuf data structures.

It makes use of Template Haskell to assist in generating the interface
between protobuf and your data structures.

Objects have finalizers so you never need to worry about memory management.

Usage
-----

```haskell
protobuf :: FilePath -> Name -> Q [Dec]
```

`protobuf` is a Template Haskell splice that takes the file path to a
compiled protobuf object file and the name of the data type you want to
build bindings to.

The data type must:

- Have a name the same as the protobuf message name with a `T` appended
- Be a record data structure
- Have a single constructor with the name of the data type with the final `T` omitted
- If a field's name is a reserved word, it may have an `_` appended

For example, if we have a Person protobuf structure in the file `person.proto`:

```
message Name {
  optional string firstname = 1;
  optional string lastname = 2;
}

message Person {
  required Name name = 1;
  required int32 id = 2;
  optional string email = 3;
}
```

First we run `protoc --cpp_out=. person.proto` then compile the `person.pb.cc`
file. **Unfortunately**, at this point, you need to mangle the C++ header file as
per the **Protobuf Mangling Guide** below, ideally this would be automated.
Do not re-run `protoc` unless you want to re-mangle the file.
Always check these files in to source control.

Then we can write two Haskell data structures to represent these types:

```haskell
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
import Data.Protobuf

data NameT = Name { firstname :: Maybe String, lastname :: Maybe String}
  deriving (Show, Eq)
protobuf "person.pb.o" ''NameT

data PersonT = Person { name :: NameT, id :: Int, email :: String }
  deriving (Show, Eq)
protobuf "person.pb.o" ''PersonT
```

If you get any of this wrong, you will get a compiler error.

Note that `NameT` is the type of the `name` field in `PersonT`. With *data
kinds* you may get confusing error messages here.

Now we can:

1. Construct a new Haskell `PersonT` value
2. Send the Haskell value to a protobuf struct with `assign`
3. Write it to a file using `writeProtobuf` which uses `SerializeToOstream`
4. Read that file using `loadProtobuf` which uses `ParseFromIstream`
5. Load that protobuf struct with `load`
6. Comapre the values

```haskell
main = do
  let val = (Person (Name (Just "Max") Nothing) 1 "maxwell.swadling@nicta.com.au")
  person <- newPb :: IO PersonPtr
  assign person val
  writeProtobuf "person.pb" person
  
  person2 <- new :: IO PersonPtr
  readProtobuf "person.pb" person2
  val2 <- derefPb person2
  print $ val == val2
```

The C++ are `ForeignPtr`s with finalizers, so you do not need to free anything.

Working with **Cabal** requires extra build steps. See this project's `Setup.hs`
for an example on how to run `protoc` and `clang++` in the build phase.

Testing
-------

The property above is used by QuickCheck to test the library works.
The tests are located in `tests/Tests.hs`.
Namely, for all types in `Protobuf` the following property holds:

```haskell
testProtobuf b = do
  x <- run $ do
    p <- newPb
    assign p b
    v <- derefPb p
    return $ b == v
  assert x
```

By profiling the tests you can verify the library does not leak memory.

Protobuf Mangling Guide
-----------------------

This is a temporary measure until we make a post-processor for protobuf header
files.

- Inline functions need to be un-inlined so they are linkable.
  If you see something like:

```
Exception when trying to run compile-time code:
  symbol "Name::clear_firstname()" missing from object file
Code: protobuf "tests/person.pb.o" ''NameT
```

This is an inline function that must be un-inlined. 

Go to the header file and search for that function name (in this
case `clear_firstname`). You will find two occurances:

```
inline void clear_firstname();
...
inline void Name::clear_firstname() {
  if (firstname_ != &::google::protobuf::internal::GetEmptyStringAlreadyInited()) {
...
```

Remove the `inline` from both of these.

- When you see the linker error that some function could not be found, it is
  probably still marked inline. Such as:

```
Undefined symbols for architecture x86_64:
  "Name::set_lastname(std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> > const&)", referenced from:
      _c73i_info in Person.o
```

Go to the header file and find `set_lastname` and remove the `inline`. It may also
look like an field name, such as:

```
"Name::lastname() const", referenced from:
      _c71z_info in Person.o
```

Remove the inline from:
```
inline const ::std::string& lastname() const;
```

When dealing with a string setter, you only need to un-inline the `std::string`
function. I.e.
```
  /*inline*/ void set_email(const ::std::string& value);
  inline void set_email(const char* value);
  inline void set_email(const char* value, size_t size);
```

- We currently exchange protobuf values via "set and delete". If you see the linker error that an `add_x(x *)` symbol is missing, then "set and delete" needs to
  be implemented for that field `x`. For example, if the `Graph::add_nodes(Node*)`
  function is missing we need to add the following function and declaration to the
  header file:

```
// in the class
void add_nodes(Node *);

// in the impl
void Graph::add_nodes(Node *x) {
  ::Node *n = nodes_.Add();
  *n = *x;
  delete x;
}
```

This is currently another work around that is fixable.

Related Work
------------

- http://hackage.haskell.org/package/protobuf lets you write Protobuf structures
  in plain Haskell.
- http://hackage.haskell.org/package/protocol-buffers generates Haskell code from
  the proto files.
- `protobuf-native` requries the user to both write proto files and Haskell
  structures. It does not require the user to write marshalling code.
  The main advantage is the ability to use C++ with these data structures.

If your problem has performance constraints you may want to consider using this
library. When working with large protobuf files, you may want to write file
iterators / network operations in C++ and process the data in Haskell.
This library lets you only pay for converting the parts of the data
structure you need. You can, for example, iterate over a large file 20 
elements at a time and only pull out the components of the protobuf structure
you need to pass to Haskell.

Future work
-----------

- The C++ protobuf implementations should be mangled automatically. A sufficiently
  complex awk program would suffice.
- Currently it is a lot of manual work writing data structures to match the
  protobuf files. We should parse the protobuf files and generate the
  data type definitions.
- Support unknown field data.
- It should support `Text`.
- It should use `Vector` instead of `[]` for `repeated` values.
