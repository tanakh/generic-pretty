# generic-pretty

Pretty printing for Generic data

# Usage

Define your data types and derives `Generic`.
Then you can make instances for pretty printing automatically.

```hs
data Foo = Foo { fooA :: Int, fooB :: String } deriving Generic
instance Pretty Foo

data Bar a = Bar { barA :: Foo, barB :: a } deriving Generic
instance Pretty a => Pretty (Bar a)
```

Now, you can pretty print your value.

```hs
> prettyPrint $ Foo 123 "foo"
Foo { fooA = 123, fooB = "foo" }

> prettyPrint $ Bar (Foo 123 "foo") (Just True)
Bar { barA = Foo { fooA = 123
                 , fooB = "foo" }
    , barB = Just True }
```

By default, `generic-pretty` prints highlighted values.
If you do not want this behavior, you can use plain version of pretty printer.

[highlighted](https://raw.githubusercontent.com/tanakh/generic-pretty/master/img/terminal.png)
