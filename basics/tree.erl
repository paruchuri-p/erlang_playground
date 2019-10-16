-module(tree).

-export([empty/0, insert/3, lookup/2]).

empty() -> {node, nil}.

%% Inserting into the tree.
insert(Key, Value, {node, nil}) ->
    {node, {Key, Value, {node, nil}, {node, nil}}};
insert(NewKey, NewValue,
       {node, {Key, Value, Greater, Smaller}})
    when NewKey > Key ->
    {node,
     {Key, Value, insert(NewKey, NewValue, Greater),
      Smaller}};
insert(NewKey, NewValue,
       {node, {Key, Value, Greater, Smaller}})
    when NewKey < Key ->
    {node,
     {Key, Value, Greater,
      insert(NewKey, NewValue, Smaller)}};
insert(Key, Value,
       {node, {Key, _, Greater, Smaller}}) ->
    {node, {Key, Value, Greater, Smaller}}.

%% Looking up a key.
lookup(_, {node, nil}) -> key_doesnt_exist;
lookup(Key, {node, {Key, Value, _, _}}) -> Value;
lookup(Key, {node, {OtherKey, _, _, Smaller}})
    when OtherKey > Key ->
    lookup(Key, Smaller);
lookup(Key, {node, {OtherKey, _, Greater, _}})
    when OtherKey < Key ->
    lookup(Key, Greater).
