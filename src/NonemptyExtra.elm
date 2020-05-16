module NonemptyExtra exposing (appendNonEmpty)

import List.Nonempty as Nonempty exposing (Nonempty)


appendNonEmpty : a -> List a -> Nonempty a
appendNonEmpty start =
    List.foldl
        (\i acc -> Nonempty.append acc <| Nonempty.fromElement i)
        (Nonempty.fromElement start)
