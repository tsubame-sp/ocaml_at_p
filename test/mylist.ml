module M = struct
    type 'a mylist = MNil | MCons of ('a * 'a mylist)

    let rec length = function
        | MNil -> 0 [@ps "MNil -> "][@p]
        | MCons (_,xs) -> 1 + length xs

    type record = {a:int;b:float}
end
