let search (list1, list2) =
        let rec internal (a, b, c, d) =
                match (c, d) with
                | ([], z::ex) -> -1
                | (_, []) -> a
                | (x1::y1, x2::y2) -> if x1=x2 then
                                              internal (a, b, y1, y2)
                                      else
                                              let rec rest (n, m) =
                                                      match (n, m) with
                                                      | (0, l) -> l
                                                      | (_, []) -> []
                                                      | (n, o::p) -> rest (n-1, p)

in internal (a+1, b+1, rest (b, list1), list2)
in internal (-1, 0, list1, list2)
;;
