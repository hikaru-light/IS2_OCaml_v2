let rec search (list1, list2) =
        match (list1, list2) with
        | ([], list2) -> -1
        | (n1::rest1, n2::rest2) -> if n1 = n2 then
                                            let rec internal (rest1, rest2) =
                                               match (rest1, rest2) with
                                               | (z1::ex, z2::[]) -> if z1 = z2 then 1
                                                                     else -1
                                               | (x1::y1, x2::y2) -> if x1 = x2 then internal (y1, y2)
                                                                      else -1
                                    else
                                            1 + search (rest1, list2)
;;
