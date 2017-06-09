let rec stairs2 (r) =
        if r<0 then
                failwith "Error"
        else
                match r with
                | 1 -> 1
                | 2 -> 2
                | _ -> stairs2 (r-2) + stairs2 (r-1)
;;
