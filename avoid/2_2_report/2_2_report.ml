let rec fib (r) =
        if r=0 then
                0
        else
                if r=1 then
                        1
        else
                fib (r-1) + fib (r-2)
;;

let stairs1 (n) =
        if n<0 then
                failwith "Error"
        else
                fib (n+1)
;;

let rec stairs2 (r) =
        if r<0 then
                failwith "Error"
        else
                match r with
                | 1 -> 1
                | 2 -> 2
                | _ -> stairs2 (r-2) + stairs2 (r-1)
;;

let trpez (a, b, h) =
        (a+.b) *. h /. 2.0
;;

let h = 0.001
;;

let diff (f, i)=
        (f(i+.h) -. f(i)) /. h
;;

let rec intel (f, a, b) =
        if a+.h <= b then
                trpez (h, h, f(a+.h)) +. intel (f, a+.h, b)
        else
                0.0
;;

let rec ext (f, a, b) =
        if a<=b then
                if -0.01< diff(f, a) && diff(f, a) <0.01 then
                        f (a)
                else
                        ext (f, a+.h, b)
        else
                failwith "Not Found"
;;

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
