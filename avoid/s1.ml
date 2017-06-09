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
