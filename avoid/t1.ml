let h = 0.01
;;

let diff (f, i)=
        (f(i+.h) -. f(i)) /. h
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
