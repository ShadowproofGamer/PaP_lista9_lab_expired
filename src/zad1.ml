type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec lrepeatExponential (n, llx)=
    let rec lrepeatExponential_rec(n, x, lazyRest) =
        if n > 1 then LCons(x, function () -> (lrepeatExponential_rec ((n-1), x, lazyRest)))
        else LCons(x, lazyRest) in

    match llx with
    | LNil -> LNil
    | LCons(x, xf) -> lrepeatExponential_rec(n, x, function () -> (lrepeatExponential((n+1), xf())));;

let rec lfrom k = LCons (k, function () -> lfrom (k+1));;
let rec ltake = function
| (0, _) -> []
| (_, LNil) -> []
| (n, LCons(x,xf)) -> x::ltake(n-1, xf());;

ltake(15,(lrepeatExponential(1,(lfrom 3))));;

