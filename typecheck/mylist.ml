exception CarOfEmptyList
let car = function h::t -> h | _ -> raise CarOfEmptyList
let cdr = function h::t -> t | _ -> []
let cons h t = h::t
let cadr l = car (cdr l)

exception FoldLError
let foldl f lst =
  let rec foldl_iter r l =
    if l == [] then r
    else foldl_iter (f r (car l)) (cdr l)
  in if lst == [] then raise FoldLError
    else if (cdr lst) == [] then raise FoldLError
    else foldl_iter (f (car lst) (car (cdr lst))) (cdr (cdr lst))
