open Base

module List = struct
  include List

  (* Split a list into groups, splitting whenever the predicate f
     returns true for an element of the list. Elements that satisfy
     the predicate are not included in the result. *)
  let group f lst =
    if length lst = 0 then []
    else
      fold_left (List.rev lst) ~init:[ [] ] ~f:(fun acc e ->
          if f e then [] :: acc
          else
            match acc with
            | [] -> [ [ e ] ]
            | g :: gs -> (e :: g) :: gs)

  let sum = reduce_exn ~f:( + )
  let max = reduce_exn ~f:Int.max
end
