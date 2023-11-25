module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module ListM (M : Monad) : sig
  val fold_left : ('state -> 'b -> 'state M.t) -> 'state -> 'b list -> 'state M.t
  val fold_right : ('b -> 'state -> 'state M.t) -> 'b list -> 'state -> 'state M.t
end = struct
  let rec fold_left f state = function
    | [] -> M.return state
    | x :: xs -> M.( >>= ) (f state x) (fun state -> fold_left f state xs)
  ;;

  let rec fold_right f list state =
    match list with
    | [] -> M.return state
    | x :: xs -> M.( >>= ) (fold_right f xs state) (fun state -> f x state)
  ;;
end
