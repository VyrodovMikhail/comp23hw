module type Monad =
sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module ListM (M : Monad) : sig
  val fold_left : ('state -> 'b -> 'state M.t) -> 'state -> 'b list -> 'state M.t
  val fold_right : ('b -> 'state -> 'state M.t) -> 'b list -> 'state -> 'state M.t
end