(** {1} Notes on Chapter 1 *)

(** {2} 1.4 Challenges *)

(** Exercise 1
    Implement the identify function. *)

let id : 'a. 'a -> 'a  =
  fun x -> x

(** Exercise 2
    Implement the composition function. *)

let (%) : 'a 'b 'c. ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c =
  fun g f x -> g (f x)

(** Exercise 3
    >  Write a program that tries to test that your composition function respects identity *)

let%test_module "composition" = (module struct
  open QCheck

  let%test _ =
    Aux.run_qcheck
      [
        Test.make ~name:"composition respects left identity"
          (pair (fun1 Observable.string int) string)
          (fun (f, x) -> Fn.apply f x = ((id : int -> int) % Fn.apply f) x)
      ; Test.make ~name:"composition respects right identity"
          (pair (fun1 Observable.string int) string)
          (fun (f, x) -> Fn.apply f x = (Fn.apply f % (id : string -> string)) x)
      ]
end)

(** Exercise 4

    > Is the worl-wide web a category in any sense? Are links morphisms?
*)



(** Exercise 5
    > Is Facebook a category, with people as objects and friendships as morphsisms? *)
