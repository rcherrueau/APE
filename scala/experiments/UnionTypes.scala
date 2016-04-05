// def size(x: Int v String) = x match {
//   case i: Int => i
//   case s: String => s.length
// }

// size(23) == 23
// size("foo") == 3
// size(1.0) // Doesn't compile

// // Scale offer Either. Either is a sum type, the analog of union types
// // in type theory. And the main disavantage is Either don't support
// // subtyping.
// def size(x: Either[Int, String]) = x match {
//   case Left(i) => i
//   case Right(s) => s.length
// }

// size(Left(23))
// size(Right("foo"))

// // There is an isomorphism between Either[Int, String] and (Int v
// // String). But the Either type manages this by way of a layer of
// // boxed representation, rather then by being an unboxed primitive
// // feature of the type system.

// // Can we do better than Either? Can we find a way of representing
// // union types in Scala which doesn't require boxing, and which
// // provides all of the static guarantees we would expect?

// // It turns out that we can, but to get there we have take a detour
// // through first-order logic via the Curry-Howard isomorphism.
// // Curry-Howard tells us that the relationships between types in a
// // type system can be viewed as an image of the relationships between
// // propositions in a logical system (and vice versa).

// // There are various ways that we can fill that claim out, depending
// // on the type system, we're talking about and the logical system
// // we're working with, but for the purposes of this discussion I'm
// // going to ignore most of the details and focus on simple examples.

// // To illustrate Curry-Howard (in the context of a type system with
// // subtyping like Scala's), we can see that there is a correspondence
// // between intersection types (A with B in scala) and logical
// // conjunction (A ∧ B); between the hypothetical union types (A ∨ B)
// // and logical disjunction (also A ∨ B); and between subtyping (A <:
// // B) and logical implication (A ⇒ B). On the left hand side of each
// // row in the table below we have a subtype relationship which is
// // valid in Scala

// //  (A with B) <: A   (A ∧ B) ⇒ A
// //  (A with B) <: B   (A ∧ B) ⇒ B
// //  A <: (A ∨ B)      A ⇒ (A ∨ B)
// //  B <: (A ∨ B)      B ⇒ (A ∨ B

// // The essence of Curry-Howard is that this mechanical rewriting
// // process (whichever direction you go in) will always preserve
// // velidity -- valid type formulae will always rewrite to valid
// // logical formulae, and vice versa. This isn't only true for
// // conjunction, disjunction and implication. We can also generalize
// // the correspondence to logical formulae wich include negation of a
// // type A (I'll write it as ¬[A]) to have as it's values everything
// // which isn't an instance of A. This is also something which can't be
// // directly expressed in Scala, but suppose it was?

// // If it was, then we would be able to crank on the Curry-Howard
// // isomorphism and De Morgan's laws to give us a definition of union
// // types in terms of interserction types (A with B) and type negation.
// // Here's how that might go ...

// // First recall the De Morgan equivalence,
// //  (A ∨ B) \Leftrightarrow ¬(¬A ∧ ¬B)

// // Now apply Curry-Howard (using Scala's =:= type equality operator).
// (A ∨ B) =:= ¬[¬[A] with ¬[B]]

// // If we could work out a way of expressing this in Scala, we'd be
// // home and dry and have our union types. So can we express type
// // negation?

// // Unfortunately we can't. But what we can do is transform all of our
// // types in a way which allows negation to be expressed in the
// // transformed context. We'll then need to work out how make that work
// // for us back in the original untransformed context.

// // Some readers might have been a little suprised earlier when I
// // illustrated Curry-Howard using interserction types as the correlate
// // of conjuction, union types as the correlate of disjunction and the
// // subtypes relation as the correlate of implication. That's not how
// // it's noramlly done -- usually product types (ie. (A, B)) model
// // conjunction, sum types (ie. Either[A,B]) model disjunction and
// // function types model implication. If we recast our earlier table in
// // terms of products, sums and functions we end up with this,

// //  (A, B) => A         (A ∧ B) ⇒ A
// //  (A, B) => B         (A ∧ B) ⇒ B
// //  A => Either[A, B]   A ⇒ (A ∨ B)
// //  B => Either[A, B]   B ⇒ (A ∨ B)

// // On the left hand side we're no longer looking for validity with
// // respect to the subtype relation, instead we're looking for
// // *evidence of the principle of parametricity*, which allow us to
// // determine if a function type is implementable just by reading it's
// // signature. It's clear that all the function signatures on the left
// // in the table above can be implemented -- for the first two we have
// // an (A, B) pair as our function argument, se we can easily evaluate
// // to either A or B, using _1 or _2,

// val conj1: ((A,B)) => A = p => p._1
// val conj2: ((A,B)) => B = p => p._2

// // And for the last two we have either an A or a B as our function
// // argument, so we can evaluate to Either[A,B] (as Left[A] or Right[B]
// // respectively).

// val disj1: A => Either[A,B] = a => Left(a)
// val disj2: B => Either[A,B] = b => Right(b)



// // This is the form in which the Curry-Howard isomorphism is typically
// // expressed for languages without subtyping. Because this mapping
// // doesn't reflect subtype relations it isn't going to be much direct
// // use to us for expressing union types which, like intersection
// // types, are inherently characterized in terms of subtyping. But it
// // can help us out with negation, which is the missing piece that we
// // need.

// // Either with or without subtyping, the bottom type (Scala Nothing
// // type) maps to logical falsehood, so for example, the following
// // equivalences all hold,

// //  A => Either[A, Nothing]   A ⇒ (A ∨ false)
// //  B => Either[Nothing, B]   B ⇒ (false ∨ B)

// // because the function signatures on the left are once again all
// // implementable, and the logical formulae on the right are again all
// // valid. Now we need to think about what a function signature like,

// A => Nothing

// // corresponds to. On the logical side of Curry-Howard this maps to A
// // ⇒ false, which is equivalent to ¬A. This seems fairly intuitively
// // reasonable -- *there are no values of type Nothing, so the
// // signature A => Nothing can't be implemented* (other than by
// // throwing an exception, which isn't allowed).

// // Let's see what happens if we take this as our representation of the
// // negation of a type,

// type ¬[A] = A => Nothing

// // The only way to implement this is using an exception which isn't
// // allowed:
// val a: ¬[Any] = any => throw new Exception
// a(10)

// // and apply it back in the subtyping context that we started with to see


// type \[A,B] = A with ¬[B]
// // function type constructor
// type f[A] = A => Any

// // et je dois tester
// f[A] <:< f[A \ B]

// implicitly[f[A] <:< f[A \ B]] // Ok
// implicitly[f[B] <:< f[A \ B]] // Nok

// type |\|[A,B] = { type λ[X] = f[X] <:< f[A \ B] }

// def encode[T <: Data : (Data |\| Pull)#λ](t: T) = ???
