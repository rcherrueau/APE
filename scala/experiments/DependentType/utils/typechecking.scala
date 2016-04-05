/*
 * Copyright (c) 2013 Miles Sabin
 * Copyright (c) 2014 Ronan-Alexandre Cherrueau
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package utils

import scala.language.experimental.macros

import scala.reflect.macros.{ whitebox, TypecheckException }

/**
  * A utility which ensures that a code fragment does not typecheck.
  *
  * Credit: Stefan Zeiger (@StefanZeiger)
  */
object illTyped {
  def apply(code: String): Unit = macro applyImplNoExp
  def apply(code: String, expected: String): Unit = macro applyImpl

  def applyImplNoExp(c: whitebox.Context)(code: c.Expr[String]) = applyImpl(c)(code, null)

  def applyImpl(c: whitebox.Context)(code: c.Expr[String], expected: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._

    val Expr(Literal(Constant(codeStr: String))) = code
    val (exp, expMsg) = expected match {
      case null => (null, "Expected some error.")
      case Expr(Literal(Constant(s: String))) =>
        (s, "Expected error matching: "+s)
    }

    try {
      c.typecheck(c.parse("{ "+codeStr+" }"))
      c.abort(c.enclosingPosition, "Type-checking succeeded unexpectedly.\n"+expMsg)
    } catch {
      case e: TypecheckException =>
        val msg = e.getMessage
        if((expected ne null) && !(exp.diff(msg).isEmpty)) {
          val Literal(Constant(s: String)) = expected.tree
          c.abort(c.enclosingPosition,
            "Type-checking failed in an unexpected way.\n" +
              expMsg + "\nActual error: " + msg)
        }
    }

    reify(())
  }
}


object typeMismatch {
  def apply(found: String, required: String): String = macro impl

  def impl(c: whitebox.Context)(found: c.Expr[String], required: c.Expr[String]) = {
    import c.universe._

    q"""
    "type mismatch;\n"+
    " found   : " + $found + "\n" +
    " required: " + $required
    """
  }
}

object badInference {
  def apply(found: String, method: String, required: String): String =
    macro impl

  def impl(c: whitebox.Context)
      (found: c.Expr[String], method: c.Expr[String], required: c.Expr[String]) = {
    import c.universe._

    q"""
    "inferred type arguments [" + $found + "] " +
    "do not conform to method " + $method +"'s type parameter " +
    "bounds [" + $required + "]"
    """
  }
}
