package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

// Add your definitions here

trait RegularLanguage

//case objects
case object Empty extends RegularLanguage
case object Epsilon extends RegularLanguage

//case classes
//Character, Union, Concat, and Star

case class Character(val c1: Char) extends RegularLanguage
case class Union(val l1: RegularLanguage, val l2: RegularLanguage) extends RegularLanguage
case class Concat(val l1: RegularLanguage, val l2: RegularLanguage) extends RegularLanguage
case class Star(val s1: RegularLanguage) extends RegularLanguage

/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage = lang match
  case (Star(Empty)) => Empty
  case (Star(Epsilon)) => Epsilon
  case Union(lang, Empty) => simplify(lang)
  case Union(Empty, lang) => simplify(lang)
  case Union(l1, l2) => Union(simplify(l1), simplify(l2))
  case Concat(Epsilon, lang) => simplify(lang)
  case Concat(lang, Epsilon) => simplify(lang)
  case Concat(Empty, lang) => simplify(Empty)
  case Concat(lang, Empty) => simplify(Empty)
  case Concat(l1, l2) => Concat(simplify(l1), simplify(l2))
  case Star(lang) => Star(simplify(lang))

  case _ => lang

/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = lang match
  case Empty | Character(_) => false
  case Epsilon => true
  case Star(_) => true
  case Concat(l1, l2) => nullable(l1) & nullable(l2)
  case Union(l1, l2) => nullable(l1) | nullable(l2)

  case _ => nullable(simplify(lang))



/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = ???


/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
