package com.rockthejvm.recap

/**
  * This simple application serves as a playground for the Scala features we are going to use in this course.
  * I wrote it already so you don't waste your time typing what you already know.
  * For any concept that we use in this course which you're rusty/uncomfortable with, feel free to refer back to this app.
  *
  * For more in-depth exploration and exercises on these topics, check out the beginners course (Scala essentials).
  *
  * Daniel @ Rock the JVM
  */
object ScalaRecap extends App {

  /**
    * Basics
    */

  // declaring a value
  val purposeOfLife: Int = 42
  //               ^^^^^ the type here is optional as the compiler can infer it from the right hand side

  // can't change a value
  // purposeOfLife = 43 // nope!

  // variables can be changed
  var mood = "sad"
  mood = "happy"

  // everything in Scala is an expression, so it's evaluated (reduced) to a value
  val aCondition = if (purposeOfLife > 0) "happy" else "sad"

  // code blocks are expressions
  val aCodeBlock = {
    val x = 2 // can define values inside
    x + 99 // the value of the block is the value of its last expression
  }

  // defining a function
  def aFunction(param: Int) = param + 1

  // functions can be recursive
  def factorial(n: Int): Int =
    if (n <= 1) 1
    else n * factorial(n - 1)

  // tail-recursive functions: VERY IMPORTANT for this course
  def factorialTailrec(n: Int, accumulator: Int): Int =
    if (n <= 1) accumulator
    else factorialTailrec(n - 1, n * accumulator)

  /**
    * Object-oriented programming
    */
  class Car
  class Supercar extends Car

  // subtype polymorphism
  val c: Car = new Supercar

  // traits are types with abstract (not implemented) methods
  trait SelfDriving {
    def drive: Unit
  }

  // a class can extend one class and mix-in as many traits as you like
  class CarOfTheFuture extends Car with SelfDriving {
    override def drive: Unit = println("I'm driving by myself! Wooohooo! I hope I don't hit any pedestrians... Who's Asimov?!")
  }

  // case classes are lightweight data structures with boilerplate e.g. equals/hashCode already implemented
  case class Person(name: String, age: Int) {
    def drives(car: Car) = println(s"$name is driving $car") // <-- s-interpolated string
  }

  // method notation
  val alice = Person("Alice", 23)
  val lamborghini = new Supercar
  alice drives lamborghini // <-- infix notation

  // generics
  class MySet[+T]

  // singletons & companions
  object MySet

  /**
    * Functional programming
    */

  // functions are instances of the FunctionX trait
  val incrementer = new Function1[Int, Int] {
    override def apply(x: Int): Int = x + 1
  }
  val two = incrementer(1) // same as incrementer.apply(1)

  // syntax sugars for functions
  val anotherIncrementer = (x: Int) => x + 1 // same as new Function1[Int, Int] { ... }

  // higher-order functions
  val incrementedList = List(1,2,3).map(incrementer) // function is passed as argument to the map method
  //                                ^^^ this returns a NEW list

  /**
    * Collections
    */

  // lists
  val aList = List(1,2,3,4)

  // sequences = abstract representations of elements in a given order
  val aSeq = Seq(1,2,3,4) // Seq is a trait, so the Seq companion's apply() actually builds a list

  // arrays
  val anArray = Array.ofDim[Int](2, 3) // <- bi-dimensional in this call

  // sets: every element appears once
  val aSet = Set(1,1,2,3)

  // vectors: efficient Seq implementation
  val aVector = Vector(1,2,3,4)

  // tuples
  val aTuple: (Int, String) = (1, "I love tuples! Yay. Please bring more tuples.")

  // maps
  val aMap = Map(
    "Daniel" -> 789,
    "Jess" -> 555
  )

  /**
    * Pattern matching
    */

  // pattern matching
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"
  }

  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"Hi, my name is $n"
  }

  // all the patterns

  /**
    * For the hardcore peeps: implicits.
    * Implicits are a beast. We are only going to use implicits for the below use cases.
    * Implicits are hard and OPTIONAL in this course, just for the sake of Scala's expressiveness.
    *
    * The advanced Scala course explains everything about implicits.
    */

  // use case 1: implicit arguments
  implicit val descendingIntOrdering: Ordering[Int] = Ordering.fromLessThan((a, b) => b > a) // comparison object
  val theObviousTruth = descendingIntOrdering.lteq(99, 100)
  //                                          ^^^^ other methods: lt, gt, gteq, etc.

  // implicit orderings are used for sorting methods
  val descendingList = List(1,2,3,4).sorted
  //                                       ^ here the compiler injects the implicit ordering I defined above, as argument

  // use case 2: implicit conversions
  implicit class MyRichInt(number: Int) {
    def isPositive = number > 0
  }

  val oneSign = 1.isPositive
  // equivalent with new MyRichInt(1).isPositive
  // the auto-conversion is done at compile time



}
