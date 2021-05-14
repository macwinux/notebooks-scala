// Databricks notebook source
// MAGIC %md
// MAGIC # Functor

// COMMAND ----------

// MAGIC %md
// MAGIC A Functor is a type class and it is any data type that defines how map applies to it.

// COMMAND ----------

// MAGIC %md
// MAGIC We have a Constructor C[_] and two types A and B, we want to apply functions of type C[A] => C[B], so we need adequate tranformations.

// COMMAND ----------

trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
}

// COMMAND ----------

// MAGIC %md
// MAGIC We can test it with a class like Option:

// COMMAND ----------

sealed abstract class OptionFun[+A] {
    def getValue: A = this match {
        case SomeStuff(x) => x
        case NoneStuff => throw new Exception
    }
    
    def isEmpty: Boolean = this eq NoneStuff
    def map[B](f:A => B): OptionFun[B] = if(isEmpty) NoneStuff else SomeStuff(f(this.getValue))
}

final case class SomeStuff[+A](value:A) extends OptionFun[A]{
    def get: A = value
}

case object NoneStuff extends OptionFun[Nothing]

// COMMAND ----------

val opt = SomeStuff(15)
println("new value " + opt.map(_.toString).getValue)

// COMMAND ----------

// MAGIC %md
// MAGIC # Applicative

// COMMAND ----------

// MAGIC %md
// MAGIC Applicative is a type class and it is any data type that defines how apply applies to it.

// COMMAND ----------

// MAGIC %md
// MAGIC Apply takes a functor that has a function in it and another functor and extracts that function from the first functor and then maps it over the second one.

// COMMAND ----------

// MAGIC %md
// MAGIC We have a Constructor C[_] and two types A and B, we want to apply functions of type C[A] => C[B], so we need adequate tranformations.

// COMMAND ----------

trait Applicative[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
    def apply[A,B](ff: F[A => B])(fa: F[A]): F[B]
    def map[A, B](fa: F[A])(f: A => B): F[B] = apply(pure(f))(fa)
}

// COMMAND ----------

// MAGIC %md
// MAGIC ![](images/applicative.png)

// COMMAND ----------

// MAGIC %md
// MAGIC Using our class OptionFun:

// COMMAND ----------

object OptionFun {
    def pure[A](x: A): OptionFun[A] = if(x == null) NoneStuff else SomeStuff(x)
}
sealed abstract class OptionFun[+A] {
    def getValue: A = this match {
        case SomeStuff(x) => x
        case NoneStuff => throw new Exception
    }
    
    def isEmpty: Boolean = this eq NoneStuff
    def map[B](f:A => B): OptionFun[B] = if(isEmpty) NoneStuff else SomeStuff(f(this.getValue))
    def apply[B](f: OptionFun[A => B]): OptionFun[B] = f.map(f => f(this.getValue))
}

final case class SomeStuff[+A](value:A) extends OptionFun[A]{
    def get: A = value
}

case object NoneStuff extends OptionFun[Nothing]

// COMMAND ----------

val a: Int => String = _.toString

// COMMAND ----------

val functionOptionFun = OptionFun.pure(a)

// COMMAND ----------

val opt = SomeStuff(15)
println("new value " + opt.apply(functionOptionFun).getValue)

// COMMAND ----------

// MAGIC %md
// MAGIC Maybe seems like this is not usefull but it is

// COMMAND ----------

// MAGIC %md
// MAGIC # Monads

// COMMAND ----------

// MAGIC %md
// MAGIC Monads apply a function that returns a wrapped value to a wrapped value

// COMMAND ----------

// MAGIC %md
// MAGIC ![](images/monad.png)

// COMMAND ----------

// MAGIC %md
// MAGIC Monad is a type class and it's a data type that implements the flatMap.

// COMMAND ----------

// MAGIC %md
// MAGIC We have a Constructor C[_] and two types A and B, we want to apply functions of type C[A] => C[B], so we need adequate tranformations.

// COMMAND ----------

trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A,B](ff: A => F[B])(fa: F[A]): F[B]
}

// COMMAND ----------

// MAGIC %md
// MAGIC Using our class OptionFun:

// COMMAND ----------

object OptionFun {
    def pure[A](x: A): OptionFun[A] = if(x == null) NoneStuff else SomeStuff(x)
}
sealed abstract class OptionFun[+A] {
    def getValue: A = this match {
        case SomeStuff(x) => x
        case NoneStuff => throw new Exception
    }
    
    def isEmpty: Boolean = this eq NoneStuff
    def map[B](f:A => B): OptionFun[B] = if(isEmpty) NoneStuff else SomeStuff(f(this.getValue))
    def apply[B](f: OptionFun[A => B]): OptionFun[B] = f.map(f => f(this.getValue))
    def flatMap[B](f: A => OptionFun[B]): OptionFun[B] = if(isEmpty) NoneStuff else f(this.getValue)
}

final case class SomeStuff[+A](value:A) extends OptionFun[A]{
    def get: A = value
}

case object NoneStuff extends OptionFun[Nothing]

// COMMAND ----------

// MAGIC %md
// MAGIC Why is so important the flatMap method? With it you can compose functions with wrapped values. Let see and example

// COMMAND ----------

// MAGIC %python
// MAGIC val a = SomeStuff(5)
// MAGIC val b = SomeStuff(6)
// MAGIC val c = SomeStuff(7)

// COMMAND ----------

// MAGIC %md
// MAGIC if we want to summ all the values, with flatMap and map we can:

// COMMAND ----------

a.flatMap(value => b.flatMap(value2 => c.map(value3 => value + value2 + value3)))

// COMMAND ----------

// MAGIC %md
// MAGIC But this is not pretty. We can do it in a readable form using for comprehension:

// COMMAND ----------

for {
    value1 <- a
    value2 <- b
    value3 <- c
} yield (value1 + value2 + value3)
