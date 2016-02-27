/**
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * Changes in Master for rebase
 * @author Patrick Nicolas
 * @date September 23, 2013
 */
package algorithms


class FactoryManifest[T](implicit m: Manifest[T]) {
   def create(len: Int): Array[T] = m.erasure.newInstance.asInstanceOf[Array[T]]
}

import scala.reflect.ClassTag
class FactoryClassTag[T: ClassTag] {
    def create(len: Int): Array[T] = new Array[T](len)
}


object TestFactory extends App {
    val factoryDouble = new FactoryManifest[Double]
    factoryDouble.create(2)
}

// -----------------------------  EOF ---------------------------