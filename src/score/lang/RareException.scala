package score.lang
  
object RareException {

  def apply() =
    new RareException()    
  def apply(message: String) =
    new RareException(message)
    
}

final class RareException protected (obj: Any) extends scala.RuntimeException {
  def this(message: String) = this(message.asInstanceOf[Any])
  def this() = this("Rare exception")
}   
