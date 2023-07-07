import scala.math.*
import scala.util.Random
import org.apache.commons.math3.special.Beta
val r = Random()
object mathengine:
  def norm(mean:Double,std:Double,x:Double):Double = pow(E,-0.5*pow((x-mean)/std,2))
  def sscore(x:Double):Double = Beta.regularizedBeta(x,0.5,1.5)
  def rscore(x:Double):Double = Beta.regularizedBeta(x,7.5,2)
  def randIndex(x:Int):Int = r.nextInt(x)
  def randDouble():Double = r.nextDouble()