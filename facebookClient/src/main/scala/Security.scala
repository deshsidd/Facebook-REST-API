////CONTAINS MISC SECURITY FUNCTIONS
package com.anupam.siddhant.facebook

import java.security._
import scala.util.Random
import java.security.AlgorithmParameters
import java.security.SecureRandom
import javax.crypto.BadPaddingException
import javax.crypto.Cipher
import javax.crypto.IllegalBlockSizeException
import javax.crypto.SecretKey
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.IvParameterSpec
import javax.crypto.spec.PBEKeySpec
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Base64
import AES256._
import scala.collection.JavaConversions._

object Security
{
	def SRSG():String = //Secure Random Alphanumeric String Generator
	{
		  val rand = new Random(new SecureRandom())

		  def randomString(alphabet: String)(n: Int): String = 
		  Stream.continually(rand.nextInt(alphabet.size)).map(alphabet).take(n).mkString

		  def randomAlphanumericString(n: Int) = 
		  randomString("abcdefghijklmnopqrstuvwxyz0123456789")(n)

		  val num= Random.nextInt(90)+10

		  var s=randomAlphanumericString(num)
		  s
	}

	def SRAG():Array[Byte] = //Secure Random Array of Bytes Generator
	{
		val rand = new SecureRandom()
		val num= Random.nextInt(90)+10
		val arr = new Array[Byte](num)
		rand.nextBytes(arr)
		
		arr
		
	}

	def SRNG():Long = //Secure Random Number Generator returns a Long number
	{
		//var rand = new SecureRandom()
		//var r = rand.getInstance("SHA1PRNG")
		var rand = SecureRandom.getInstance("SHA1PRNG")
		//rand.setSeed("abcdefghijklmnop".getBytes("us-ascii"))
		//val num= Random.nextInt(1000000000)+10000000
		var randomnumber=rand.nextLong()
		randomnumber

	}



 
  def sha256(s: String): String = { // returns the Sha256 of given string

    val m = java.security.MessageDigest.getInstance("SHA-256").digest(s.getBytes("UTF-8"))
    m.map("%02x".format(_)).mkString
  }

}







