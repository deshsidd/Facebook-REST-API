package com.anupam.siddhant.facebook

import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.io.IOException
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.security.KeyPair
import java.security.KeyPairGenerator
import java.security.NoSuchAlgorithmException
import java.security.PrivateKey
import java.security.PublicKey
import javax.crypto.Cipher
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
import org.apache.commons.codec.binary.Base64
import java.security.spec.X509EncodedKeySpec
import java.security.spec.PKCS8EncodedKeySpec

object RSA
{
	val ALGORITHM = "RSA"

	def getRSAKeys() : (PublicKey, PrivateKey) =
	{
		val kpg = KeyPairGenerator.getInstance("RSA") //we create an instance of KeyPairGenerator suitable for generating RSA keys
	    kpg.initialize(2048) //we initialise the generator, telling it the bit length of the modulus that we require
	    val kp = kpg.genKeyPair() //we call genKeyPair(), which eventually returns a KeyPair object
	    val publicKey = kp.getPublic //we call getPublic() and getPrivate() on the latter to pull out the public and private keys
	    val privateKey = kp.getPrivate
	    (publicKey,privateKey)
	}
	def encrypt(text: String, key: PublicKey): Array[Byte] = {
    var cipherText: Array[Byte] = null
    try {
      val cipher = Cipher.getInstance(ALGORITHM)
      cipher.init(Cipher.ENCRYPT_MODE, key)
      cipherText = cipher.doFinal(text.getBytes)
    } catch {
      case e: Exception => e.printStackTrace()
    }
    cipherText
  }

  def decrypt(text: Array[Byte], key: PrivateKey): String = {
    var dectyptedText: Array[Byte] = null
    try {
      val cipher = Cipher.getInstance(ALGORITHM)
      cipher.init(Cipher.DECRYPT_MODE, key)
      dectyptedText = cipher.doFinal(text)
    } catch {
      case ex: Exception => ex.printStackTrace()
    }
    new String(dectyptedText)
  }

  def encrypt(text: String, key: PrivateKey): Array[Byte] = {
    var cipherText: Array[Byte] = null
    try {
      val cipher = Cipher.getInstance(ALGORITHM)
      cipher.init(Cipher.ENCRYPT_MODE, key)
      cipherText = cipher.doFinal(text.getBytes)
    } catch {
      case e: Exception => e.printStackTrace()
    }
    cipherText
  }
  def decrypt(text: Array[Byte], key: PublicKey): String = {
    var dectyptedText: Array[Byte] = null
    try {
      val cipher = Cipher.getInstance(ALGORITHM)
      cipher.init(Cipher.DECRYPT_MODE, key)
      dectyptedText = cipher.doFinal(text)
    } catch {
      case ex: Exception => ex.printStackTrace()
    }
    new String(dectyptedText)
  }

	def PublicToString(publicKey:PublicKey):String =
	{
			val publicK = Base64.encodeBase64String(publicKey.getEncoded)
				publicK
	}

	def PrivateToString(privateKey:PrivateKey):String =
	{

				val privateK = Base64.encodeBase64String(privateKey.getEncoded)
				privateK
	}

	def StringToPublic(str:String):PublicKey=
	{
			val publicBytes = Base64.decodeBase64(str)
			val keySpec = new X509EncodedKeySpec(publicBytes)
			val keyFactory = KeyFactory.getInstance("RSA")
			val publicKey = keyFactory.generatePublic(keySpec)

			publicKey
	}

	def StringToPrivate(str:String):PrivateKey=
	{
			val privateBytes = Base64.decodeBase64(str)
			val keySpec = new PKCS8EncodedKeySpec(privateBytes)
			val keyFactory = KeyFactory.getInstance("RSA")
			val privateKey = keyFactory.generatePrivate(keySpec)

			privateKey

	}
	//this is how to call getRSAKeys()
	// val (publicKey,privateKey) = getRSAKeys()
/*
	def saveKeysToFile()
	{
		val fact = KeyFactory.getInstance("RSA")
	    val pub = fact.getKeySpec(kp.getPublic, classOf[RSAPublicKeySpec])
	    val priv = fact.getKeySpec(kp.getPrivate, classOf[RSAPrivateKeySpec])
	    saveToFile("public.key", pub.getModulus, pub.getPublicExponent)
	    saveToFile("private.key", priv.getModulus, priv.getPrivateExponent)

	}



    def saveToFile(fileName: String, mod: BigInt, exp: BigInt)
    {
	    	val oout = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(fileName)))
	    try
	    {
		      oout.writeObject(mod)
		      oout.writeObject(exp)
	    } catch
	    {
	      	  case e: Exception => throw new IOException("Unexpected error", e)
	    } finally
	    {
	      	  oout.close()
	    }
   }
*/
}
