package com.anupam.siddhant.facebook
import java.security._

object PublicKeyStore {
  val puKeyMap = scala.collection.mutable.Map[String, PublicKey]()

  def setMap(str : String, puk : PublicKey){
    puKeyMap += (str -> puk)
  }

  def getMap(str : String) : PublicKey = {
    puKeyMap(str)
  }
}
