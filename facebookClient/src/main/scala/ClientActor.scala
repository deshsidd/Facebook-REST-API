import akka.actor._
import akka.actor.ActorSystem
import spray.httpx.SprayJsonSupport._
import spray.json.DefaultJsonProtocol
import spray.http._
import spray.client.pipelining._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

import java.awt.image.BufferedImage
import java.io._
import javax.imageio.ImageIO
import javax.crypto.SecretKey
//import java.util._
import java.nio.charset.StandardCharsets
import org.apache.commons.codec.binary.Base64

import FacebookProtocol.User
import FacebookProtocol.LoginData
import FacebookProtocol.Image
import FacebookProtocol.AESKeyMap
import FacebookProtocol.KeyAndData

import com.anupam.siddhant.facebook._
import com.anupam.siddhant.facebook.Images._
import com.anupam.siddhant.facebook.Security._
import java.security._
import akka.actor.{ ActorRef, ActorSystem }
import akka.serialization._
import com.typesafe.config.ConfigFactory

class ClientActor(id : Int, system:ActorSystem, pk : PrivateKey, puk : PublicKey) extends Actor{
  val selfId = id
  var selfName = "new"
  val serverLocation = "http://localhost:8080"
  val timeout = 5.seconds
  var serverPuk = puk
  var token = Array[Byte]()
  var keyMap = scala.collection.mutable.Map[String, Array[Byte]]()
  var idCounter = ""

  var failedAttempts = 0

  def receive = {
    case InitializeUser(name : String, userName : String) => {
        val newUser = User(selfId, name, userName, RSA.PublicToString(puk))
        selfName = name
        var reply = createUser(newUser)
        serverPuk = RSA.StringToPublic(reply)
        token = loginRequest("LoginRequest")
        if(selfId == 4)
        {
          PublicKeyStore.setMap("user"+selfId.toString(), RSA.StringToPublic(RSA.PublicToString(puk)))
          setMap()
        }
        //sender ! UserCreated
    }

    case AddFriend(friendId : Int) => {
      println( stringCases("addfriend",selfId,friendId) )
      //stringCases("addfriend",selfId,friendId)
      //sender ! FriendsAdded
    }

    case AcceptRequest(friendId : Int) => {
      println( stringCases("acceptrequest", selfId, friendId) )
    }

    // Needs server changes
    case GetFriendsList => {
      println( getFriendsList("friendlist") )
    }

    case GetAnotherUserProfile(userId : Int) => {
      val response = getAnotherUserProfile("getprofile", userId)
      printf("User Name : %s\n", response.username)
    }

    case PostOnUserPage(userId : Int, postData : String, postId : Int, userPk : PublicKey) => {
      var userdata = "user"+selfId.toString

      var key = postId.toString
      var currentAes = AES256.generateKey()
      keyMap += (key -> RSA.encrypt(currentAes, userPk))
      shareKey(key,userId)

      var data = RSA.encrypt(RSA.decrypt(token, pk), serverPuk)
      val loginreq = LoginData(userdata, data)
      val newPost = FacebookProtocol.Post(AES256.AESEncrypt(selfName, currentAes), AES256.AESEncrypt(postData, currentAes), "", loginreq, postId)

      //println(newPost.creator)
      println( postOnUserPage("posting", userId, newPost) )
    }

    case GetUserWallPage(userId : Int, postId : Int) => {
      var data = getUserWallPage("page", userId, postId)
      if(data.creator != "")
      {
        val k = RSA.decrypt(data.key, pk)
        var creator = AES256.AESDecrypt(data.creator, k)
        var content = AES256.AESDecrypt(data.content, k)
        printf("\nPOST:\nCreator : "+creator + "\n" + "Content : "+ content + "\n" + "Time : " + data.time + "\n\n")
      }
      else{
        printf("\nPOST:\nContent : "+ data.content+"\n\n")
      }
    }

    case AcceptAllRequests => {
      val str = "acceptallrequests"
      //acceptAllRequests(str)
      println( acceptAllRequests(str) )
      //sender ! AcceptedRequests
    }

    case ReturnErrors => {
      sender ! ReceiveErrorsCount(failedAttempts)
    }


    case UploadAnImage(imagedata:String,caption:String) => {
      var data = RSA.encrypt(RSA.decrypt(token, pk), serverPuk)
      var userdata = "user"+selfId.toString
      val loginreq = LoginData(userdata, data)

      var cap="From Actor " + selfId
      val newImage= Image(imagedata,cap,0, loginreq)
      println(uploadingImage("image", newImage) + " of actor " + selfName)
      //sender ! StartGettingAnImage
    }

    case GetAnImage(userId,imageId) =>
    {
        println(getImage("image",userId,imageId))
        system.shutdown
    }
  }

  import system.dispatcher
  def loginRequest(str : String) : Array[Byte] ={
    var response = Array[Byte]()
    try{
      val pipeline: HttpRequest => Future[Array[Byte]] = sendReceive ~> unmarshal[Array[Byte]]
      val f: Future[Array[Byte]] = pipeline(Post(s"$serverLocation/$str", selfId.toString))
      response = Await.result(f, timeout)
    } catch{
      case e: Exception =>
        printf("Failed login attempt with exception : %s\n",e)
        failedAttempts += 1
    }
    val returnData = RSA.encrypt(RSA.decrypt(response,pk), serverPuk)
    return login(returnData, "Login")
  }

  def login(data : Array[Byte], str : String) : Array[Byte] ={
    var response = Array[Byte]()
    var userdata = "user"+selfId.toString
    val loginreq = LoginData(userdata, data)
    try{
      val pipeline: HttpRequest => Future[Array[Byte]] = sendReceive ~> unmarshal[Array[Byte]]
      val f: Future[Array[Byte]] = pipeline(Post(s"$serverLocation/$str", loginreq))
      response = Await.result(f, timeout)
    } catch{
      case e: Exception =>
        printf("Failed login with exception : %s\n",e)
        failedAttempts += 1
    }
    //response = RSA.encrypt(RSA.decrypt(response, pk), serverPuk)
    return response
  }

  def shareKey(key : String, userId : Int) : String = {
    var response = ""
    var aes = keyMap(key)
    var str = "ShareKey"
    var map = AESKeyMap(key, aes)
    try{
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Post(s"$serverLocation/$str/$userId", map))
      response = Await.result(f, timeout)
    } catch{
      case e: Exception =>
        printf("Failed share with exception : %s\n",e)
    }
    return response
  }

  def stringCases(str : String, idOne : Int, idTwo : Int) : String = {
    var response = ""
    var userdata = "user"+selfId.toString
    var data = RSA.encrypt(RSA.decrypt(token, pk), serverPuk)
    val loginreq = LoginData(userdata, data)
    try{
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Post(s"$serverLocation/$str/$idOne/$idTwo", loginreq))
      response = Await.result(f, timeout)
    } catch{
      case e: Exception =>
        printf("Failed with exception : %s\n",e)
        failedAttempts += 1
    }
    return response
  }

  def createUser(newUser : User) : String = {
    var response = ""
    try{
      val timot = 5.seconds
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Post(s"$serverLocation/user", newUser))
      response = Await.result(f, timot)
    } catch{
      case e: Exception =>
        failedAttempts += 1
        printf("Failed create with exception : %s\n",e)
    }
    return response
  }

  // What to expect?????
  def getFriendsList(str : String) : String = {
    var response = ""
    var userdata = "user"+selfId.toString
    var data = RSA.encrypt(RSA.decrypt(token, pk), serverPuk)
    val loginreq = LoginData(userdata, data)
    try{
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Post(s"$serverLocation/$str/$selfId", loginreq))
      response = Await.result(f, timeout)
    }catch{
      case e:Exception =>
        printf("Failed get friends list with exception : %s\n",e)
    }
    return response
  }

  def getAnotherUserProfile(str : String, userId : Int) : User = {
    var response = User(0,"Fake","Fake","")
    var userdata = "user"+selfId.toString
    var data = RSA.encrypt(RSA.decrypt(token, pk), serverPuk)
    val loginreq = LoginData(userdata, data)
    try{
      val pipeline: HttpRequest => Future[User] = sendReceive ~> unmarshal[User]
      val f: Future[User] = pipeline(Post(s"$serverLocation/$str/$userId", loginreq))
      response = Await.result(f, timeout)
    } catch{
      case e: Exception =>
        failedAttempts += 1
        printf("Failed get profile with exception : %s\n",e)
    }
    return response
  }

  def postOnUserPage(str : String, receiverId : Int, newpost : FacebookProtocol.Post) : String = {
    var response = ""

    try{
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Post(s"$serverLocation/$str/$selfId/$receiverId/1", newpost))
      response = Await.result(f, timeout)
    } catch{
      case e: Exception =>
        failedAttempts += 1
        printf("Failed Post with exception : %s\n",e)
    }
    return response
  }

  def getUserWallPage(str : String, receiverId : Int, postId : Int) : KeyAndData = {
    var response = KeyAndData(Array[Byte](),"","","")
    var userdata = "user"+selfId.toString
    var data = RSA.encrypt(RSA.decrypt(token, pk), serverPuk)
    val loginreq = LoginData(userdata, data)
    try{
      val pipeline: HttpRequest => Future[KeyAndData] = sendReceive ~> unmarshal[KeyAndData]
      val f: Future[KeyAndData] = pipeline(Post(s"$serverLocation/$str/$postId/$selfId/$receiverId", loginreq))
      response = Await.result(f, timeout)
    } catch{
      case e: Exception =>
        failedAttempts += 1
        printf("Failed Get user wall with exception : %s\n",e)
    }
    return response
  }

  def acceptAllRequests(str : String) : String = {
    var response = ""
    var userdata = "user"+selfId.toString
    var data = RSA.encrypt(RSA.decrypt(token, pk), serverPuk)
    val loginreq = LoginData(userdata, data)
    try{
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Post(s"$serverLocation/$str/$selfId", loginreq))
      response = Await.result(f, timeout)
    } catch{
      case e: Exception =>
        printf("Failed accept all requests with exception : %s\n",e)
    }
    return response
  }

  def uploadingImage(str:String , newimage : Image) : String = {
    var response = ""
    try{
      val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
      val f: Future[String] = pipeline(Post(s"$serverLocation/$str/$selfId", newimage))
      response = Await.result(f, timeout)
    } catch{
      case e: Exception =>
        failedAttempts += 1
        printf("Failed to Post Image with exception : %s\n",e)
    }
    return response


  }

  def getImage(str:String,receiverId:Int,imageId:Int) : BufferedImage ={
    var response = Image("Fake","Fake",0,LoginData("", Array[Byte]()))
    var userdata = "user"+selfId.toString
    var data = RSA.encrypt(RSA.decrypt(token, pk), serverPuk)
    val loginreq = LoginData(userdata, data)
    try{
      val pipeline: HttpRequest => Future[Image] = sendReceive ~> unmarshal[Image]
      val f: Future[Image] = pipeline(Post(s"$serverLocation/$str/$selfId/$receiverId/0", loginreq))
      response = Await.result(f, timeout)
    } catch{
      case e: Exception =>
        failedAttempts += 1
        printf("User not allowed to access image\n")
    }
    var image:BufferedImage = StringToImage(response.imagedata)
    image
  }







  def setMap(){
    PublicKeyStore.setMap("user1", RSA.StringToPublic(RSA.PublicToString(puk)))
    PublicKeyStore.setMap("user2", RSA.StringToPublic(RSA.PublicToString(puk)))
    PublicKeyStore.setMap("user5", RSA.StringToPublic(RSA.PublicToString(puk)))
    PublicKeyStore.setMap("user6", RSA.StringToPublic(RSA.PublicToString(puk)))
    PublicKeyStore.setMap("user8", RSA.StringToPublic(RSA.PublicToString(puk)))
  }

}
