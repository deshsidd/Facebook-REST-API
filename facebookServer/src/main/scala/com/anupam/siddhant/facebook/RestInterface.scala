package com.anupam.siddhant.facebook

import akka.actor._
import akka.util.Timeout
import spray.http.StatusCodes
import spray.httpx.SprayJsonSupport._
import spray.routing._

import spray.http.MultipartFormData

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.collection.mutable.ArrayBuffer
import java.util.Calendar
import java.security._
import javax.crypto.SecretKey
import scala.concurrent.ExecutionContext

case class LoginRegistered(data : Array[Byte])

class RestInterface(pk : PrivateKey, puk : PublicKey) extends HttpServiceActor
  with RestApi {
//curl -i 'http://localhost:8080/quizzes' -X POST -H "Content-Type: application/json" -d '{"id": "my_quiz_id", "question": "my_question", "correctAnswer": "my_answer"}'
  def receive =
              runRoute(routes(pk : PrivateKey, puk : PublicKey))
}

trait RestApi extends HttpService with ActorLogging { actor: Actor =>
  import com.anupam.siddhant.facebook.FacebookProtocol._

  implicit val timeout = Timeout(10 seconds)
  val currSecureNum = scala.collection.mutable.Map[String, String]()
  val tokenMap = scala.collection.mutable.Map[String, Array[Byte]]()

  def routes(pk : PrivateKey, puk : PublicKey): Route =

    pathPrefix("user") { // create user
        pathEnd{
        post{
          //log.info("Creating User")
          entity(as[User]) { newuser=> (ctx: RequestContext) =>
            val responder = createResponder(ctx, puk)

            createUser(newuser) match {
              case true => responder ! UserCreated
                           AllUsers+= (newuser.id -> newuser.username)
                           PublicKeyStore.setMap("user"+newuser.id.toString(), RSA.StringToPublic(newuser.password))
                           val profile=context.actorOf(Props(new Profile(newuser, pk)) , name= "user" + newuser.id)
                           val userid = "user"+newuser.id
                           printf("Creating user with userid %s\n", userid)
                            actorMap += (userid->profile)
              case _ => responder ! UserAlreadyExists
              //curl -i 'http://localhost:8080/user' -X POST -H "Content-Type: application/json" -d '{"id": 3, "name": "ramlah" , "username":"ramlah" , "password" : "sid"}'
            }
          }
         }
       }

      }~
      path(IntNumber) { id => //delete user
      delete { (ctx: RequestContext) =>
         val responder = createResponder(ctx, puk)
         deleteUser(id)
         responder ! UserDeleted
       }
   }~
      path("LoginRequest") {
          post{
            entity(as[String]) { userId=> (ctx: RequestContext) =>
                //println("Inside LOGIN REQUEST")
                  currSecureNum +=   "user"+userId->Security.SRNG().toString
                //  println("Getting TESTSUM")
                  val testNum = RSA.encrypt(currSecureNum("user"+userId), PublicKeyStore.getMap("user"+userId))
                  ctx.complete(testNum)

            }
        }
   }~
   path("Login") {
       post{
         entity(as[LoginData]) { dataEntity=> (ctx: RequestContext) =>
           if( currSecureNum(dataEntity.userid) == RSA.decrypt(dataEntity.data, pk) ){
             println("User logged in. Token issued")
             //Thread sleep 2000
             tokenMap += dataEntity.userid -> RSA.encrypt(Security.SRSG(), puk)
             currSecureNum(dataEntity.userid) = ""
             ctx.complete(RSA.encrypt(RSA.decrypt(tokenMap(dataEntity.userid), pk), PublicKeyStore.getMap(dataEntity.userid)))
           }
           else{
             printf("User %s could not login", dataEntity.userid)
           }
           currSecureNum(dataEntity.userid) = ""
         }
     }
   }~
    path("users"){ // get list of all users
      get{
        complete{
          log.info("getting users")
          Users
          //curl "http://localhost:8080/users"
        }
      }
    }~
    path("addfriend"/IntNumber/IntNumber){ (senderid,receiverid) => // add friend
      post{
        entity(as[LoginData]) { token=> (ctx: RequestContext) =>
        if(RSA.decrypt(token.data, pk) == RSA.decrypt(tokenMap(token.userid), pk)){
          log.info("adding friend")
          var sender=actorMap("user"+senderid)
          var receiver=actorMap("user"+receiverid)
           receiver ! FriendRequest(senderid)
           sender ! RequestSent(receiverid)
           ctx.complete("friend request sent")
         }

         ctx.complete("Fake user")

           //curl -i 'http://localhost:8080/addfriend/1/2' -X POST
         }
      }
    }~
    path("acceptrequest"/IntNumber/IntNumber){ (senderid,receiverid) => //accept friend request
      post{
        entity(as[LoginData]) { token=> (ctx: RequestContext) =>
        if(RSA.decrypt(token.data, pk) == RSA.decrypt(tokenMap(token.userid), pk)){
          log.info("accepting friend request")
          var sender=actorMap("user"+senderid)
          var receiver=actorMap("user"+receiverid)
           receiver ! RequestAccepted(senderid)
           sender ! AcceptRequest(receiverid)
           ctx.complete("friend request accepted")
        }
        ctx.complete("Fake user")
      }
    }

    }~
    path("page"/IntNumber/IntNumber/IntNumber){ (pageid,senderid,receiverid) => // get page
      post{
        entity(as[LoginData]) { token=> (ctx: RequestContext) =>
          //log.info("getting user page")
          var sender=actorMap("user"+senderid)
          var receiver=actorMap("user"+receiverid)
          if(RSA.decrypt(token.data, pk) == RSA.decrypt(tokenMap(token.userid), pk)){
            receiver ! PageRequest(pageid,senderid,ctx)
          }
          else{
            ctx.complete("Unauthorized access")
          }
        }
      }
    }~
    //any user can get friend list of any other user and no access control is present
    path("friendlist"/IntNumber){ (senderid)=> //get friendlist of user
      post{
        entity(as[LoginData]) { token=> (ctx: RequestContext) =>
        log.info("getting friend list")
        if(RSA.decrypt(token.data, pk) == RSA.decrypt(tokenMap(token.userid), pk)){
          var sender=actorMap("user"+senderid)
          sender ! GetFriendList(ctx)
        }
        else{
          ctx.complete("Unauthorized")
        }
      }
    }
    }~
    path("acceptallrequests"/IntNumber){ userid => // accept all friend requests of a user
      post{
        entity(as[LoginData]) { token=> (ctx: RequestContext) =>
          var user=actorMap("user"+userid)
          if(RSA.decrypt(token.data, pk) == RSA.decrypt(tokenMap(token.userid), pk)){
            user ! AcceptAll(ctx)
          }
          else{
            ctx.complete("Unauthorized access")
          }
      }
    }
    }~
    path("posting"/IntNumber/IntNumber/IntNumber) { (senderid,receiverid,pageid)=> // post on someones page

      post{
        entity(as[com.anupam.siddhant.facebook.FacebookProtocol.Post]) { newpost=> (ctx: RequestContext) =>
          var current :String= (Calendar.getInstance.getTime).toString
          var emptyToken = LoginData("", Array[Byte]())
          var np= new com.anupam.siddhant.facebook.FacebookProtocol.Post(newpost.creator,newpost.content,current, emptyToken, newpost.id)

          var sender=actorMap("user"+senderid)
          var receiver=actorMap("user"+receiverid)
          if(RSA.decrypt(newpost.token.data, pk) == RSA.decrypt(tokenMap(newpost.token.userid), pk)){
            receiver ! Posting(senderid,pageid,np,ctx)
          }
          else{
            ctx.complete("Unauthorized access")
          }
        }
      }

    }~
    path("getprofile"/IntNumber){ (userid) => // get a profile of some user
      post{
        entity(as[LoginData]) { token=> (ctx: RequestContext) =>
        if(RSA.decrypt(token.data, pk) == RSA.decrypt(tokenMap(token.userid), pk)){
          var user=actorMap("user"+userid)
          user ! GetProfile(ctx)
        }
      }
      }
    }~
    path("ShareKey"/IntNumber){ (userid) =>
      post{
        entity(as[com.anupam.siddhant.facebook.FacebookProtocol.AESKeyMap]) { newKeyMap=> (ctx: RequestContext) =>
          var sender=actorMap("user"+userid)
          sender ! ShareKey(ctx, newKeyMap)
      }
    }
    }~
path("image"/IntNumber){ (uploaderid) =>
  post{
    entity(as[Image]){ newimage=> (ctx: RequestContext) =>
        var uploader=actorMap("user"+uploaderid)
        if(RSA.decrypt(newimage.token.data, pk) == RSA.decrypt(tokenMap(newimage.token.userid), pk)){
          uploader ! UploadImage(newimage,ctx)
        }
        else{
          ctx.complete("Unauthorized access")
        }
    }

  }
}~
path("image"/IntNumber/IntNumber/IntNumber) { (senderid,receiverid,imageid) =>
  post{
    entity(as[LoginData]){ token=> (ctx: RequestContext) =>
      var sender=actorMap("user"+senderid)
      var receiver=actorMap("user"+receiverid)
      if(RSA.decrypt(token.data, pk) == RSA.decrypt(tokenMap(token.userid), pk)){
        receiver ! ImageRequest(imageid,senderid,ctx)
      }
      else{
        ctx.complete("Unauthorized access")
      }
    }
  }
}


////////////////
///////////////
///METHODS/////
///////////////
//////////////
  private def createResponder(requestContext:RequestContext, puk : PublicKey) = {
    context.actorOf(Props(new Responder(requestContext, puk)))
  }

  private def createUser(newuser: User): Boolean = {
    val doesNotExist = !Users.exists(_.id == newuser.id)
    if (doesNotExist) Users = Users :+ newuser
    doesNotExist
  }

  private def deleteUser(id: Int): Unit = {
    Users = Users.filterNot(_.id == id)
  }
  private def createGroup(newgroup: Group): Boolean = {
    val doesNotExist = !Groups.exists(_.id == newgroup.id)
    if (doesNotExist) Groups = Groups :+ newgroup
    doesNotExist
  }

}
////////////////
///////////////
///RESPONDER///
///////////////
//////////////

class Responder(requestContext:RequestContext, puk : PublicKey) extends Actor with ActorLogging {
  import com.anupam.siddhant.facebook.FacebookProtocol._


  def receive = {

    case UserCreated =>
      requestContext.complete(RSA.PublicToString(puk))
      killYourself

    case UserDeleted =>
      requestContext.complete(StatusCodes.OK)
      killYourself

    case UserAlreadyExists =>
      requestContext.complete("ERROR")
      killYourself

    case GroupCreated => requestContext.complete(StatusCodes.OK)
                           killYourself

    case GroupAldreadyExists => requestContext.complete("ERROR")
                                killYourself

    case LoginRegistered(data : Array[Byte]) =>
      requestContext.complete(data)
      killYourself

  }

  private def killYourself = self ! PoisonPill

}


import com.anupam.siddhant.facebook.FacebookProtocol._

////////////
////////////
//PROFILE///
////////////
///////////
class Profile(user:User, pk : PrivateKey) extends Actor with ActorLogging{
   import com.anupam.siddhant.facebook.FacebookProtocol._
   import spray.json.DefaultJsonProtocol._
   import spray.httpx.SprayJsonSupport._

var arr = Vector[Post]()

var ProfilePage=new Page(1,user.username, arr)
var pagecounter=1
var pages= ArrayBuffer[Page]()
pages.append(ProfilePage)
var FriendList= Vector[Int]()
var Friends= Vector[String]()
var FriendRequestsList= ArrayBuffer[Int]()
var RequestsSentList= ArrayBuffer[Int]()
var keyMap = scala.collection.mutable.Map[String, Array[Byte]]()

var Album=ArrayBuffer[Image]()
var albumcounter=0


def checkPage(pageid : Int) : Boolean = {
  for( i <- 0 to pages.length-1){
    if(pages(i).id == pageid)
      return true
  }
  return false
}

def checkAlbum(imageid : Int) : Boolean = {
  for( i <- 0 to Album.length-1){
    if(Album(i).id == imageid)
      return true
  }
  return false
}

def getListOfStrings() : String = {
  //var tempList = List[String]()
  var tempString = ""
  for(i <- 0 to pages(0).posts.length-1){
    val post = pages(0).posts(i)
    tempString += post.creator + "\n" + post.content + "\n" + post.time + "\n\n"
    //tempList = tempList :: tempString
  }
  return tempString
}

def getListOfStrings(post : Post) : String = {
  //var tempList = List[String]()
  var tempString = ""
  tempString += "Creator : "+post.creator + "\n" + "Content : "+ post.content + "\n" + "Time : " + post.time + "\n\n"
    //tempList = tempList :: tempString
  return tempString
}

def getFriendsList() : String = {
  var tempString = ""
  for(i <- 0 to Friends.length-1){
    tempString += Friends(i) + "\n"
  }
  return tempString
}

def receive= {
  case FriendRequest(userid) => FriendRequestsList.append(userid)

  case RequestSent(userid) => RequestsSentList.append(userid)

  case RequestAccepted(userid) => RequestsSentList-=userid
                                  FriendList = FriendList :+ userid
                                  Friends = Friends :+ AllUsers(userid)

  case AcceptRequest(userid) => FriendRequestsList-=userid
                                FriendList = FriendList :+ userid
                                Friends = Friends :+ AllUsers(userid)

  case PageRequest(pageid,userid,ctx) =>
                                      if(FriendList.contains(userid))
                                      {
                                          if(checkPage(1))
                                          {
                                                if(pages(0).posts.length > 0)
                                                {
                                                var post = pages(0).posts(0)
                                                if(post.id == pageid)
                                                {
                                                  /*println(post.id)
                                                  println(pageid)
                                                  println(keyMap)*/
                                                  //var k = RSA.decrypt(keyMap(pageid.toString), pk)
                                                  //var strK = RSA.encrypt(k, PublicKeyStore.getMap("user"+userid))
                                                  var data = KeyAndData(keyMap(pageid.toString),post.creator,post.content,post.time)
                                                  ctx.complete(data)
                                                }
                                                else{
                                                  ctx.complete(KeyAndData(Array[Byte](),"","Unauthorized user",""))
                                                }
                                              }
                                              else{
                                                ctx.complete(KeyAndData(Array[Byte](),"","Unauthorized user",""))
                                              }
                                          }

                                          else
                                          {
                                            ctx.complete(KeyAndData(Array[Byte](),"","Post not accessible",""))
                                          }
                                      }
                                      else
                                      {
                                        ctx.complete(KeyAndData(Array[Byte](),"","Page not accessible",""))
                                      }

    case GetFriendList(ctx) => ctx.complete{
                                                      //printf("List of all friends of %d\n",user.id)
                                                      //FriendList.foreach(println)
                                                      //Friends.foreach(println)
                                                      getFriendsList()
                                                  }

    case AcceptAll(ctx) =>
                          var it = FriendRequestsList.iterator
                           while(it.hasNext)
                           {
                           var receiverid = it.next
                           var receiver=actorMap("user"+receiverid.toString())
                           FriendList = FriendList :+ receiverid
                           Friends = Friends :+ AllUsers(receiverid)
                           receiver ! RequestAccepted(user.id)
                          }
                          //FriendList.foreach(println)
                          //Friends.foreach(println)
                           FriendRequestsList.clear()
                           ctx.complete{
                               "Accepted All Requests"
                           }

    case Posting(userid,pageid,newpost,ctx)=>
                                          //println("Gonna try to post now")
                                            if(FriendList.contains(userid))
                                          {
                                            if(checkPage(pageid))
                                           {
                                             //println("Found the page and posted")
                                              ctx.complete{
                                                var newPosts = pages(0).posts :+ newpost
                                                pages.clear()
                                                pages.append(Page(1,user.username,newPosts))
                                                //printf("Number of posts : %d\n", newPosts.length)
                                                printf("\nPOST:\nCreator : "+newpost.creator + "\n" + "Content : "+ newpost.content + "\n" + "Time : " + newpost.time + "\n\n")
                                                //getListOfStrings(getPost(newpost))
                                                "Data Posted on server"
                                              }
                                           }
                                           else
                                           {
                                              println("Page not found")
                                              ctx.complete("Page doesn't exist to post.")
                                           }
                                          }
                                          else
                                          {
                                            println("Not Allowed to post on user")
                                              ctx.complete("Not allowed to post on page.")
                                          }

    case GetProfile(ctx) => ctx.complete{
                                            user
                                        }

    case UploadImage(newimage,ctx) => ctx.complete{
                                                  var img=new Image(newimage.imagedata,newimage.caption,albumcounter,LoginData("",Array[Byte]()))
                                                  albumcounter+=1
                                                  Album.append(img)
                                                  "image added to album"
                                                  }

    case ImageRequest(imageid,userid,ctx) => if(FriendList.contains(userid))
                                                {
                                                    if(checkAlbum(imageid))
                                                    {
                                                        ctx.complete{
                                                         Album(imageid)
                                                        }
                                                    }
                                                    else
                                                    {
                                                      ctx.complete(null)
                                                      println("Image not Found in album")
                                                    }
                                                }
                                                else
                                                {
                                                  println("User not allowed to access image")
                                                  ctx.complete(null)
                                                }

  case ShareKey(ctx, map) => ctx.complete{
                                            keyMap += (map.mapKey ->map.aes)
                                            "Key Shared"
                                          }

  case _ => println("received something fishy..")
}

/*def getPost(post : Post) : Post ={
  var key = "user"+user.id.toString+post.id.toString
  if(!keyMap.contains(key)){
    return Post("","Don't have permission to view","",post.token,post.id)
  }
  else{
  var dec = keyMap(key)
  var retpost = Post(AES256.AESDecrypt(post.creator, dec), AES256.AESDecrypt(post.content, dec), post.time, post.token, post.id)
  return retpost
  }
}*/


}
////////////
////////////
//GROUP/////
////////////
///////////

class GroupProfile(group:Group) extends Actor with ActorLogging{
   var GroupUsersList=Vector[Int]()
   var arr = Vector[Post]()
   var GroupPage=new Page(group.id,group.creator, arr)

   def receive={
    case JoinGroup(userid,ctx) => if(GroupUsersList.contains(userid))
                                  {
                                      ctx.complete(StatusCodes.Conflict)
                                  }
                                  else
                                  {
                                      GroupUsersList = GroupUsersList :+ userid
                                      log.info(userid + " is now a member of group :" + group.name)
                                  }

    case GroupPosting(userid,newpost,ctx) => if(GroupUsersList.contains(userid))
                                            {
                                                ctx.complete{
                                                GroupPage.posts :+ newpost
                                                newpost
                                                            }

                                            }
                                            else
                                            {
                                                ctx.complete("ERROR")
                                            }
    case _ => println("received some thing fishy")
   }

}
