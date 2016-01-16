import akka.actor._

import com.anupam.siddhant.facebook.Images._
import com.anupam.siddhant.facebook.Security._

import java.awt.image.BufferedImage
import java.io._
import javax.imageio.ImageIO
//import java.util._
import java.nio.charset.StandardCharsets
import org.apache.commons.codec.binary.Base64
import scala.util.Random
import java.security._
import com.anupam.siddhant.facebook._

object Main {

  var noOfClients = 10

  def main(args : Array[String]){
    val system = ActorSystem("ClientSystem")

    if(args.length < 1){
      printf("Using default number of clients : %d\n", noOfClients)
    }
    else{
      noOfClients = args(0).toInt
      printf("Using %d number of clients\n", noOfClients)
    }

    val topologyActor = system.actorOf(Props(new TopologyActor(system, noOfClients)), "TopologyActor")
    topologyActor ! StartOperation
  }
}

class TopologyActor(system : ActorSystem, noOfClients : Int) extends Actor{
  var actorsList = List[ActorRef]()
  var totalErrors = 0
  var usersStarted = 0
  var friendsAdded = 0
  var inYet = false
  var friendlistmap = scala.collection.mutable.Map[Int, List[Int]]()
  var two = false;
  var idCounter = 0

  def receive = {
    case StartOperation => {
      for(i <- 0 to noOfClients){
          val id = i
          val strId = id.toString()
          val (puk, pk) = RSA.getRSAKeys()
          PublicKeyStore.setMap(strId, puk)
          val actor = context.actorOf(Props(new ClientActor(id, system, pk, puk)), strId)
          actor ! InitializeUser("Actor "+strId, "actor"+strId);
          actorsList = actorsList ::: List(actor)
      }
      println("After Creating ...... ")
      //val stop = readInt()
      actorsList(4) ! AddFriend(8)
      actorsList(4) ! AddFriend(5)
      actorsList(4) ! AddFriend(6)
      actorsList(4) ! AddFriend(1)
      actorsList(6) ! AddFriend(2)
      actorsList(2) ! AddFriend(0)
      Thread sleep 1000
      println("Accepting all requests .......")
      //actorsList.foreach((actor : ActorRef) => actor ! AcceptAllRequests)
      actorsList(8) ! AcceptRequest(4)
      actorsList(5) ! AcceptRequest(4)
      //Thread sleep 2000
      actorsList(6) ! AcceptRequest(4)
      actorsList(2) ! AcceptRequest(6)
      actorsList(0) ! AcceptRequest(2)
      actorsList(1) ! AcceptRequest(4)
      Thread sleep 2000
      println("Posting Data .......")
      actorsList(4) ! PostOnUserPage(8, "Actor 4 post on actor 8",2,PublicKeyStore.getMap("user"+"8"))
      actorsList(4) ! PostOnUserPage(5, "Actor 4 post on actor 5",3,PublicKeyStore.getMap("user"+"5"))
      //actorsList(6) ! PostOnUserPage(2, "Actor 6 post on actor 2",4,PublicKeyStore.getMap("user"+"2"))
      actorsList(4) ! PostOnUserPage(6, "Actor 4 post on actor 6",1,PublicKeyStore.getMap("user"+"6"))
      actorsList(4) ! PostOnUserPage(2, "Actor 3 post on actor 2",5,PublicKeyStore.getMap("user"+"2"))
      //actorsList(6) ! PostOnUserPage(8, "Actor 6 post on actor 8",6,PublicKeyStore.getMap("user"+"6"))
      actorsList(4) ! PostOnUserPage(1, "Actor 1 post on actor 4",7,PublicKeyStore.getMap("user"+"1"))
      Thread sleep 3000
      println("Getting Posts .........")
      actorsList(4) ! GetUserWallPage(6,1)
      actorsList(4) ! GetUserWallPage(5,3)
      //Thread sleep 1000
      actorsList(4) ! GetUserWallPage(2,4)
      actorsList(4) ! GetUserWallPage(8,2)
      actorsList(4) ! GetUserWallPage(6,5)
      actorsList(6) ! GetUserWallPage(2,4)
      actorsList(4) ! GetUserWallPage(1,7)
      Thread sleep 1000
      // Images
      try{
        var  image: BufferedImage = ImageIO.read(new File("C:/Users/AnupamBahl/Desktop/New folder/Fb_Final - Copy/facebookClient/src/main/scala/images/square.BMP")) //get image
        var str=ImageToString(image)
        actorsList(0) ! UploadAnImage(str,"")
      } catch {
        case e: Exception =>
          printf("%s\n",e)
          system.shutdown
      }
      Thread sleep 1000
      actorsList(2) ! GetAnImage(0,0)

      //actorsList(4) ! GetFriendsList
      //Thread sleep 1000
      //system.shutdown()
    }

    case UserCreated =>{
      if(usersStarted == noOfClients && !inYet){
        inYet = true
        self ! AddFriends
      }
      else if(usersStarted < noOfClients){
        usersStarted += 1
      }
    }

    case AddFriends => {
      var first = (noOfClients/2).toInt
      var count = 0
      for(i <- 0 to first ){
        count += 2
        //30 100 150 500
        var tempList = getListOfNumbers(2)
        friendlistmap += (i -> tempList)
        tempList.foreach((num : Int) => actorsList(i) ! AddFriend(num))
      }
      for( i <- first+1 to noOfClients-1){
        count += 2
        var tempList = getListOfNumbers(2)
        friendlistmap += (i -> tempList)
        tempList.foreach((num : Int) => actorsList(i) ! AddFriend(num))
      }
      actorsList(0) ! AddFriend(2)
      usersStarted = count+1
      //printf("No of users processed for friends : %d\n", usersStarted)
    }

    case FriendsAdded => {
      friendsAdded += 1
      if(friendsAdded == usersStarted){
        printf("Friends requests sent :%d out of :%d\n", friendsAdded, usersStarted)
        friendsAdded = 0
        actorsList.foreach((actor : ActorRef) => actor ! AcceptAllRequests)
      }
    }

    case AcceptedRequests => {
      if(friendsAdded == noOfClients){
        printf("Friend Requests Accepted by :%d users\n",friendsAdded)
        println("\n\n##########\nSystem ready to start simulation\n##########\n")
        Thread sleep 2000
        println("\n")
        self ! StartPostingOne
      }
      else if(friendsAdded < noOfClients){
      friendsAdded += 1
      //
      }
    }

    case StartPostingOne => {
      printf("Group one started posting data to random friends, covering each friend exactly once.\n")
      for(i <-1 to 5){
        var str = "From Actor :"+i.toString()
      //  friendlistmap(i).foreach{(num : Int) => actorsList(i) ! PostOnUserPage(num, str, idCounter)
    //                                        idCounter += 1}
      }
      Thread sleep 2000
      usersStarted = 0
      for(i <- 1 to 5){
        actorsList(i) ! ReturnErrors
      }
    }

    case StartPostingTwo => {
      two = true
      printf("Group Two started posting data to random friends, covering each friend exactly once.\n")
      Thread sleep 5000
      idCounter += 1
      for(i <-5 to 9){
        var str = "From Actor :"+i.toString()
      //  friendlistmap(i).foreach{(num : Int) => actorsList(i) ! PostOnUserPage(num, str,idCounter)
      //                                          idCounter += 1}
      }
      Thread sleep 2000
      usersStarted = 0
      for(i <- 5 to 9){
        actorsList(i) ! ReturnErrors
      }
    }

    case GetErrors => {
      for(i <- 1 to actorsList.length){
        actorsList(i) ! ReturnErrors
      }
    }

    case ReceiveErrorsCount(num : Int) => {
      if(two){
        totalErrors += num
        usersStarted += 1
        if(usersStarted == 5){
          printf("Number of conflict errors for this group : %d, with timeout 5 sec\n", totalErrors)
          self ! StartGettingPosts
        }
      }
      else{
      totalErrors += num
      usersStarted += 1
      if(usersStarted == 5){
        printf("Number of conflict errors for this group : %d, with timeout 5 sec\n\n", totalErrors)
        usersStarted = 0
        self ! StartPostingTwo
      }
      }
    }

    case StartGettingPosts => {
      friendlistmap(5)
    }

    case StartUploadingImages => {
      try{
        var  image: BufferedImage = ImageIO.read(new File("C:/Users/AnupamBahl/Desktop/Fb_3/facebookClient/src/main/scala/square.BMP")) //get image
        var str=ImageToString(image)
        actorsList(0) ! UploadAnImage(str,"")
      } catch {
        case e: Exception =>
          printf("%s\n",e)
          system.shutdown
      }
    }

    case StartGettingAnImage =>
                                  {
                                    //var n=Random.nextInt(actorsList.length-1)
                                     //actorsList(n) ! GetAnImage(n,0)
                                     actorsList(2) ! GetAnImage(0,0)
                                  }
  }

  def getListOfNumbers(num : Int) : List[Int]={
    val r = scala.util.Random
    var list = List[Int]()
    for( i<- 1 to num){
      list = list ::: List(r.nextInt(noOfClients-1))
    }
    return list
  }
}
