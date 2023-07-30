package ce.todo

import cats.effect.IOApp
// import cats.effect.IO
import CommandsLib._
import cats.effect.*
import cats.implicits.*
import cats.implicits
import cats.instances.string

object Main extends IOApp {

  // This is your new "main"!
  def run(args: List[String]): IO[ExitCode] = {
    val b = init()
    for{
      _<-BoardViewer(b).showBoard
      exitcode <-commander(b)
    } yield exitcode
  }
  def commander(b:Board):IO[ExitCode] ={
    for{
      _<-IO(println("enter your command"))
      str<-IO.readLine
      commandParts<-parse(str)
      _<-IO.println(commandParts)
      _<-executeCommand(b,commandParts)
      _<-BoardViewer(b).showBoard
      _<-commander(b)
    } yield ExitCode.Success
  }

  val input:IO[String] =  IO.readLine
  
  def parse1(s:String):IO[Unit] = IO(println(s"entered string is $s"))
  
  def parse(s:String):IO[List[String]] = s.split(" ").toList.pure[IO]
  
  def executeCommand(b:Board,commandParts:List[String]):IO[Unit]=
    given board:Board = b
    
    def getCommandParameter(commandParts:List[String]):Option[(String,String)]=
      commandParts match
        case command :: parameter :: Nil => Some((command,parameter))
        case command :: Nil => Some((command,""))

        case _ => None
    
    def getFunction(command:String):Option[Function1[String,IO[Unit]]] =
      command match
        case "sl" => Some(selectList)
        case "st" => Some(selectTask)
        case _=> None
    
    
    getCommandParameter(commandParts) match
        case None => IO(())
        case Some((command,parameter)) =>
          getFunction(command) match
            case None => IO(())
            case Some(f) => f(parameter)*>IO(println(s"comm $command param $parameter"))  

  
  def init() = 
    val b = Board("Main")
    
    val optionTLtodo = TaskList("TO DO")
    val optionTaskCreate = Task("create Program","Create program on some programing language")
    val optionTaskTest = Task("test Program","Test created program on some test library")
   
    optionTLtodo.map(tlTODO=>
      optionTaskCreate.map(t=>tlTODO.add(t))
      optionTaskTest.map(t=>tlTODO.add(t))
      b.add(tlTODO) )
    val optionTLDONE = TaskList("DONE")
    
    optionTLDONE.map(tld =>
      Task("Make Project ","Make project of program").map(t=> tld.add(t))
      b.add(tld)
)
    b
}