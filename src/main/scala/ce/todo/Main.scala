package ce.todo

import cats.effect.IOApp
// import cats.effect.IO
import CommandsLib._
import cats.effect.*
import cats.implicits.*
import cats.implicits
import cats.instances.string

def printColorLine(str:String,color: String):IO[Unit] = IO(println(color+str))
def printColor(str:String,color: String):IO[Unit] = IO(print(color+str))
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
      _<-printColorLine("enter your command",Console.WHITE)*>printColor(">",Console.WHITE)
      str<-IO.readLine
      commandParts<-parse(str)
      _<-IO.println(commandParts)
      _<-executeCommand(b,commandParts)
      _<-BoardViewer(b).showBoard
      _<-commander(b)
    } yield ExitCode.Success
  }
  

  def parse(s:String):IO[List[String]] = s.split(" ").toList.pure[IO]
  
  def executeCommand(b:Board,commandParts:List[String]):IO[Unit]=
    given board:Board = b
    
    def getCommandParameter(commandParts:List[String]):Option[Either[String,(String,String)]]=
      commandParts match
        case command :: parameter :: Nil => Some(Right((command,parameter)))
        case command :: Nil => Some(Left(command))
        case _ => None
    
    def getFunctionWithParam(command:String):Option[Function1[String,IO[Unit]]] =
      command match
        case "sl" => Some(selectList)
        case "st" => Some(selectTask)
        case _=> None
    
    def getFunctionWithoutParams(command:String): Option[IO[Unit]] =
      command match {
        case "show" => Some(BoardViewer(b).showBoard)
        case _=> None
      }
    
    
    getCommandParameter(commandParts) match
        case None => IO(())
        case Some(commandType) =>
          commandType match {
            case Right((command,parameter)) => getFunctionWithParam(command) match
              case None => IO(println(s"not found command '$command' with parameter"))
              case Some(f) => f(parameter)
            case Left(command) => getFunctionWithoutParams(command) match
              case None => IO(println(s"Not found command '$command'"))
              case Some(f) => f 
            
          }
            

  
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