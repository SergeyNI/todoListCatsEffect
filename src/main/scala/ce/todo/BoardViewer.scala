package ce.todo


import cats.Show
import cats.effect._

import cats.implicits._
// for Show
import cats.instances.string._ // for Show
import cats.syntax.show._
// for show
implicit val taskShow:Show[Task] = Show.show[Task] { 
  task =>
    val name = task.name.show
    val content = task.name.show
    s"$name: $content"  

  }
implicit val taskListShow:Show[TaskList] = Show.show[TaskList] { 
  tl =>
    val name = tl.name.show
    val count = tl.get.length.show
    s"$name:count - $count"  

  }

case class BoardViewer(board: Board) {
  def printLine(str:String,color: String):IO[Unit] = IO(println(color+str))
  def printTaskList(tl:TaskList):IO[Unit] =
    printLine(tl.name,Console.RED)*>
    tl.get.traverse(task=>outputTask(task,tl.get)).void
  def outputTask(task:Task,l:List[Task]):IO[Unit] =
      val index = l.indexOf(task)
      //val mark = markCurrentTask(task)
      val mark = "+"
      val taskShow = task.show
      IO(println(Console.CYAN+s"$mark $index. $taskShow"))
  def showBoard:IO[Unit] = 
    val lists:List[TaskList] = board.getLists()
    printLine(s"$board",Console.GREEN)*>lists.traverse(tl=>printTaskList(tl)).void
  
  
  //def showList(tl:TaskList):IO[Unit] =
    
    // def markCurrentList(list:TaskList):String = 
    //   val someCurrentList:Option[TaskList] = board.getCurrentTaskList()
    //   someCurrentList match
    //     case Some(l) if l == list => "*"
    //     case _=>""
    
    // def markCurrentTask(task:Task):String =
    //   val isCurrentList = this.board.isCurrent(tl)
    //   if tl.isCurrent(task) && isCurrentList then "*" else ""
    
    // def outputTask(task:Task,l:List[Task]):Unit =
    //   val index = l.indexOf(task)
    //   val mark = markCurrentTask(task)
    //   val taskShow = task.show
    //   println(Console.CYAN+s"$mark $index. $taskShow")
    
    
  
     
    // val ptl = IO(tl.get.foreach(task => 
    //   val index = l.indexOf(task)
    //   val mark = markCurrentTask(task)
    //   val taskShow = task.show
    //   val ot = outputTask(task:Task,l:List[Task])
      // println(Console.CYAN+s"$mark $index. $taskShow")))
    //))
    // head>>ptl

    // println('\n'+Console.MAGENTA+s"$mark ($listIndex)$listshow"+'\n')
    // tl.get.foreach(task => 
    //   val index = l.indexOf(task)
    //   val mark = markCurrentTask(task)
    //   val taskShow = task.show
    //   println(Console.CYAN+s"$mark $index. $taskShow"))
    


}
