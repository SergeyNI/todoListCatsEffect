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
    val content = task.content.show
    s"$name: $content"  

  }
implicit val taskListShow:Show[TaskList] = Show.show[TaskList] { 
  tl =>
    val name = tl.name.show
    val count = tl.get.length.show
    s"$name:count - $count"  

  }

case class BoardViewer(board: Board) {
  
  def printTaskList(tl:TaskList):IO[Unit] =
    val mark = if board.isCurrent(tl)  then "*" else "-"
    printColorLine(s"$mark ${tl.show}",Console.RED)*>
    tl.get.traverse(task=>outputTask(task,tl)).void
  
  def outputTask(task:Task,tl:TaskList):IO[Unit] =
      val index = tl.get.indexOf(task)      
      val mark = if board.isCurrent(tl) && tl.isCurrent(task) then "*" else "-"
      IO(board.isCurrent(tl))*>IO(tl.isCurrent(task))*>printColorLine(s"$mark $index. ${task.show}", Console.CYAN)
  
  def showBoard:IO[Unit] = 
    val lists:List[TaskList] = board.getLists()   
    printColorLine(s"$board",Console.GREEN)*>lists.traverse(tl=>printTaskList(tl)).void
  
  // def taskList
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
