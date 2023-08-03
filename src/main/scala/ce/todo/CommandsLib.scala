package ce.todo
import cats.effect.IO

object CommandsLib {
  def selectList(parameter:String)(using b:Board):IO[Unit] = 
      parameter.toIntOption match
        case None => IO.println(s"can't convert index of list $parameter to Int")
        case Some(index) => b.list(index) match 
          case Some(taskList) => IO(b.setCurrentList(taskList))*> IO.println(s"seted  current task list $taskList")
          case None => IO.println(s"can't find task list by index $index")
  

  def selectTask(parameter:String)(using b:Board):IO[Unit] =
    b.getCurrentTaskList() match 
      case None => IO.println("not found current tasklist")
      case Some(taskList) => parameter.toIntOption match
        case None => IO.println(s"can't convert index  of task $parameter to Int")
        case Some(index) => 
          taskList.task(index) match
          case None => IO.println(s"can't find task list by index $index")
          case Some(task) => IO(taskList.setCurrentTask(Some(task)))*> IO.println(s"seted  current task $task")
}
