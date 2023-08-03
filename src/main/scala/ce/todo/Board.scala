package ce.todo

case class Board(name:String,var taskLists:List[TaskList] = Nil, var currentTaskList:Option[TaskList] = None){
  override def toString(): String = name
  def setCurrentList(list:TaskList):Unit = currentTaskList = Some(list)
  
  def getCurrentTaskList():Option[TaskList] = currentTaskList
  
  def getLists() = taskLists
  def isCurrent(list:TaskList) = currentTaskList.get == list
  
  def add(list:TaskList) =
    taskLists = list ::taskLists
    if taskLists.length == 1 then setCurrentList(list) 
  
  def list(index:Int):Option[TaskList] = if index >=0 && index < taskLists.length then Some(taskLists(index)) else None

  def remove(list:TaskList) = {
    taskLists = taskLists.filterNot(currlist => currlist == list)
    if taskLists.length == 1 then setCurrentList(taskLists(0)) 
  }


}
