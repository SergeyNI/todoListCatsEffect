package ce.todo


case class TaskList(name:String):
  
  private var tasks: List[Task] = Nil
  private var currentTask: Option[Task] = None
  
  def setCurrentTask(task:Option[Task]):Boolean =
    task match
      case Some(t) =>
        val result = 
          if tasks.exists(curr => curr == t) then
            currentTask = Some(t)
            // println(s"in list $this current task is $t")
            true
          else
            // println(s"in list '$this' task '$t' not found!!Unable set current list  ")
            false
          end if
        result
      case None => currentTask = None;true
  
  def isCurrent(task:Task):Boolean = 
    currentTask match
      case None => false
      case Some(value) => value == task
    
    currentTask.get == task
  
  def add(task:Task) = 
    tasks = tasks:+task
    if tasks.length == 1 then setCurrentTask(Some(task))
  
  def current = currentTask
  def task(index:Int):Option[Task] = 
    try
      Some(tasks(index))
    catch 
      case _ => None
  
  
  def get:List[Task] = tasks
  
  def up(task:Task):Unit =
    if  tasks.contains(task) then
      val (leftTaskswithoutCurr,rightTasksWithCurr) = tasks.splitAt(tasks.indexOf(task))
      val newrightTasksWithoutCurr =  rightTasksWithCurr match
        case head :: tail => tail
        case _ => List()
      val newLeftListWithCurr = leftTaskswithoutCurr match
        case headList:+last => headList:+task:+last
        case _ => leftTaskswithoutCurr
       
      tasks = newLeftListWithCurr ::: newrightTasksWithoutCurr
    
  def down(task:Task):Unit =
    if tasks.contains(task) then
      val (leftTaskswithoutCurr,rightTasksWithCurr) = tasks.splitAt(tasks.indexOf(task))
      val newrightTasksWithCurr =  rightTasksWithCurr match
        case curr::head::tail => head::curr::tail
        case curr :: Nil => curr::Nil
        case Nil => Nil
      tasks = leftTaskswithoutCurr:::newrightTasksWithCurr

  def moveTo(task:Task, newTaskList:TaskList):Unit =
    newTaskList.add(task)
    remove(task)
    setCurrentTask(tasks match
      case Nil => None
      case head::tail => Some(head)
      )
  
  def remove(task:Task) =
    if tasks.contains(task) then
      tasks = tasks.filter(currTask => currTask != task)

object TaskList:
  def apply(name:String):Option[TaskList]= if name.length() >3 then Some(new TaskList(name)) else None
