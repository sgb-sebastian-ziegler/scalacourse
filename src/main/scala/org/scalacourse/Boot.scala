package org.scalacourse

import com.synergygb.zordon.common.data.DataTransformContext
import com.synergygb.zordon.core.ServiceBoot
import com.synergygb.zordon.gen.models.{Task, Project, Error}
import com.synergygb.zordon.gen.routes.ApplicationRoutesConsolidated
import spray.routing.Route
import spray.http.StatusCodes._

object Boot extends App with ServiceBoot with ApplicationRoutesConsolidated{

  import Context.keyValueStore
  import Context.executionContext

  import com.synergygb.zordon.core.util._

  implicit val context = Context

  protected def apiResourceClass = getClass

  override def dataContext: DataTransformContext = ???

  override def handleDeleteProjectProjectIdTaskTaskIdUserName(projectId: String, taskId: String, userName: String)(): Route = super.handleDeleteProjectProjectIdTaskTaskIdUserName(projectId, taskId, userName)()

  override def handlePostProjectProjectIdTaskTaskIdUserName(projectId: String, taskId: String, userName: String)(): Route = super.handlePostProjectProjectIdTaskTaskIdUserName(projectId, taskId, userName)()

  override def handlePutProjectProjectIdTaskTaskId(projectId: String, taskId: String, task: Task)(): Route = {

    onSuccess(keyValueStore.read[Project]("project-"+projectId)) { x =>

      x match {
        case Some(projectGot) =>

          val taskModified = projectGot.tasksAssociated.map(taskGot =>
            if (taskGot.id == taskId) {
              Task(
                id = Option(taskId),
                name = task.name,
                userAssigned = task.userAssigned,
                completed = task.completed
              )
            } else {
              taskGot
            }
          )

          val projectToStore = Project(
            id = projectGot.id,
            name = projectGot.name,
            leaderName = projectGot.leaderName,
            initDate = projectGot.initDate,
            finishDate = projectGot.finishDate,
            tasksAssociated = taskModified
          )

          onSuccess(keyValueStore.write("project-"+projectId, projectToStore)) { x =>
            complete(Created, projectToStore)
          }

        case None =>
          complete(NotFound, Error(
            code = "404",
            msg = "Not found"
          ))
      }
    }
  }

  override def handleDeleteProjectProjectIdTaskTaskId(projectId: String, taskId: String)(): Route = {

    onSuccess(keyValueStore.read[Project]("project-"+projectId)) { x =>

      x match {
        case Some(project) =>

          val filteredTasks = project.tasksAssociated.filter(_.id != taskId)

          val projectToStore = Project(
            id = project.id,
            name = project.name,
            leaderName = project.leaderName,
            initDate = project.initDate,
            finishDate = project.finishDate,
            tasksAssociated = filteredTasks
          )

          onSuccess(keyValueStore.delete("project-" + projectId)) { x =>
            complete(Accepted, project)
          }
        case None =>
          complete(NotFound, Error(
            code = "404",
            msg = "Not found"
          ))
      }
    }
  }

  override def handlePostProjectProjectIdTaskTaskId(projectId: String, taskId: String)(): Route = {

    onSuccess(keyValueStore.read[Project]("project-"+projectId)) { x =>
      x match {
        case Some(project) =>
          val filteredTasks = project.tasksAssociated.map(task =>
            if(task.id == taskId) {
              Task(
                id = Option(taskId),
                name = task.name,
                userAssigned = task.userAssigned,
                completed = Option(!task.completed.getOrElse(false))
              )
            } else {
              task
            }
          )

          val projectToStore = Project(
            id = project.id,
            name = project.name,
            leaderName = project.leaderName,
            initDate = project.initDate,
            finishDate = project.finishDate,
            tasksAssociated = filteredTasks
          )

          onSuccess(keyValueStore.write("project-"+project.id, projectToStore)) { x =>
            complete(Created, projectToStore)
          }
        case None =>
          complete(NotFound, Error(
            code = "404",
            msg = "Not found"
          ))
      }
    }
  }

  override def handlePostProjectProjectIdTask(projectId: String, task: Task)(): Route = {

    val taskToStore = Task(
      id = Option(Generator.generateShortRandom()),
      name = task.name,
      userAssigned = task.userAssigned,
      completed = Option(task.completed.getOrElse(false))
    )

    onSuccess(keyValueStore.read[Project]("project-"+projectId)) { x =>
      x match {
        case Some(project) =>
          val projectToStore = Project(
              id = project.id,
              name = project.name,
              leaderName = project.leaderName,
              initDate = project.initDate,
              finishDate = project.finishDate,
              tasksAssociated = project.tasksAssociated :+ taskToStore
            )
          onSuccess(keyValueStore.write("project-"+project.id, projectToStore)) { x =>
            complete(Created, projectToStore)
          }
        case None =>
          complete(NotFound, Error(
            code = "404",
            msg = "Not found"
          ))
      }
    }
  }

  override def handlePostProject(project: Project)(): Route = {

    val projectToStore = Project(
      id = Option(Generator.generateShortRandom()),
      name = project.name,
      leaderName = project.leaderName,
      initDate = project.initDate,
      finishDate = project.finishDate,
      tasksAssociated = project.tasksAssociated
    )
    onSuccess(keyValueStore.write("project-"+projectToStore.id, projectToStore)) { x =>
      complete(Created, projectToStore)
    }
  }

  override def handleDeleteProjectProjectId(projectId: String)(): Route = {

    onSuccess(keyValueStore.read[Project]("project-"+projectId)) { x =>

      x match {
        case Some(project) =>
          onSuccess(keyValueStore.delete("project-" + projectId)) { x =>
            complete(Accepted, project)
          }
        case None =>
          complete(NotFound, Error(
            code = "404",
            msg = "Not found"
          ))
      }
    }
  }

  override def handleGetProjectProjectId(projectId: String)(): Route = super.handleGetProjectProjectId(projectId)()

  override def handlePutProjectProjectId(projectId: String, project: Project)(): Route = {

    onSuccess(keyValueStore.read[Project]("project-"+projectId)) { x =>

      x match {
        case Some(projectGot) =>
          val projectToStore = Project(
            id = Option(projectId),
            name = project.name,
            leaderName = project.leaderName,
            initDate = project.initDate,
            finishDate = project.finishDate,
            tasksAssociated = project.tasksAssociated
          )

          onSuccess(keyValueStore.write("project-"+projectId, projectToStore)) { x =>
            complete(Created, projectToStore)
          }
        case None =>
          complete(NotFound, Error(
            code = "404",
            msg = "Not found"
          ))
      }
    }
  }

  override def handleGetProjectProjectIdUserName(projectId: String, userName: String)(): Route = {
    import spray.json.DefaultJsonProtocol._
    onSuccess(keyValueStore.read[Project]("project-"+projectId)) { x =>

      x match {
        case Some(projectGot) =>
          complete(Created, projectGot.tasksAssociated.filter(_.userAssigned.getOrElse("") == userName))
        case None =>
          complete(NotFound, Error(
            code = "404",
            msg = "Not found"
          ))
      }
    }
  }
}
