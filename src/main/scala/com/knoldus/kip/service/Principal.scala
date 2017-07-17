package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models._

trait Principal {


  def findOutIfCSE(id: Int): CoursePerformance = {

    val coursePerformance: Option[CoursePerformance] = RamDatabase.getById(id)

    val course: Option[String] = coursePerformance.map(_.course.name)

    course match {

      case Some("CSE") => coursePerformance.get

      case None => throw new Exception("no CSE course with this id")

    }
  }

  def findOutIfAnyCourse(id: Int, courseName: String): CoursePerformance = {

    val coursePerformance: Option[CoursePerformance] = RamDatabase.getById(id)

    val course: Option[String] = coursePerformance.map(_.course.name)

    course match {

      case Some(`courseName`) => coursePerformance.get
      case Some(a) => throw new Exception(s"$courseName not associated with id = $id")
      case None => throw new Exception(s"there is no course linked to id = $id")
    }
  }

  def expression(mod: Any): String = mod match {

    case Student(_, _, _, _, _, _, _, _, _) => "Shut up "
    case Scorecard(_, _, _, _, _, _) => "Hmmm .... "
    case Subject(_, _, _, _) => "aha "
    case _ => "!!! ???"
  }

  //def checkScoreboard(scoreboard: Scoreboard): List[String] = ???


  def expressionRevisited: PartialFunction[ModelIdentifier, String] = {

    case x:Student => "Shut up "
    case x:Scorecard => "Hmmm .... "
    case Subject(_, _, _, _) => "aha "
    case _ => "!!! ???"

  }


}