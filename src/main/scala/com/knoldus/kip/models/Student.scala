package com.knoldus.kip.models

case class Student(id: Int, firstName: String, middleName: Option[String],
                   lastName: String, rollNumber: Int, age: Option[Int], gender: Char, enrollmentNumber: Long,
                   address: Option[String]) extends ModelIdentifier
{
  def getAddress:String = address.fold("N/A"){_.toString}

  def getMiddleName:String = middleName.map(_.split(" ")(0)).getOrElse("")
}

case class Subject(id: Int, name: String, obtainedMarks: Float, maxMarks: Float) extends ModelIdentifier


case class Course(id: Int, name: String, category: String, subjects: List[Subject]) extends ModelIdentifier


case class Scorecard(id: Int, student: Student, subjects: List[Subject], total: Float, percentage: Float, grade: String) extends ModelIdentifier {

  def getSubjectsWithMaxMarks: List[Subject] = {
    val max = subjects.map(_.obtainedMarks).max
    subjects.filter(_.obtainedMarks == max)
  }

  def getSubjectsWithMinMarks: List[Subject] = {
    val min = subjects.map(_.obtainedMarks).min
    subjects.filter(_.obtainedMarks == min)
  }

}

object Scorecard {
  def apply(student: Student, subjects: List[Subject]): Scorecard = {

    val total = subjects.map(_.obtainedMarks).sum
    val percentage = total / subjects.map(_.maxMarks).sum

    val grade = if (percentage >= 95) {
      "A+"
    }
    else if (percentage >= 90) {
      "A"
    }
    else if (percentage >= 85) {
      "B+"
    }
    else if (percentage >= 80) {
      "B"
    }
    else if (percentage >= 70) {
      "C+"
    }
    else if (percentage >= 60) {
      "C"
    }
    else if (percentage >= 50) {
      "D+"
    }
    else if (percentage >= 40) {
      "D"
    }
    else {
      "F"
    }

    new Scorecard(student.id, student, subjects, total, percentage, grade)
  }

  def checkScoreboard(scoreboard: List[Scorecard]): List[String] = {

    for {scorecard <- scoreboard
         subject <- scorecard.subjects
    }
      yield {

        scorecard.student.firstName + " " + subject.name + " " + subject.obtainedMarks
      }



  }
}

case class CoursePerformance(id: Int, year: Int, course: Course, scorecards: List[Scorecard]) extends ModelIdentifier