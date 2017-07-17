package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.{CoursePerformance, Scorecard}

trait Postman {


  def getTheFirstAddressOfFirstYearPerformance(id: Int) : String= {

    val coursePerformance: Option[CoursePerformance] = RamDatabase.getById(id)



    val scorecard:Option[Scorecard] = coursePerformance.flatMap(_.scorecards.headOption)

    val address:String = scorecard.map(_.student.getAddress).getOrElse("N/A")

    address
  }
}
