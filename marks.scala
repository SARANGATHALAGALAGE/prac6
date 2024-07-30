import scala.io.StdIn._

object StudentRecordManager {

  def getGrade(percentage: Double): Char = {
    percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }
  }

  def getStudentInfo(name: String, marks: Int, totalMarks: Int): (String, Int, Int, Double, Char) = {
    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = getGrade(percentage)
    (name, marks, totalMarks, percentage, grade)
  }

  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Name: $name")
    println(s"Marks: $marks")
    println(s"Total Marks: $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
  }

  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (marks < 0 || marks > totalMarks) {
      (false, Some("Marks must be between 0 and total possible marks."))
    } else if (totalMarks <= 0) {
      (false, Some("Total possible marks must be a positive integer."))
    } else {
      (true, None)
    }
  }

  private def readStudentInfo(): (String, Int, Int) = {
    println("Enter student's name (or type 'exit' to quit):")
    val name = readLine().trim
    if (name.toLowerCase == "exit") {
      return (name, 0, 0)
    }
    println("Enter marks obtained:")
    val marks = readInt()
    println("Enter total possible marks:")
    val totalMarks = readInt()
    (name, marks, totalMarks)
  }

  def getStudentInfoWithRetry: Option[(String, Int, Int, Double, Char)] = {
    var valid = false
    var studentInfo: Option[(String, Int, Int, Double, Char)] = None

    while (!valid) {
      val (name, marks, totalMarks) = readStudentInfo()
      if (name.toLowerCase == "exit") {
        return None
      }
      val (isValid, errorMessage) = validateInput(name, marks, totalMarks)
      
      if (isValid) {
        val percentage = (marks.toDouble / totalMarks) * 100
        val grade = getGrade(percentage)
        studentInfo = Some((name, marks, totalMarks, percentage, grade))
        valid = true
      } else {
        errorMessage.foreach(println)
      }
    }

    studentInfo
  }

  def main(args: Array[String]): Unit = {
    var studentRecords: List[(String, Int, Int, Double, Char)] = List()

    while (true) {
      getStudentInfoWithRetry match {
        case Some(studentRecord) => studentRecords = studentRecords :+ studentRecord
        case None => 
          println("Exiting input...")
          printAllStudentRecords(studentRecords)
          sys.exit(0)
      }
    }
  }

  def printAllStudentRecords(records: List[(String, Int, Int, Double, Char)]): Unit = {
    println("Student Records:")
    records.foreach(printStudentRecord)
  }
}
