import scala.io.StdIn._

object StudentRecordManager {

  // Function to calculate percentage and grade
  def getGrade(percentage: Double): Char = {
    percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }
  }

  // Function to get student info
  def getStudentInfo: (String, Int, Int, Double, Char) = {
    val (name, marks, totalMarks) = readStudentInfo()
    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = getGrade(percentage)
    (name, marks, totalMarks, percentage, grade)
  }

  // Function to print student record
  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Name: $name")
    println(s"Marks: $marks")
    println(s"Total Marks: $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
  }

  // Function to validate input
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

  // Function to read student info from the keyboard
  private def readStudentInfo(): (String, Int, Int) = {
    println("Enter student's name:")
    val name = readLine().trim
    println("Enter marks obtained:")
    val marks = readInt()
    println("Enter total possible marks:")
    val totalMarks = readInt()
    (name, marks, totalMarks)
  }

  // Function to get student info with retry for invalid input
  def getStudentInfoWithRetry: (String, Int, Int, Double, Char) = {
    var valid = false
    var studentInfo: (String, Int, Int, Double, Char) = (null, 0, 0, 0.0, 'D')

    while (!valid) {
      val (name, marks, totalMarks) = readStudentInfo()
      val (isValid, errorMessage) = validateInput(name, marks, totalMarks)
      
      if (isValid) {
        studentInfo = getStudentInfo
        valid = true
      } else {
        errorMessage.foreach(println)
      }
    }

    studentInfo
  }

  // Main method to demonstrate the functionality
  def main(args: Array[String]): Unit = {
    val studentRecord = getStudentInfoWithRetry
    printStudentRecord(studentRecord)
  }
}
