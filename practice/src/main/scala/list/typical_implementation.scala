package list

abstract class Employee(
  firstName: String,
  lastName: String,
  department: String,
  salary: Double
)

case class Engineer(
  firstName: String,
  lastName: String,
  department: String,
  salary: Double,
  group: String
) extends Employee(firstName, lastName, department, salary)

case class Doctor(
  firstName: String,
  lastName: String,
  department: String,
  salary: Double,
  group: String
) extends Employee(firstName, lastName, department, salary)


object TypicalImplementation {
  def main(args: Array[String]): Unit = {
    val eng1 = Engineer(
      "Isaac",
      "Newton",
      "IT",
      4500.50,
      "Engineering"
    )

    val eng2 = Engineer(
      "Albert",
      "Einstein",
      "Infra",
      4600.50,
      "Engineering"
    )

    val doc1 = Doctor(
      "Michael",
      "Young",
      "Cardio",
      5000.5,
      "Medicine"
    )

    val doc2 = Doctor(
      "Jeffrey",
      "Hall",
      "Pathology",
      5100.5,
      "Medicine"
    )

    val engineers = List(eng1, eng2)
    val doctors = List(doc1, doc2)

    val employees = engineers ::: doctors
    val emp2 = List(engineers, doctors)

    val emp2Size = emp2.flatten
    println(f"emp2Size: $emp2Size")

    val engineersSize = engineers.size
    println(f"engineersSize: $engineersSize")

    val doctorsSize = doctors.size
    println(f"doctorsSize: $doctorsSize")

    val employeesSize = employees.size
    println(f"employeesSize: $employeesSize")

    val employeesExistsEng1 = employees.exists(x => x == eng1)
    println(f"employeesExistsEng1: $employeesExistsEng1")

    val employeesExistsEngineers = employees.exists(x => x == engineers)
    println(f"employeesExistsEngineers: $employeesExistsEngineers")

    val emp2ExistsEngineers = emp2.exists(x => x == engineers)
    println(f"emp2ExistsEngineers: $emp2ExistsEngineers")

  }
}
