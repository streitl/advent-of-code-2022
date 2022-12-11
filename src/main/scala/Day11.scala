import scala.collection.mutable.ListBuffer

case class Monkey(items: ListBuffer[BigInt], operation: BigInt => BigInt, divisor: Int, target: BigInt => Int)

def parseMonkeys(s: String): Array[Monkey] =
  s.split("\n\n").map(g =>
    val lines: Array[String] = g.split('\n')
    val items: ListBuffer[BigInt] = ListBuffer(lines(1).split("Starting items: ")(1).split(", ").map(_.toInt)*)
    val ops: Array[String] = lines(2).split("Operation: new = old ")(1).split(' ')
    val operation: BigInt => BigInt = ops match
      case Array("+", "old") => old => old + old
      case Array("*", "old") => old => old * old
      case Array("+", op) => old => old + op.toInt
      case Array("*", op) => old => old * op.toInt
    val divisor: Int = lines(3).split("divisible by ")(1).toInt
    val monkeyTrue: Int = lines(4).split("monkey ")(1).toInt
    val monkeyFalse: Int = lines(5).split("monkey ")(1).toInt
    Monkey(items, operation, divisor, (i: BigInt) => if (i % divisor == 0) monkeyTrue else monkeyFalse)
  )

def monkeyBusiness(monkeys: Array[Monkey], simSteps: Int, relieved: Boolean): BigInt =
  val counts: ListBuffer[BigInt] = ListBuffer.tabulate(monkeys.length)(_ => 0)
  val gcd: Int = monkeys.map(_.divisor).product
  (1 to simSteps).foreach(_ =>
    monkeys.zipWithIndex.foreach((monkey, i) =>
      monkey.items.foreach(worry =>
        var newWorry: BigInt = monkey.operation(worry % gcd) % gcd
        if (relieved)
          newWorry = newWorry / 3
        counts(i) += 1
        monkeys(monkey.target(newWorry)).items += newWorry
      )
      monkey.items.dropInPlace(monkey.items.length)
    )
  )
  counts.toList.sorted.reverse.take(2).product

@main
def day11(): Unit =
  val input: String = "Monkey 0:\n  Starting items: 52, 60, 85, 69, 75, 75\n  Operation: new = old * 17\n  Test: divisible by 13\n    If true: throw to monkey 6\n    If false: throw to monkey 7\n\nMonkey 1:\n  Starting items: 96, 82, 61, 99, 82, 84, 85\n  Operation: new = old + 8\n  Test: divisible by 7\n    If true: throw to monkey 0\n    If false: throw to monkey 7\n\nMonkey 2:\n  Starting items: 95, 79\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 5\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 88, 50, 82, 65, 77\n  Operation: new = old * 19\n  Test: divisible by 2\n    If true: throw to monkey 4\n    If false: throw to monkey 1\n\nMonkey 4:\n  Starting items: 66, 90, 59, 90, 87, 63, 53, 88\n  Operation: new = old + 7\n  Test: divisible by 5\n    If true: throw to monkey 1\n    If false: throw to monkey 0\n\nMonkey 5:\n  Starting items: 92, 75, 62\n  Operation: new = old * old\n  Test: divisible by 3\n    If true: throw to monkey 3\n    If false: throw to monkey 4\n\nMonkey 6:\n  Starting items: 94, 86, 76, 67\n  Operation: new = old + 1\n  Test: divisible by 11\n    If true: throw to monkey 5\n    If false: throw to monkey 2\n\nMonkey 7:\n  Starting items: 57\n  Operation: new = old + 2\n  Test: divisible by 17\n    If true: throw to monkey 6\n    If false: throw to monkey 2"
  // val example: String = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"
  // println(s"Part 1: ${monkeyBusiness(parseMonkeys(example), 20, true)}")
  println(s"Part 1: ${monkeyBusiness(parseMonkeys(input), 20, true)}")
  // println(s"Part 2: ${monkeyBusiness(parseMonkeys(example), 10000, false)}")
  println(s"Part 2: ${monkeyBusiness(parseMonkeys(input), 10000, false)}")