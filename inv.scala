case class Product(name: String, quantity: Int, price: Double)

object InventoryManager {
  def main(args: Array[String]): Unit = {
    val inventory1: Map[Int, Product] = Map(
      101 -> Product("Apple", 50, 0.5),
      102 -> Product("Banana", 30, 0.2),
      103 -> Product("Orange", 20, 0.3)
    )

    val inventory2: Map[Int, Product] = Map(
      102 -> Product("Banana", 40, 0.25),
      104 -> Product("Grapes", 15, 0.4)
    )

    val productNames = inventory1.values.map(_.name).mkString(", ")
    println(s"Product Names: $productNames")

    val totalValue = inventory1.values.map(p => p.quantity * p.price).sum
    println(s"Total Value: $$${totalValue}")

    val isEmpty = inventory1.isEmpty
    println(s"Is Inventory1 Empty? $isEmpty")

    val mergedInventory = (inventory1.toSeq ++ inventory2.toSeq)
      .groupBy(_._1)
      .map { case (id, products) =>
        val mergedProduct = products.map(_._2).reduce { (p1, p2) =>
          Product(p1.name, p1.quantity + p2.quantity, Math.max(p1.price, p2.price))
        }
        id -> mergedProduct
      }
    println(s"Merged Inventory: $mergedInventory")

    val productId = 102
    val productDetails = inventory1.get(productId)
    productDetails match {
      case Some(product) => println(s"Product ID: $productId, Name: ${product.name}, Quantity: ${product.quantity}, Price: ${product.price}")
      case None => println(s"Product ID: $productId not found")
    }
  }
}
