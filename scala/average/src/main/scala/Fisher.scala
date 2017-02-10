package fisher

object Logic {
  type Amount = Double
  type Price  = Double
  type Travel = Boolean

  /** Sum of entries amount. */
  def amountSum(entries: List[(Amount, Price, Travel)]): Amount =
    entries.map(_._1).sum

  /** Average of entries.
    *
    * Average is the `amount` times `price`, plus a percentage
    * calculated from the `travelCost`. The percentage is calculated
    * only if the `travel` option is `True`.
    */
  def average(entries: List[(Amount, Price, Travel)],
              travelCost: Double): Price = {
    def entryPrice(entry: (Amount, Price, Travel)): Price = {
      val (amount, price, travel) = entry
      val eprice = amount * price
      val cprice = if (travel) eprice * travelCost * .001 else .0

      eprice + cprice
    }

    entries.map(entryPrice).sum / entries.length
  }

  /** Average valued by a factor. */
  def averageValued(entries: List[(Amount, Price, Travel)],
                    travelCost: Double,
                    factor: Double): Price =
    average(entries,travelCost) / factor
}

object Prop {
  import com.typesafe.config._
  import scala.util.Properties

  val jarConfig = ConfigFactory.load()
  val homeConfig = ConfigFactory.load(Properties.userHome + "/.fisher.conf")
  val config = homeConfig.withFallback(jarConfig)

  // Graphic properties
  val windowWidth: Double = config.getDouble("windowWidth")
  val windowHeight: Double = config.getDouble("windowHeight")
  val fontFace: String = config.getString("fontFace")
  val fontSize: Double = config.getDouble("fontSize")

  // Application properties
  val factorMax: Double = config.getDouble("factorMax")
  val factorMin: Double = config.getDouble("factorMin")
  val travelCost: Double = config.getDouble("travelCost")
}

import javafx.application.Application
import javafx.beans.property._
import javafx.geometry.Insets
import javafx.scene.Group
import javafx.scene.Scene
import javafx.scene.control.Label
import javafx.scene.control.TableColumn
import javafx.scene.control.TableView
import javafx.scene.control.cell.PropertyValueFactory
import javafx.scene.control.cell.CheckBoxTableCell
import javafx.scene.layout.VBox
import javafx.scene.text.Font
import javafx.stage.Stage


import javafx.collections.FXCollections
import javafx.collections.ObservableList

class View extends Application {
  import Logic._

  val $label = new Label("Fisher")
  val $table = new TableView[Entry]()

  val entries: ObservableList[Entry] = FXCollections.observableArrayList(
    new Entry(31.5, 100, true),
    new Entry(31.5, 100, true),
    new Entry(31.5, 100, false))

  class Entry(_amount: Amount, _price: Price, _travel: Travel) {
    val amount: SimpleDoubleProperty = new SimpleDoubleProperty(_amount)
    val price: SimpleDoubleProperty = new SimpleDoubleProperty(_price)
    val travel: SimpleBooleanProperty = new SimpleBooleanProperty(_travel)
  }

  override def start(stage: Stage): Unit = {
    val scene = new Scene(new Group())
    stage.setTitle("Table View Sample")
    stage.setWidth(Prop.windowWidth)
    stage.setHeight(Prop.windowHeight)

    $label.setFont(new Font(Prop.fontFace, 20))

    $table.setEditable(true)

    val $amountCol = new TableColumn[Entry,Number]("Qté")
    $amountCol.setMinWidth(100)
    $amountCol.setCellValueFactory(_.getValue().amount)

    val $priceCol = new TableColumn[Entry,Number]("Prix")
    $priceCol.setMinWidth(100)
    $priceCol.setCellValueFactory(_.getValue().price)

    val $travelCol = new TableColumn[Entry,java.lang.Boolean]("Transport")
    $travelCol.setMinWidth(100)
    $travelCol.setCellValueFactory(_.getValue().travel)
    $travelCol.setCellFactory(tc => new CheckBoxTableCell())

    $table.setItems(entries)
    $table.getColumns().addAll($amountCol, $priceCol, $travelCol)

    val vbox = new VBox()
    vbox.setSpacing(5)
    vbox.setPadding(new Insets(10, 0, 0, 10))
    vbox.getChildren().addAll($label, $table)

    scene.getRoot().asInstanceOf[Group].getChildren().addAll(vbox)

    stage.setScene(scene)
    stage.show()
  }
}

object View {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[View], args: _*)
  }

  // }
  // import scalafx.geometry.Insets
  // import scalafx.collections.ObservableBuffer
  // import scalafx.beans.value.ObservableValue
  // import scalafx.beans.property._
  // import scalafx.scene.Scene
  // import scalafx.scene.control._
  // import scalafx.scene.control.TableColumn._
  // import scalafx.scene.control.cell._
  // import scalafx.scene.control.Label
  // import scalafx.scene.layout.BorderPane

  // // class Entry(_amount: Amount, _price: Price, _travel: Travel) {
  // //   val amount = DoubleProperty(_amount)
  // //   val price = DoubleProperty(_price)
  // //   val travel = _travel
  // // }
  // type Entry = (Amount, Price, Travel)


  // val entries = ObservableBuffer[Entry]()

  // // val $cAmount = new TableColumn[Entry, Amount] {
  // //   text = "Qté"
  // //   cellValueFactory = { _.value.amount }
  // //   cellFactory = { _ =>
  // //     new TableCell[Entry, Amount] {
  // //       null
  // //     //   item.onChange { (_, _, newColor) =>
  // //     //     graphic =
  // //     //       if (newColor != null)
  // //     //         new Circle {
  // //     //           fill = newColor
  // //     //           radius = 8
  // //     //         }
  // //     //         else
  // //     //           null
  // //     //   }
  // //     // }
  // //   }
  // // }

  // val $cTravel = new TableColumn[Entry, Boolean] {
  //   text = "Transport"
  //   cellValueFactory = { (cdf: CellDataFeatures[Entry, Boolean]) => {
  //     val v: scalafx.beans.value.ObservableValue[Boolean,Boolean] =
  //       new BooleanProperty(this, "travel", cdf.value._3)

  //     v
  //   }
  //   }
  //   cellFactory = { _ =>
  //     new CheckBoxTableCell[Entry, Boolean]()
  //   }
  // }



  // // val $entries = new TableView[Entry](entries) {
  // //   columns += $cAmount  //   // columns ++= List(
  // // //     new TableColumn[Entry, Amount] {
  // // //       text = "Qté"
  // // //       cellValueFactory = { _.value.amount }
  // // //     }
  // // //     //   ,
  // // //     // new TableColumn[Entry, Price] {
  // // //     //   text = "Prix"
  // // //     //   cellValueFactory = { _.value._2 }
  // // //     // }
  // // //     //   ,
  // // //     // new TableColumn[Entry, Travel] {
  // // //     //   text = "Transport"
  // // //     //   cellValueFactory = { _.value._3 }
  // // //     // }
  // // //   // )
  // // }

  // stage = new JFXApp.PrimaryStage {
  //   title = "Dady's Fisher"
  //   scene = new Scene {
  //     root = new BorderPane {
  //       padding = Insets(25)
  //       center = new Label("Hello World")
  //     }
  //   }
  // }
}
