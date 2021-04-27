//  ,---.    ____      __       ___          __   __
// ( @ @ )  / __ \____/ /____  / _ \___ ____/ /__/ /_ __
//  ).-.(  / /_/ / __/ __/ _ \/ // / _ `/ _  / _  / // /
// '/|||\` \____/\__/\__/\___/____/\_,_/\_,_/\_,_/\_, /
//   '|`                                         /___/
//
// Adapted Calculator for my Daddy's Needs
open Belt

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Utils

/** Compute the sum of a list of floats */
let sum = xs => xs->List.reduce(0., (a, b) => a +. b)

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Model and default values

type entry = {
  quantity: float,
  price: float,
  isShipped: bool,
}

let defaultCoefMin: float = 0.8
let defaultCoefMax: float = 0.7
let defaultShipFactor: float = 3.
let defauntNewEntry: entry = {
  quantity: infinity,
  price: infinity,
  isShipped: true,
}

let entriesMock = list{
  {quantity: 1., price: 1., isShipped: false},
  {quantity: 1., price: 1., isShipped: true},
  {quantity: 2., price: 1., isShipped: false},
  {quantity: 3., price: 1., isShipped: false},
  {quantity: 4., price: 1., isShipped: false},
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Application Logic

/** Compute the price of an entry */
let computePrice = (shipFactor: float, {quantity, price, isShipped}: entry): float => {
  let computedPrice = quantity *. price
  let factorPrice =
    computedPrice *. if isShipped {
      shipFactor /. 100.
    } else {
      0.
    }

  computedPrice +. factorPrice
}

/** Compute the total quantity between entries  */
let totalQuantity = (entries: list<entry>): float => {
  entries->List.map(e => e.quantity)->sum
}

/** Compute the mean price for all entries */
let priceMean = (entries: list<entry>, shipFactor: float): float => {
  let cPriceF = computePrice(shipFactor)
  let prices = entries->List.map(cPriceF)
  let totalQ = entries->totalQuantity

  prices->sum /. totalQ
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// View system

/** Regexp pattern to recognize a float */
let floatPattern = "-?[0-9]*\.?[0-9]*"

/** Format a float to 3 digits after the comma */
let fmtFloat = (f) => Js.Float.toFixedWithPrecision(f, ~digits=3)

/** View results of the computations */
module ViewHeader = {

  /* https://reactjs.org/docs/dom-elements.html */
  @react.component
  let make = (~entries) => {
    let totalQuantity = entries->totalQuantity
    let priceMean = entries->priceMean(defaultShipFactor)
    let priceMeanMin = priceMean /. defaultCoefMin
    let priceMeanMax = priceMean /. defaultCoefMax

    <header>
      <div id="header-top">
        // first line
        <h3>{React.string(j`Total Qté`)}</h3>
        <h3>{React.string("Prix Moy")}</h3>
        <h3>{React.string("Moy Min")}</h3>
        <h3>{React.string("Moy Max")}</h3>

        // second line
        <p id="total-quantity">{React.string(totalQuantity->fmtFloat)}</p>
        <p id="price-mean">{React.string(priceMean->fmtFloat)}</p>
        <p id="price-mean-min">{React.string(priceMeanMin->fmtFloat)}</p>
        <p id="price-mean-max">{React.string(priceMeanMax->fmtFloat)}</p>
      </div>

      // third line
      <div id="header-inputs">
        <label htmlFor="new-entry:quantity"><p>{React.string(j`Qté ..`)}</p>
          <input id="new-entry:quantity" type_="text" pattern=floatPattern/>
        </label>
        <label htmlFor="new-entry:price"><p>{React.string("Prix ...")}</p>
            <input id="new-entry:price" type_="text" pattern=floatPattern/>
        </label>
        <div>
          <input id="new-entry:shipping" type_="checkbox" defaultChecked={true} />
          <label htmlFor="new-entry:shipping" className="truck-icon"><i className="fas fa-truck" /></label>
        </div>
        <button><i className="fas fa-plus" /></button>
      </div>
    </header>
  }
}

module ViewFooter = {
  @react.component
    let make = (~coefMin: float, ~coefMax: float, ~shipFactor: float) => {
      <footer>
        <label>
            <p>{React.string("Coef min ...")}</p>
            <input id="coefMin" type_="text" pattern=floatPattern defaultValue={coefMin->fmtFloat} />
        </label>
        <label htmlFor="coefMax">
            <p>{React.string("Coef max ...")}</p>
            <input id="coefMax" type_="text" pattern=floatPattern defaultValue={coefMax->fmtFloat} />
        </label>
        <label htmlFor="shipFactor">
            <p> <i className="fas fa-truck" />{React.string("(%)")} </p>
            <input id="shipFactor" type_="text" pattern=floatPattern defaultValue={shipFactor->fmtFloat} />
        </label>
        <button type_="reset"><i className="fas fa-redo"/></button>
      </footer>
    }
}

module ViewMain = {
  @react.component
  let make = () => {
    <main>
      <table>
        <tbody>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
          <tr> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> <td>{React.string("H1")}</td> </tr>
        </tbody>
      </table>
    </main>
  }
}

let make = (childs: React.element) => {
  <div className="foo">  childs </div>
}

let app = React.array([
  <ViewHeader entries={entriesMock}/>,
  <ViewMain/>,
  <ViewFooter coefMin={defaultCoefMin}
              coefMax={defaultCoefMax}
              shipFactor={defaultShipFactor} />
])

switch (ReactDOM.querySelector("#root")) {
| Some(root) => ReactDOM.render(app, root)
| None => ()
}
