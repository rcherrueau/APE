//  ,---.    ____      __       ___          __   __
// ( @ @ )  / __ \____/ /____  / _ \___ ____/ /__/ /_ __
//  ).-.(  / /_/ / __/ __/ _ \/ // / _ `/ _  / _  / // /
// '/|||\` \____/\__/\__/\___/____/\_,_/\_,_/\_,_/\_, /
//   '|`                                         /___/
//
// Adapted Calculator for my Daddy's Needs

// Useful links
// https://reactjs.org/docs/dom-elements.html
open Belt

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Utils

/** Compute the sum of a list of floats */
let sum = xs => xs->Array.reduce(0., (a, b) => a +. b)

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

let entriesMock = [
  {quantity: 1., price: 1., isShipped: false},
  {quantity: 1., price: 1., isShipped: true},
  {quantity: 2., price: 1., isShipped: false},
  {quantity: 3., price: 1., isShipped: false},
  {quantity: 4., price: 1., isShipped: false},
  {quantity: 5., price: 1., isShipped: true},
]

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
let totalQuantity = (entries: array<entry>): float => {
  entries->Array.map(e => e.quantity)->sum
}

/** Compute the mean price for all entries */
let priceMean = (entries: array<entry>, shipFactor: float): float => {
  let cPriceF = computePrice(shipFactor)
  let prices = entries->Array.map(cPriceF)
  let totalQ = entries->totalQuantity

  prices->sum /. totalQ
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// View system

/** Regexp pattern to recognize a float */
let floatPattern = "-?[0-9]*\.?[0-9]*"

/** Format a float to 2 digits after the comma */
let fmtFloat = (f) => Js.Float.toFixedWithPrecision(f, ~digits=2)

/** Set the value of an input text */
let updateInputTxt = (setter, evt) => {
  ReactEvent.Form.preventDefault(evt)
  let newValue = ReactEvent.Form.target(evt)["value"]
  Js.log(Float.fromString(newValue))

  setter(_oldValue => newValue)
}

/** Header
- View results for entries computations
- Text field for a new entry
*/
module ViewHeader = {
  let prependEntry = (quantity: string, price: string, isShipped: bool, entries: array<entry>): array<entry> => {
    let newEntry = {quantity: Js.Float.fromString(quantity),
                    price: Js.Float.fromString(price),
                    isShipped: isShipped}

    Js.log(newEntry)
    Array.concat([newEntry], entries)
  }

  @react.component
  let make = (~entries: array<entry>,
              ~setEntries: (array<entry> => array<entry>) => unit,
              ~coefMin: string,
              ~coefMax: string, ~shipFactor: string) => {

    // Compute results
    let totalQuantity = entries->totalQuantity
    let priceMean = entries->priceMean(Js.Float.fromString(shipFactor))
    let priceMeanMin = priceMean /. Js.Float.fromString(coefMin)
    let priceMeanMax = priceMean /. Js.Float.fromString(coefMax)

    // States for a new entry
    let (quantity, setQuantity) = React.useState(_ => "")
    let (price, setPrice) = React.useState(_ => "")
    let (isShipped, setIsShipped) = React.useState(_ => true)

    <header>
      <div id="results">
        // Headline (first line)
        <h3>{React.string(j`Total Qté`)}</h3>
        <h3>{React.string("Prix Moy")}</h3>
        <h3>{React.string("Moy Min")}</h3>
        <h3>{React.string("Moy Max")}</h3>

        // Computed values (second line)
        <p id="total-quantity">{React.string(totalQuantity->fmtFloat)}</p>
        <p id="price-mean">{React.string(priceMean->fmtFloat)}</p>
        <p id="price-mean-min">{React.string(priceMeanMin->fmtFloat)}</p>
        <p id="price-mean-max">{React.string(priceMeanMax->fmtFloat)}</p>
      </div>

      // User inputs for a new entry (third line)
      <div id="user-inputs">
        <span>
          <label className="for-input-text" htmlFor="new-entry:quantity"><p>{React.string(j`Qté ..`)}</p>
            <input id="new-entry:quantity" type_="text" pattern=floatPattern
                   value={quantity} onChange={updateInputTxt(setQuantity)} />
          </label>
        </span>
        <span>
          <label className="for-input-text" htmlFor="new-entry:price"><p>{React.string("Prix ...")}</p>
            <input id="new-entry:price" type_="text" pattern=floatPattern
                   value={price} onChange={updateInputTxt(setPrice)} />
          </label>
        </span>
        <span>
          <label className="for-checkbox" htmlFor="new-entry:shipping">
             <input id="new-entry:shipping" type_="checkbox"
                checked={isShipped}
                onChange={evt => setIsShipped(_ => ReactEvent.Form.target(evt)["checked"])} />
             <i className="fas fa-truck" />
           </label>
        </span>
        <span>
          <button onClick={_evt => {
            // Add the new entry to the list of entries
            setEntries(prependEntry(quantity, price, isShipped))

            // Reset textfield
            setQuantity(_ => "")
            setPrice(_ => "")
            setIsShipped(_ => true)
          }}>
          <i className="fas fa-plus" />
          </button>
        </span>
      </div>
    </header>
  }
}

/** Footer: Text field to set default values */
module ViewFooter = {
  @react.component
  let make = (~coefMin: string,
              ~setCoefMin: (string => string) => unit,
              ~coefMax: string,
              ~setCoefMax: (string => string) => unit,
              ~shipFactor: string,
              ~setShipFactor: (string => string) => unit) => {

    <footer>
      // Coef Min user input
      <span>
        <label className="for-input-text"><p>{React.string("Coef min ...")}</p>
        <input id="coefMin" type_="text" pattern=floatPattern value={coefMin}
               onChange={{updateInputTxt(setCoefMin)}} />
        </label>
      </span>

      // Coef Max user input
      <span>
        <label className="for-input-text"><p>{React.string("Coef max ...")}</p>
        <input id="coefMax" type_="text" pattern=floatPattern value={coefMax}
                onChange={{updateInputTxt(setCoefMax)}} />
        </label>
      </span>

      // Shipping factor user input
      <span>
        <label className="for-input-text" htmlFor="shipFactor">
          <p><i className="fas fa-truck"/>{React.string("(%)")}</p>
          <input id="shipFactor" type_="text" pattern=floatPattern value={shipFactor}
                onChange={{updateInputTxt(setShipFactor)}} />
        </label>
      </span>

      // Refresh button
      <span>
        <button type_="reset" name="reset"><i className="fas fa-redo"/></button>
      </span>
    </footer>
  }
}

module ViewMain = {
  @react.component
  let make = (~entries: array<entry>,
              ~setEntries: (array<entry> => array<entry>) => unit) => {

    /** Toggle shipping state of an entry at a specific index */
    let toggleEntryShipping = (idx: int) => {
      Js.log("Toggle shipping of row #" ++ Int.toString(idx))

      setEntries(oldEntries =>
        oldEntries->Array.mapWithIndex((idx', e) =>
          idx == idx' ? {...e, isShipped: !e.isShipped}: e))
    }

    /** Delete one entry at a specific index */
    let deleteEntry = (idx: int) => {
      Js.log("Delete row #" ++ Int.toString(idx))

      setEntries(oldEntries =>
        oldEntries->Array.keepWithIndex((_, idx') => idx != idx'))
    }

   /** Render one entry has html and set update and delete actions */
   let renderEntry = (idx: int, e: entry) => {
     <div key={Int.toString(idx)}>
       // Quantity
       <span>{React.string(e.quantity->fmtFloat)}</span>

       // Price
       <span>{React.string(e.price->fmtFloat)}</span>

       // Is shipped?
       <span>
         <label className="for-checkbox" >
         <input type_="checkbox" checked={e.isShipped} onChange={_ => toggleEntryShipping(idx)} />
         <i className="fas fa-truck" />
         </label>
       </span>

       // Delete
       <span>
         <button onClick={_ => deleteEntry(idx)}>
         <i className="far fa-trash-alt"/>
         </button>
       </span>
     </div>
   }

    // Render entries
    <main> {React.array(entries->Array.mapWithIndex(renderEntry))} </main>
  }
}


module App = {

  @react.component
  let make = () => {
    // States
    let (coefMin, setCoefMin) = React.useState(_ => defaultCoefMin->fmtFloat)
    let (coefMax, setCoefMax) = React.useState(_ => defaultCoefMax->fmtFloat)
    let (shipFactor, setShipFactor) = React.useState(_ => defaultShipFactor->fmtFloat)
    let (entries, setEntries) = React.useState(_ => entriesMock)

    // Application
    React.array([
      <ViewHeader key="header" entries setEntries coefMin coefMax shipFactor />,
      <ViewMain   key="main"   entries setEntries />,
      <ViewFooter key="footer" coefMin setCoefMin
                               coefMax setCoefMax
                               shipFactor setShipFactor />
    ])
  }

}

switch (ReactDOM.querySelector("#root")) {
| Some(root) => ReactDOM.render(<App/>, root)
| None => ()
}
