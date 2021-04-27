// Generated by ReScript, PLEASE EDIT WITH CARE

import * as React from "react";
import * as Belt_List from "bs-platform/lib/es6/belt_List.js";
import * as ReactDom from "react-dom";
import * as Pervasives from "bs-platform/lib/es6/pervasives.js";

function sum(xs) {
  return Belt_List.reduce(xs, 0, (function (a, b) {
                return a + b;
              }));
}

var defauntNewEntry = {
  quantity: Pervasives.infinity,
  price: Pervasives.infinity,
  isShipped: true
};

var entriesMock = {
  hd: {
    quantity: 1,
    price: 1,
    isShipped: false
  },
  tl: {
    hd: {
      quantity: 1,
      price: 1,
      isShipped: true
    },
    tl: {
      hd: {
        quantity: 2,
        price: 1,
        isShipped: false
      },
      tl: {
        hd: {
          quantity: 3,
          price: 1,
          isShipped: false
        },
        tl: {
          hd: {
            quantity: 4,
            price: 1,
            isShipped: false
          },
          tl: /* [] */0
        }
      }
    }
  }
};

function computePrice(shipFactor, param) {
  var computedPrice = param.quantity * param.price;
  var factorPrice = computedPrice * (
    param.isShipped ? shipFactor / 100 : 0
  );
  return computedPrice + factorPrice;
}

function totalQuantity(entries) {
  return sum(Belt_List.map(entries, (function (e) {
                    return e.quantity;
                  })));
}

function priceMean(entries, shipFactor) {
  var cPriceF = function (param) {
    return computePrice(shipFactor, param);
  };
  var prices = Belt_List.map(entries, cPriceF);
  var totalQ = totalQuantity(entries);
  return sum(prices) / totalQ;
}

var floatPattern = "-?[0-9]*\\.?[0-9]*";

function fmtFloat(f) {
  return f.toFixed(3);
}

function Demo$ViewHeader(Props) {
  var entries = Props.entries;
  var totalQuantity$1 = totalQuantity(entries);
  var priceMean$1 = priceMean(entries, 3);
  var priceMeanMin = priceMean$1 / 0.8;
  var priceMeanMax = priceMean$1 / 0.7;
  return React.createElement("header", undefined, React.createElement("div", {
                  id: "header-top"
                }, React.createElement("h3", undefined, "Total Qté"), React.createElement("h3", undefined, "Prix Moy"), React.createElement("h3", undefined, "Moy Min"), React.createElement("h3", undefined, "Moy Max"), React.createElement("p", {
                      id: "total-quantity"
                    }, totalQuantity$1.toFixed(3)), React.createElement("p", {
                      id: "price-mean"
                    }, priceMean$1.toFixed(3)), React.createElement("p", {
                      id: "price-mean-min"
                    }, priceMeanMin.toFixed(3)), React.createElement("p", {
                      id: "price-mean-max"
                    }, priceMeanMax.toFixed(3))), React.createElement("div", {
                  id: "header-inputs"
                }, React.createElement("label", {
                      htmlFor: "new-entry:quantity"
                    }, React.createElement("p", undefined, "Qté .."), React.createElement("input", {
                          id: "new-entry:quantity",
                          pattern: floatPattern,
                          type: "text"
                        })), React.createElement("label", {
                      htmlFor: "new-entry:price"
                    }, React.createElement("p", undefined, "Prix ..."), React.createElement("input", {
                          id: "new-entry:price",
                          pattern: floatPattern,
                          type: "text"
                        })), React.createElement("div", undefined, React.createElement("input", {
                          defaultChecked: true,
                          id: "new-entry:shipping",
                          type: "checkbox"
                        }), React.createElement("label", {
                          className: "truck-icon",
                          htmlFor: "new-entry:shipping"
                        }, React.createElement("i", {
                              className: "fas fa-truck"
                            }))), React.createElement("button", undefined, React.createElement("i", {
                          className: "fas fa-plus"
                        }))));
}

var ViewHeader = {
  make: Demo$ViewHeader
};

function Demo$ViewFooter(Props) {
  var coefMin = Props.coefMin;
  var coefMax = Props.coefMax;
  var shipFactor = Props.shipFactor;
  return React.createElement("footer", undefined, React.createElement("label", undefined, React.createElement("p", undefined, "Coef min ..."), React.createElement("input", {
                      defaultValue: coefMin.toFixed(3),
                      id: "coefMin",
                      pattern: floatPattern,
                      type: "text"
                    })), React.createElement("label", {
                  htmlFor: "coefMax"
                }, React.createElement("p", undefined, "Coef max ..."), React.createElement("input", {
                      defaultValue: coefMax.toFixed(3),
                      id: "coefMax",
                      pattern: floatPattern,
                      type: "text"
                    })), React.createElement("label", {
                  htmlFor: "shipFactor"
                }, React.createElement("p", undefined, React.createElement("i", {
                          className: "fas fa-truck"
                        }), "(%)"), React.createElement("input", {
                      defaultValue: shipFactor.toFixed(3),
                      id: "shipFactor",
                      pattern: floatPattern,
                      type: "text"
                    })), React.createElement("button", {
                  type: "reset"
                }, React.createElement("i", {
                      className: "fas fa-redo"
                    })));
}

var ViewFooter = {
  make: Demo$ViewFooter
};

function Demo$ViewMain(Props) {
  return React.createElement("main", undefined, React.createElement("table", undefined, React.createElement("tbody", undefined, React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")), React.createElement("tr", undefined, React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1"), React.createElement("td", undefined, "H1")))));
}

var ViewMain = {
  make: Demo$ViewMain
};

function make(childs) {
  return React.createElement("div", {
              className: "foo"
            }, childs);
}

var app = [
  React.createElement(Demo$ViewHeader, {
        entries: entriesMock
      }),
  React.createElement(Demo$ViewMain, {}),
  React.createElement(Demo$ViewFooter, {
        coefMin: 0.8,
        coefMax: 0.7,
        shipFactor: 3
      })
];

var root = document.querySelector("#root");

if (!(root == null)) {
  ReactDom.render(app, root);
}

var defaultCoefMin = 0.8;

var defaultCoefMax = 0.7;

var defaultShipFactor = 3;

export {
  sum ,
  defaultCoefMin ,
  defaultCoefMax ,
  defaultShipFactor ,
  defauntNewEntry ,
  entriesMock ,
  computePrice ,
  totalQuantity ,
  priceMean ,
  floatPattern ,
  fmtFloat ,
  ViewHeader ,
  ViewFooter ,
  ViewMain ,
  make ,
  app ,
  
}
/* app Not a pure module */