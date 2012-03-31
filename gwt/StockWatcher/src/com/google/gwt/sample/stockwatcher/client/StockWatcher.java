package com.google.gwt.sample.stockwatcher.client;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyPressEvent;
import com.google.gwt.event.dom.client.KeyPressHandler;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.NumberFormat;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlexTable;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;

public class StockWatcher implements EntryPoint {
  private static final int REFRESH_INTERVAL = 5000; // in ms.
  private VerticalPanel mainPanel = new VerticalPanel();
  private FlexTable stocksFlexTable = new FlexTable();
  private HorizontalPanel addPanel = new HorizontalPanel();
  private TextBox newSymbolTextBox = new TextBox();
  private Button addStockButton = new Button("Add");
  private Label lastUpdatedLabel = new Label();
  private List<String> stocks = new ArrayList<String>();
  private StockPriceServiceAsync stockPriceSvc = GWT
      .create(StockPriceService.class);
  private Label errorMsgLabel = new Label();

  /**
   * Entry point method.
   */
  @Override
  public void onModuleLoad() {
    // Create table for stock data.
    this.stocksFlexTable.setText(0, 0, "Symbol");
    this.stocksFlexTable.setText(0, 1, "Price");
    this.stocksFlexTable.setText(0, 2, "Change");
    this.stocksFlexTable.setText(0, 3, "Remove");

    // Adding style to element in the stock list table.
    this.stocksFlexTable.setCellPadding(6);
    this.stocksFlexTable.getRowFormatter().addStyleName(0, "watchListHeader");
    this.stocksFlexTable.addStyleName("watchList");
    this.stocksFlexTable.getCellFormatter().addStyleName(0, 1,
        "watchListNumericColumn");
    this.stocksFlexTable.getCellFormatter().addStyleName(0, 2,
        "watchListNumericColumn");
    this.stocksFlexTable.getCellFormatter().addStyleName(0, 3,
        "watchListRemoveColumn");

    // Assemble Add Stock panel.
    this.addPanel.add(this.newSymbolTextBox);
    this.addPanel.add(this.addStockButton);
    this.addPanel.addStyleName("addPanel");

    // Assemble Main panel.
    this.errorMsgLabel.setStyleName("errorMessage");
    this.errorMsgLabel.setVisible(false);
    this.mainPanel.add(this.errorMsgLabel);
    this.mainPanel.add(this.stocksFlexTable);
    this.mainPanel.add(this.addPanel);
    this.mainPanel.add(this.lastUpdatedLabel);

    // Associate the Main panel with the HTML host page.
    RootPanel.get("stockList").add(this.mainPanel);

    // Move cursor focus to the input box.
    this.newSymbolTextBox.setFocus(true);

    // Setup timer to refresh list automatically
    Timer refreshTimer = new Timer() {
      @Override
      public void run() {
        refreshWatchList();
      }
    };
    refreshTimer.scheduleRepeating(REFRESH_INTERVAL);

    // Listen for mouse events on the Add button
    this.addStockButton.addClickHandler(new ClickHandler() {
      @Override
      public void onClick(ClickEvent event) {
        addStock();
      }
    });

    // Listen for keyboard events in the input box
    this.newSymbolTextBox.addKeyPressHandler(new KeyPressHandler() {
      @Override
      public void onKeyPress(KeyPressEvent event) {
        if (event.getCharCode() == KeyCodes.KEY_ENTER) {
          addStock();
        }
      }
    });
  }

  /**
   * Generate random stock prices.
   */
  protected void refreshWatchList() {
    // Initialize the service proxy
    if (this.stockPriceSvc == null) {
      this.stockPriceSvc = GWT.create(StockPriceService.class);
    }

    // Set up the callback object.
    AsyncCallback<StockPrice[]> callback = new AsyncCallback<StockPrice[]>() {

      @Override
      public void onSuccess(StockPrice[] prices) {
        updateTable(prices);
      }

      @Override
      public void onFailure(Throwable caught) {
        // If the stock code is in the list of delisted codes, display an error
        // message.
        String details = caught.getMessage();
        if(caught instanceof DelistedException) {
          details = "Company '" + ((DelistedException) caught).getSymbol()
              + "' was delisted";
        }

        errorMsgLabel.setText("Error: " + details);
        errorMsgLabel.setVisible(true);
      }
    };

    // Make the call to the stock price service.
    stockPriceSvc.getPrices(this.stocks.toArray(new String[0]), callback);
  }

  /**
   * Update the Price and Change fields all the rows in the stock table.
   * 
   * @param prices
   *          Stock data for all rows.
   */
  private void updateTable(StockPrice[] prices) {
    for (int i = 0; i < prices.length; i++) {
      updateTable(prices[i]);
    }
    
    // Display timestamp showing last refresh.
    this.lastUpdatedLabel.setText("Last update: "
        + DateTimeFormat.getMediumDateTimeFormat().format(new Date()));
    
    // Clear any errors.
    errorMsgLabel.setVisible(false);
  }

  /**
   * Update a single row in the stock table.
   * 
   * @param price
   *          Stock data for a single row.
   */
  private void updateTable(StockPrice stockPrice) {
    // Test stock is still in the stock table.
    if (!this.stocks.contains(stockPrice.getSymbol())) {
      return;
    }

    int row = this.stocks.indexOf(stockPrice.getSymbol()) + 1;

    // Format the data in the Price and change fileds.
    String priceText = NumberFormat.getFormat("#,##0.00").format(
        stockPrice.getPrice());
    NumberFormat changeFormat = NumberFormat.getFormat("+#,##0.00;-#,##0.00");
    String changeText = changeFormat.format(stockPrice.getChange());
    String changePercentText = changeFormat.format(stockPrice
        .getChangePercent());

    // Populate the Price and change fields with new data.
    this.stocksFlexTable.setText(row, 1, priceText);
    Label changedWidget = (Label) this.stocksFlexTable.getWidget(row, 2);
    changedWidget.setText(changeText + " (" + changePercentText + "%)");

    // Change the color of label
    String changeStyleName = "noChange";
    if (stockPrice.getChangePercent() < -.1) {
      changeStyleName = "negativeChange";
    } else if (stockPrice.getChangePercent() > .1) {
      changeStyleName = "positiveChange";
    }

    changedWidget.addStyleName(changeStyleName);
  }

  /**
   * Add stock to FlexTable. Executed when the user clicks the addStockButton or
   * presses enter in the newSymbolTextBox.
   */
  private void addStock() {
    final String symbol = this.newSymbolTextBox.getText().toUpperCase().trim();

    // Stock code must be between 1 and 10 chars that are numbers, letters,
    // or dots.
    if (!symbol.matches("^[0-9A-Z\\.]{1,10}$")) {
      Window.alert("'" + symbol + "' is not a valid symbol.");
      this.newSymbolTextBox.selectAll();
      return;
    }

    this.newSymbolTextBox.setText("");
    this.newSymbolTextBox.setFocus(true);

    // Don't add the stock if it's already in the table.
    if (this.stocks.contains(symbol)) {
      return;
    }

    // Add the stock to the table.
    int row = this.stocksFlexTable.getRowCount();
    this.stocks.add(symbol);
    this.stocksFlexTable.setText(row, 0, symbol);
    this.stocksFlexTable.setWidget(row, 2, new Label());

    // Set style
    this.stocksFlexTable.getCellFormatter().addStyleName(row, 1,
        "watchListNumericColumn");
    this.stocksFlexTable.getCellFormatter().addStyleName(row, 2,
        "watchListNumericColumn");
    this.stocksFlexTable.getCellFormatter().addStyleName(row, 3,
        "watchListRemoveColumn");

    // Add a button to remove this stock from the table.
    Button removeStockButton = new Button("x");
    removeStockButton.addClickHandler(new ClickHandler() {
      @Override
      public void onClick(ClickEvent event) {
        int removedIndex = stocks.indexOf(symbol);
        stocks.remove(removedIndex);
        stocksFlexTable.removeRow(removedIndex + 1);
      }
    });
    this.stocksFlexTable.setWidget(row, 3, removeStockButton);
    removeStockButton.addStyleDependentName("remove");

    // Get the stock price.
    refreshWatchList();
  }
}
