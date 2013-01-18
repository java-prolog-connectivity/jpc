package org.jpc.util.ui;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;

import org.jpc.util.concurrent.JpcExecutor;
import org.minitoolbox.exception.NotImplementedException;

/**
 * This class is incomplete and ugly
 * @author sergioc
 *
 */
public class QueryPane extends HBox {
	
	public final TextArea queryText;
	private final Button nextSolutionButton;
	public final Button allSolutionsButton;
	private final Button clearTextButton;
	
	public QueryPane() {
		setSpacing(10);
		getChildren().add(new Text("Query:"));
		queryText = new TextArea();
		VBox vBoxSecondColumn = new VBox();
		vBoxSecondColumn.getChildren().add(queryText);
		nextSolutionButton = new Button("Next");
		allSolutionsButton = new Button("All Solutions");
		clearTextButton = new Button("Clear");
		
		clearTextButton.setOnAction(new EventHandler<ActionEvent>() {
			@Override
			public void handle(ActionEvent arg0) {
				queryText.setText("");
			}
		});
		HBox hBoxButtons = new HBox();
		hBoxButtons.setSpacing(10);
		//hBoxButtons.getChildren().addAll(nextSolutionButton, allSolutionsButton, clearTextButton);
		hBoxButtons.getChildren().addAll(allSolutionsButton, clearTextButton);
		vBoxSecondColumn.getChildren().add(hBoxButtons);
		getChildren().add(vBoxSecondColumn);
	}
	
	public void query(String query, JpcExecutor jpcExecutor) {
		throw new NotImplementedException();
	}
	
	public void enable() {
		queryText.setDisable(false);
		nextSolutionButton.setDisable(false);
		clearTextButton.setDisable(false);
	}
	
	public void disable() {
		queryText.setDisable(true);
		nextSolutionButton.setDisable(true);
		clearTextButton.setDisable(true);
	}

}
