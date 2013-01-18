package org.jpc.util.ui;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;

import org.jpc.engine.prolog.PrologEngineConfiguration;
import org.jpc.util.PrologEngineManager;

import com.google.common.collect.Multimap;
import com.google.common.collect.Multiset;

public class PrologConfigurationChooserPane extends HBox {

	final Multimap<String, PrologEngineConfiguration> groupedEngineConfigurations;
	final Multiset<String> prologEnginesMultiset;
	final SortedSet<String> prologEngines;
	
	final ComboBox prologEnginesComboBox;
	final ComboBox bridgeLibraryComboBox;
	
	public PrologConfigurationChooserPane() {
		this(new PrologEngineManager(PrologEngineManager.findConfigurations()).groupByPrologEngine());
	}
	
	public PrologConfigurationChooserPane(final Multimap<String, PrologEngineConfiguration> groupedEngineConfigurations) {
		this.groupedEngineConfigurations = groupedEngineConfigurations;
		prologEnginesMultiset = groupedEngineConfigurations.keys();
		prologEngines = new TreeSet<>(Arrays.asList(prologEnginesMultiset.toArray(new String[]{}))); //an ordered set (by default alphabetical order)
		
		prologEnginesComboBox = new ComboBox();
		bridgeLibraryComboBox = new ComboBox();
		
		prologEnginesComboBox.valueProperty().addListener(new ChangeListener<String>() {

			@Override
			public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
				if(!newValue.equals(oldValue)) {
					Set<String> bridgeLibraries = new HashSet<>();
					String selectedPrologEngine = (String) prologEnginesComboBox.getValue();
					Collection<PrologEngineConfiguration> prologEngineConfigurations = groupedEngineConfigurations.get(selectedPrologEngine);
					for(PrologEngineConfiguration prologEngineConfiguration : prologEngineConfigurations) {
						bridgeLibraries.add(prologEngineConfiguration.getLibraryName());
					}
					bridgeLibraryComboBox.getItems().clear();
					bridgeLibraryComboBox.getItems().addAll(bridgeLibraries);
					if(!bridgeLibraries.isEmpty()) {
						bridgeLibraryComboBox.setValue(bridgeLibraries.iterator().next());
					}	
				}
			}
		});
		
		prologEnginesComboBox.getItems().addAll(prologEngines);
		
		getChildren().add(new Label("Engine:"));
		getChildren().add(prologEnginesComboBox);
		getChildren().add(new Label("Library:"));
		getChildren().add(bridgeLibraryComboBox);
		
		if(!prologEngines.isEmpty())
			prologEnginesComboBox.setValue(prologEngines.iterator().next());
		
		//setPadding(new Insets(10, 10, 10, 10));
		setSpacing(10);
	}
	
	/**
	 * Verify that a logic engine can be instantiated, otherwise shows an error message
	 * @return true if a logic engine can be instantiated, false otherwise
	 */
	public boolean verifyPrologEngineSelection() {
		if(((String)prologEnginesComboBox.getValue()).trim().isEmpty() || ((String)bridgeLibraryComboBox.getValue()).trim().isEmpty()) {
			return false;
		} else
			return true;
	}
	
	public void disableEngineConfigurationOptions() {
		prologEnginesComboBox.setDisable(true);
		bridgeLibraryComboBox.setDisable(true);
	}
	
	public PrologEngineConfiguration getSelectedConfiguration() {
		String prologEngineName = (String)prologEnginesComboBox.getValue();
		Collection<PrologEngineConfiguration> configurations = groupedEngineConfigurations.get(prologEngineName);
		String libraryName = (String) bridgeLibraryComboBox.getValue();
		for(PrologEngineConfiguration config : configurations) {
			if(config.getLibraryName().equals(libraryName))
				return config;
		}
		return null;
	}
	
}
