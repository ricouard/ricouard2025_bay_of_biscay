package scripts;

import java.util.*;
import java.io.*;

import org.nuiton.topia.TopiaContext;

import org.nuiton.math.matrix.MatrixIterator;
import org.nuiton.math.matrix.MatrixND;
import org.nuiton.math.matrix.*;

import fr.ifremer.isisfish.entities.*;
import fr.ifremer.isisfish.datastore.RegionStorage;
import fr.ifremer.isisfish.IsisFishDAOHelper;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import fr.ifremer.isisfish.ui.simulator.SimulatorContext;

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * remplir target factor (il existe une option pour mettre 1 partout)
 *
 * Auteur: RICOUARD Antoine, 2023 after PHAN Tuan Anh, 2022
 */
public class FillTargetFactor_EquationsFromFiles {

    private static Log log = LogFactory.getLog(FillTargetFactor_EquationsFromFiles.class);
    protected String myTFdirectory = "/home/aricouard/Documents/recherche/publi/ALR2025/scripts/ricouard2025_what_can_be_done_scripts/TargetFactors/mod_3a/";


	/** les objets pour manipuler objets isis */ 
	protected TopiaContext myTX;
	protected List<Metier> myListMetiers;
	protected List<Species> myListSpecies;
	protected TargetSpeciesDAO myTsDAO;

	 // public FillTargetFactor() {} // exception cause by reading csv file
	/** lancer le script dans isis */
	public static void main(String[] args) throws Exception {
		System.out.println("~~~ Start: " + new Date() + " ~~~" + System.lineSeparator());
		FillTargetFactor_EquationsFromFiles script = new FillTargetFactor_EquationsFromFiles();
		script.run();
		System.out.println("~~~  Done: " + new Date() + " ~~~");
	}
   
	/** vrai lancement du script car main() accepte pas ces fx */
	public void run() throws Exception {
        myTX = RegionStorage.getRegion("macco_mod3a_sim2018_250718").getStorage().beginTransaction();
        myListMetiers = IsisFishDAOHelper.getMetierDAO(myTX).findAll();
        myListSpecies = IsisFishDAOHelper.getSpeciesDAO(myTX).findAll();
        myTsDAO = IsisFishDAOHelper.getTargetSpeciesDAO(myTX);

        SimulatorContext.setDb(myTX);
    
		setTargetFactor();

		// save the changes
		myTX.commitTransaction();
		myTX.closeContext();
	}

	/** cœur de ce script */
	protected void setTargetFactor() throws Exception {
		System.out.println("remplir target factor pour tous engins + espèces");
        
        // MatrixND matrixTargetFactors = MatrixFactory.getInstance().create(myTFfile);
		for (Metier metier : myListMetiers) {
			 log.warn("  > metier: " + metier.getName());
			List<MetierSeasonInfo> msis = metier.getMetierSeasonInfo();
			for (MetierSeasonInfo msi : msis) {
				for (Species sp : myListSpecies) {
					 log.warn("    > species: " + sp.getName());
				   remplirTF(msi, metier, sp);
				}
			}
		}
		System.out.println(System.lineSeparator());
	}

	private void remplirTF(MetierSeasonInfo msi, Metier metier, Species sp) throws Exception {

		String fileName = myTFdirectory + sp.getName() + "/" + metier.getName() +".txt";
		System.out.println("      + file name: " + fileName);

        // BufferedReader buffer = (new BufferedReader(new FileReader(filePath)))
		// String TF = new FileReader(new File(fileName));
		
		Path path = Path.of(fileName);
		String TF = Files.readString(path);
		System.out.println("      + equation: " + TF);

		// check if there is any already equation
		TargetSpecies ts = msi.getSpeciesTargetSpecies(sp);
		if (ts != null) {
			System.out.println("      + modif");
			ts.getTargetFactorEquation().setContent(TF);
            ts.setSpecies(sp) ;
			ts.update();
            msi.addSpeciesTargetSpecies(ts);
            msi.update(); 
		} else { // if not registered yet then make it
			System.out.println("      + new");
			TargetSpecies new_ts = myTsDAO.create();
			new_ts.getTargetFactorEquation().setContent(TF);
			new_ts.setSpecies(sp) ;
			new_ts.update();
			msi.addSpeciesTargetSpecies(new_ts);
			msi.update();
		}
	}


}

